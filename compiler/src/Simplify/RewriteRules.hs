module Simplify.RewriteRules (rewrite, rewrite') where

import Data.Maybe (fromMaybe)
import Control.Monad (mapM)
import Data.Functor.Identity (runIdentity, Identity)

import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)

import qualified AST.Optimized as Opt
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Simplify.Utils hiding (mapNode)
import Simplify.SimpleRule (SimpleRule (..), simpleRules)

-- TOPLEVEL REWRITING

rewrite :: Opt.GlobalGraph -> Opt.GlobalGraph
rewrite g =
  g { Opt._g_nodes=nodes' }
  where nodes' = Map.map rewriteNode $ Opt._g_nodes g

rewrite' :: Opt.LocalGraph -> Opt.LocalGraph
rewrite' g =
  g { Opt._l_nodes=nodes' }
  where nodes' = Map.map rewriteNode $ Opt._l_nodes g


-- FOR TESTING


recomputeDeps :: Opt.Node -> Opt.Node
recomputeDeps (Opt.Define expr deps) =
  Opt.Define expr (MultiSet.toMap $ exprDeps expr)
recomputeDeps (Opt.DefineTailFunc argNames body deps) =
  Opt.DefineTailFunc argNames body (MultiSet.toMap $ exprDeps body)
recomputeDeps (Opt.PortIncoming decoder deps) =
  Opt.PortIncoming decoder (MultiSet.toMap $ exprDeps decoder)
recomputeDeps (Opt.PortOutgoing encoder deps) =
  Opt.PortOutgoing encoder (MultiSet.toMap $ exprDeps encoder)
recomputeDeps x = x


-- MONADS


newtype Edited a = Edited (a, Bool)

instance Applicative Edited where
  pure x = Edited (x, False)
  (<*>) (Edited (f, b)) (Edited (x, b')) =
    Edited (f x, b || b')

instance Functor Edited where
  fmap f (Edited (x, b)) = (Edited (f x, b))

instance Monad Edited where
  return x = Edited (x, False)
  (>>=) (Edited (x, b)) f = Edited (x', b || b')
    where (Edited (x', b')) = f x

liftEdit :: (a -> Maybe a) -> (a -> Edited a)
liftEdit f x =
  case f x of
    Just x' -> Edited (x', True)
    Nothing -> Edited (x, False)

mapExprM :: Monad m => (Opt.Expr -> m Opt.Expr) -> Opt.Expr -> m Opt.Expr
mapExprM f expr =
  let f' = mapExprM f in
  f =<< case expr of
    (Opt.List es) -> Opt.List <$> mapM f' es
    (Opt.Function argNames body) -> Opt.Function argNames <$> f' body
    (Opt.Call func args) -> do
      args <- mapM f' args
      func <- f' func
      return $ Opt.Call func args
    (Opt.TailCall n args) -> do
      es <- mapM f' $ map snd args
      return $ Opt.TailCall n (zip (map fst args) es)
    (Opt.If branches final) -> do
      branches <- mapM mapBoth branches
      final <- f' final
      return $ Opt.If branches final
        where mapBoth (x, y) = do
                x <- f' x
                y <- f' y
                return $ (x, y)
    (Opt.Let def expr) -> do
      def <- case def of
        Opt.Def n e -> Opt.Def n <$> f' e
        Opt.TailDef n ns e -> Opt.TailDef n ns <$> f' e
      expr <- f' expr
      return $ Opt.Let def expr
    (Opt.Destruct d e) -> Opt.Destruct d <$> f' e
    (Opt.Case n1 n2 d es) -> do
      es' <- mapM f' $ map snd es
      return $ Opt.Case n1 n2 d (zip (map fst es) es')
    (Opt.Access e n) -> do
      e <- f' e
      return $ Opt.Access e n
    _ -> return expr

mapExpr :: (Opt.Expr -> Opt.Expr) -> Opt.Expr -> Opt.Expr
mapExpr f = runIdentity . mapExprM (return . f)


-- REWRITE ENGINE


editUntilFixpoint :: (a -> Edited a) -> a -> Edited a
editUntilFixpoint f x =
  let e@(Edited (x', b)) = f x
  in if b then e >>= editUntilFixpoint f else Edited (x', False)

mapUntilFixpoint :: (Opt.Expr -> Maybe Opt.Expr) -> Opt.Expr -> Edited Opt.Expr
mapUntilFixpoint f = mapExprM $ editUntilFixpoint $ liftEdit f

updateDeps :: Opt.Expr -> Map.Map Opt.Global Int -> (Opt.Expr, Map.Map Opt.Global Int)
updateDeps expr deps = (expr', deps')
  where
    Edited (expr', b) = mapUntilFixpoint rewriteExpr expr
    deps' = if b then MultiSet.toMap $ exprDeps expr' else deps

-- TODO: Include Cycle
rewriteNode :: Opt.Node -> Opt.Node
rewriteNode (Opt.Define expr deps) =
  Opt.Define expr' deps'
  where (expr', deps') = updateDeps expr deps
rewriteNode (Opt.DefineTailFunc argNames body deps) =
  Opt.DefineTailFunc argNames body' deps'
  where (body', deps') = updateDeps body deps
rewriteNode (Opt.PortIncoming decoder deps) =
  Opt.PortIncoming decoder' deps'
  where (decoder', deps') = updateDeps decoder deps
rewriteNode (Opt.PortOutgoing encoder deps) =
  Opt.PortOutgoing encoder' deps'
  where (encoder', deps') = updateDeps encoder deps
rewriteNode x = x


-- REWRITE RULES


rewriteExpr :: Opt.Expr -> Maybe Opt.Expr
rewriteExpr (Opt.Let (Opt.Def var expr) body) =
  -- TODO: Add support for TailDef
  let numUses = countUses body MultiSet.! var
  in if numUses == 0 then Just body
  else if numUses == 1 || isSmall expr then
    Just $ subst var expr body
  else Nothing
rewriteExpr (Opt.Call func args) =
  case func of
    (Opt.Function argNames body) ->
      if null argNames || null args then Nothing
      else Just $ betaReduce argNames body args
    (Opt.VarGlobal funcName) ->
      foldl (\acc (SimpleRule funcName' rewrite) ->
                if funcName == funcName' then
                  case rewrite args of
                    Nothing -> acc
                    Just x -> Just x
                else acc)
         Nothing simpleRules
    _ -> Nothing
rewriteExpr (Opt.If branches final) =
  let (branches', maybeFinal) = shortcircuit branches
  in case branches' of
    [] -> Just $ (fromMaybe final maybeFinal)
    _ ->
      if length branches == length branches' then Nothing
      else Just $ Opt.If branches' (fromMaybe final maybeFinal)
rewriteExpr _ = Nothing

betaReduce :: [Name] -> Opt.Expr -> [Opt.Expr] -> Opt.Expr
betaReduce [] body [] = body
betaReduce [] body args =
  -- Should never happen in well-typed formula
  Opt.Call (Opt.Function [] body) args
betaReduce argNames body [] = Opt.Function argNames body
betaReduce (argName : argNames) body (arg : args) =
  Opt.Let (Opt.Def argName arg) (betaReduce argNames body args)

shortcircuit :: [(Opt.Expr, Opt.Expr)] -> ([(Opt.Expr, Opt.Expr)], Maybe Opt.Expr)
shortcircuit [] = ([], Nothing)
shortcircuit (((Opt.Bool True), e) : tl) = ([], Just e)
shortcircuit (((Opt.Bool False), e) : tl) = shortcircuit tl
shortcircuit (branch : tl) = (branch : branches, final)
  where (branches, final) = shortcircuit tl

subst :: Name -> Opt.Expr -> Opt.Expr -> Opt.Expr
subst var expr = mapExpr replaceVar
  where
    replaceVar :: Opt.Expr -> Opt.Expr
    replaceVar (Opt.VarLocal var') | var == var' = expr
    replaceVar e = e

isSmall :: Opt.Expr -> Bool
isSmall (Opt.Bool _) = True
isSmall (Opt.Chr _) = True
isSmall (Opt.Str _) = True
isSmall (Opt.Int _) = True
isSmall (Opt.Float _) = True
isSmall (Opt.VarLocal _) = True
isSmall (Opt.VarGlobal _) = True
isSmall (Opt.VarEnum _ _) = True
isSmall (Opt.VarBox _) = True
isSmall (Opt.VarCycle _ _) = True
isSmall (Opt.VarKernel _ _) = True
isSmall Opt.Unit = True
isSmall _ = False
