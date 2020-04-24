{-# LANGUAGE TupleSections, NamedFieldPuns #-}
module Simplify.RewriteRules (rewrite) where

import Data.Maybe (fromMaybe)
import Control.Monad (mapM)
import Data.Functor.Identity (runIdentity, Identity)

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)

import AST.Optimized
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Simplify.Utils hiding (mapNode)
import Simplify.SimpleRule (SimpleRule (..), simpleRules)

-- TOPLEVEL REWRITING

rewrite :: GlobalGraph -> Edited GlobalGraph
rewrite g = do
  nodes' <- mapFields rewriteNode $ _g_nodes g
  return $ g { _g_nodes=nodes' }

-- rewrite' :: LocalGraph -> LocalGraph
-- rewrite' g =
--   g { _l_nodes=nodes' }
--   where nodes' = Map.map rewriteNode $ _l_nodes g


recomputeDeps :: Node -> Node
recomputeDeps (Define expr deps) =
  Define expr (exprDeps expr)
recomputeDeps (DefineTailFunc argNames body deps) =
  DefineTailFunc argNames body (exprDeps body)
recomputeDeps (PortIncoming decoder deps) =
  PortIncoming decoder (exprDeps decoder)
recomputeDeps (PortOutgoing encoder deps) =
  PortOutgoing encoder (exprDeps encoder)
recomputeDeps x = x

mapExprM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapExprM f expr =
  let f' = mapExprM f in
  f =<< case expr of
    (List es) -> List <$> mapM f' es
    (Function argNames body) -> Function argNames <$> f' body
    (Call func args) -> do
      args <- mapM f' args
      func <- f' func
      return $ Call func args
    (TailCall n args) -> do
      es <- mapM f' $ map snd args
      return $ TailCall n (zip (map fst args) es)
    (If branches final) -> do
      branches <- mapM mapBoth branches
      final <- f' final
      return $ If branches final
        where mapBoth (x, y) = do
                x <- f' x
                y <- f' y
                return $ (x, y)
    (Let def expr) -> do
      def <- case def of
        Def n e -> Def n <$> f' e
        TailDef n ns e -> TailDef n ns <$> f' e
      expr <- f' expr
      return $ Let def expr
    (Destruct d e) -> Destruct d <$> f' e
    (Case n1 n2 d es) -> do
      es' <- mapM f' $ map snd es
      d <- deciderHelper f' d
      return $ Case n1 n2 d (zip (map fst es) es')
    (Access e n) -> do
      e <- f' e
      return $ Access e n
    (Update expr updates) -> do
      expr <- f' expr
      updates <- mapFields f' updates
      return $ Update expr updates
    (Record fields) -> Record <$> mapFields f' fields
    (Tuple e1 e2 maybeExpr) -> do
      e1 <- f' e1
      e2 <- f' e2
      maybeExpr <- case maybeExpr of
                     Nothing -> return Nothing
                     Just e3 -> Just <$> f' e3
      return $ Tuple e1 e2 maybeExpr
    _ -> return expr

deciderHelper :: Monad m => (Expr -> m Expr) -> Decider Choice -> m (Decider Choice)
deciderHelper f (Leaf (Inline expr)) = (Leaf . Inline) <$> f expr
deciderHelper f l@(Leaf _) = return l
deciderHelper f (Chain { _success, _failure, _testChain }) = do
  _success <- deciderHelper f _success
  _failure <- deciderHelper f _failure
  return $ Chain { _success, _failure, _testChain }
deciderHelper f (FanOut { _path, _tests, _fallback }) = do
  _fallback <- deciderHelper f _fallback
  _tests <- mapM (\(t, d) -> (t, ) <$> deciderHelper f d) _tests
  return $ FanOut { _path, _tests, _fallback }

mapFields :: (Monad m, Ord k) => (a -> m a) -> Map k a -> m (Map k a)
mapFields f updates =
  Map.fromList <$>
  (mapM (\(k, a) -> (k, ) <$> f a) $
  Map.assocs updates)

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f = runIdentity . mapExprM (return . f)

newtype Collect a b = Collect (b, [a])

instance Functor (Collect a) where
  fmap f (Collect (x, b)) = (Collect (f x, b))

instance Applicative (Collect a) where
  pure x = Collect (x, [])
  (<*>) (Collect (f, l)) (Collect (x, l')) =
    Collect (f x, l ++ l')

instance Monad (Collect a) where
  return x = Collect (x, [])
  (>>=) (Collect (x, l)) f = Collect (x', l ++ l')
    where (Collect (x', l')) = f x

foldExpr :: (Expr -> a) -> (a -> a -> a) -> a -> Expr -> a
foldExpr f combine base e = foldr ($) base final
  where collect expr = Collect (expr, [combine (f expr)])
        Collect (_, final) = mapExprM collect e

-- REWRITE ENGINE


mapUntilFixpoint :: (Expr -> Maybe Expr) -> Expr -> Edited Expr
mapUntilFixpoint f = mapExprM (fst . editUntilFixpoint Nothing (liftEdit f))

updateDeps :: Expr -> MultiSet Global -> Edited (Expr, MultiSet Global)
updateDeps expr deps =
    let Edited (expr', b) = mapUntilFixpoint rewriteExpr expr
    in Edited ((expr',
            if b then
              exprDeps expr'
              <> MultiSet.filter (\(Global (ModuleName.Canonical pkg _) _) -> pkg == Pkg.json) deps
            else deps), b)

-- TODO: Include Ports
rewriteNode :: Node -> Edited Node
rewriteNode (Define expr deps) = do
  (expr', deps') <- updateDeps expr deps
  return $ Define expr' deps'
rewriteNode (DefineTailFunc argNames body deps) = do
  (body', deps') <- updateDeps body deps
  return $ DefineTailFunc argNames body' deps'
rewriteNode (Cycle names es defs deps) =
  Edited (Cycle names es defs' deps', b)
  where
    Edited (defs', b) = mapM (mapDefM $ mapUntilFixpoint rewriteExpr) defs
    deps' =
      if b then
        MultiSet.unions $ map (exprDeps . exprOfDep) defs'
      else deps
    mapDefM :: Monad m => (Expr -> m Expr) -> Def -> m Def
    mapDefM f (Def name expr) = Def name <$> f expr
    mapDefM f (TailDef name names expr) = TailDef name names <$> f expr
rewriteNode x = return x

exprOfDep :: Def -> Expr
exprOfDep (Def _ expr) = expr
exprOfDep (TailDef _ _ expr) = expr


-- REWRITE RULES


rewriteExpr :: Expr -> Maybe Expr
rewriteExpr (Let (Def var expr) body) = attemptInline var expr body
rewriteExpr (Destruct (Destructor var _) body) =
  let numUses = countUses body MultiSet.! var
  in if not (uninlineable var body) && numUses == 0
  then Just body else Nothing
-- TODO: Add support for TailDef
rewriteExpr (Call func args) =
  case func of
    (Function argNames body) ->
      if null argNames || null args then Nothing
      else betaReduce argNames body args
    (VarGlobal funcName) ->
      foldl (\acc (SimpleRule funcName' rewrite) ->
                if funcName == funcName' then
                  case rewrite args of
                    Nothing -> acc
                    Just x -> Just x
                else acc)
         Nothing simpleRules
    _ -> Nothing
rewriteExpr (If branches final) =
  let (branches', maybeFinal) = shortcircuit branches
  in case branches' of
    [] -> Just $ (fromMaybe final maybeFinal)
    _ ->
      if length branches == length branches' then Nothing
      else Just $ If branches' (fromMaybe final maybeFinal)
rewriteExpr _ = Nothing

uninlineable :: Name -> Expr -> Bool
uninlineable var expr =
  foldExpr aux (||) False expr
  where
    aux :: Expr -> Bool
    aux (Destruct (Destructor _ path) _) | var == rootOfPath path = True
    aux (Case _ root _ _) | var == root = True
    aux (Let (Def v _) _) | var == v = True
    aux _ = False

-- TODO: Sometimes misses rewriting oppurtunities at inline site
attemptInline :: Name -> Expr -> Expr -> Maybe Expr
attemptInline var expr body =
  let numUses = countUses body MultiSet.! var
  in if uninlineable var body then Nothing
  else if numUses == 0 then Just body
  else if numUses == 1 || isSmall expr then
    Just $ subst var expr body
  else Nothing

subst :: Name -> Expr -> Expr -> Expr
subst var expr = mapExpr replaceVar
  where
    replaceVar :: Expr -> Expr
    replaceVar (VarLocal var') | var == var' = expr
    replaceVar e = e

-- This is fairly sensitive to changes since we need to avoid shadowing
-- TODO: Rewrite this to be readable
-- TODO: Try to break this; might still produce illegal shadowing
betaReduce :: [Name] -> Expr -> [Expr] -> Maybe Expr
betaReduce argNames body args =
  let (argNames', args', body') = aux argNames args in
    if null args && not (null args) then Just $ Call body' args'
    else if null argNames' then Just body'
    else if length argNames' == length argNames then Nothing
    else Just $ Call (Function argNames' body') args'
  where
    aux :: [Name] -> [Expr] -> ([Name], [Expr], Expr)
    aux [] [] = ([], [], body)
    aux [] args = ([], args, body)
    aux argNames [] = (argNames, [], body)
    aux (argName : argNames_t) (arg : args_t) =
      let (argNames', args', body') = aux argNames_t args_t
      in case (attemptInline argName arg body') of
        Nothing -> (argName : argNames', arg : args', body')
        Just body'' -> (argNames', args', body'')

shortcircuit :: [(Expr, Expr)] -> ([(Expr, Expr)], Maybe Expr)
shortcircuit [] = ([], Nothing)
shortcircuit (((Bool True), e) : tl) = ([], Just e)
shortcircuit (((Bool False), e) : tl) = shortcircuit tl
shortcircuit (branch : tl) = (branch : branches, final)
  where (branches, final) = shortcircuit tl

isSmall :: Expr -> Bool
isSmall (Bool _) = True
isSmall (Chr _) = True
isSmall (Str _) = True
isSmall (Int _) = True
isSmall (Float _) = True
isSmall (VarLocal _) = True
isSmall (VarGlobal _) = True
isSmall (VarEnum _ _) = True
isSmall (VarBox _) = True
isSmall (VarCycle _ _) = True
isSmall (VarKernel _ _) = True
isSmall Unit = True
isSmall _ = False
