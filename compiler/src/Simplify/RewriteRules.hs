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

-- TODO: Sometimes misses rewriting oppurtunities at inline site
attemptInline :: Name -> Expr -> Expr -> Maybe Expr
attemptInline var expr body =
  let numUses = countUses body MultiSet.! var
  in if uninlineable var body || isRecursive var expr then Nothing
  else if numUses == 0 then Just body
  else if numUses == 1 || isSmall expr then
    Just $ subst var expr body
  else Nothing

uninlineable :: Name -> Expr -> Bool
uninlineable var body =
  foldExpr aux (||) False body
  where
    aux :: Expr -> Bool
    aux (Destruct (Destructor _ path) _) | var == rootOfPath path = True
    aux (Case _ root _ _) | var == root = True
    aux (Let (Def v _) _) | var == v = True
                            -- error $ "inlining shadowed variable " ++ Name.toChars var
    aux _ = False

-- TODO: You know this is wrong
isRecursive :: Name -> Expr -> Bool
isRecursive var expr =
  foldExpr aux (||) False expr
  where
    aux :: Expr -> Bool
    aux (VarLocal var') = var == var'
    aux (Case _ root _ _) = var == root
    aux _ = False

subst :: Name -> Expr -> Expr -> Expr
subst var expr = mapExpr replaceVar
  where
    replaceVar :: Expr -> Expr
    replaceVar (VarLocal var') | var == var' = expr
    replaceVar (Case temp root branches jumps) | root == var =
      Case temp var branches jumps
    replaceVar e = e

betaReduce :: [Name] -> Expr -> [Expr] -> Maybe Expr
betaReduce [] body [] = Just $ body
betaReduce [] body args = Just $ Call body args
betaReduce argNames body [] = Just $ Function argNames body
betaReduce (argName : argNames) body (arg : args) =
  case arg of
    (VarLocal var) | var == argName ->
                     betaReduce argNames body args
    _ ->
      Let (Def argName arg) <$>
      betaReduce argNames body args

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
