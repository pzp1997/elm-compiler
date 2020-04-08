module Simplify.Utils where

import AST.Display ()

import Control.Arrow (second)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import qualified AST.Optimized as Opt
import Data.Name (Name)
import qualified Data.Name as Name
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import qualified Elm.ModuleName as ModuleName

{- GETTERS -}

nodeExpr :: Opt.Node -> Maybe Opt.Expr
nodeExpr (Opt.Define e _) = Just e
nodeExpr (Opt.DefineTailFunc _ e _) = Just e
-- nodeExpr (Opt.Cycle _ _ defs _) = Just
-- nodeExpr (Opt.PortIncoming e _) = Just e
-- nodeExpr (Opt.PortOutgoing e _) = Just e
nodeExpr _ = Nothing

-- TODO: Should this include Ports?
defsUsedByNode :: Opt.Node -> MultiSet Opt.Global
defsUsedByNode (Opt.Define _ ds) = ds
defsUsedByNode (Opt.DefineTailFunc _ _ ds) = ds
defsUsedByNode (Opt.Cycle _ _ _ ds) = ds
defsUsedByNode (Opt.Kernel _ ds) = ds
defsUsedByNode (Opt.PortIncoming _ ds) = ds
defsUsedByNode (Opt.PortOutgoing _ ds) = ds
defsUsedByNode _ = MultiSet.empty

-- TODO: Missing some json functions for main?
exprDeps :: Opt.Expr -> MultiSet Opt.Global
exprDeps (Opt.Bool b) =
  MultiSet.singleton (Opt.Global ModuleName.basics $ Name.fromChars $
                      if b then "True" else "False")
exprDeps (Opt.Chr _) = MultiSet.singleton (Opt.toKernelGlobal Name.utils)
exprDeps (Opt.VarGlobal g) = MultiSet.singleton g
exprDeps (Opt.VarEnum g _) = MultiSet.singleton g
exprDeps (Opt.VarBox g) =
  MultiSet.singleton (Opt.Global ModuleName.basics Name.identity) <>
  MultiSet.singleton g
exprDeps (Opt.VarKernel home _) =
  MultiSet.singleton (Opt.toKernelGlobal home)
exprDeps (Opt.List entries) =
  MultiSet.singleton (Opt.toKernelGlobal Name.list) <>
  (MultiSet.unions $ map exprDeps entries)
exprDeps (Opt.Function _ body) = exprDeps body
exprDeps (Opt.Call func args) =
  exprDeps func `MultiSet.union`
  (MultiSet.unions $ map exprDeps args)
exprDeps (Opt.TailCall _ args) =
  MultiSet.unions $ map (exprDeps . snd) args
exprDeps (Opt.If branches final) =
  exprDeps final `MultiSet.union`
  (MultiSet.unions $ map (\(cond, body) ->
                           exprDeps cond `MultiSet.union` exprDeps body
                        ) branches)
exprDeps (Opt.Let def body) =
  exprDeps body <>
  (case def of
     Opt.Def _ e -> exprDeps e
     Opt.TailDef _ _ e -> exprDeps e)
exprDeps (Opt.Destruct _ body) = exprDeps body
exprDeps (Opt.Case _ _ decider jumps) =
  deciderDeps decider <> (MultiSet.unions $ map (exprDeps . snd) jumps)
  where
    deciderDeps :: Opt.Decider Opt.Choice -> MultiSet.MultiSet Opt.Global
    deciderDeps (Opt.Leaf (Opt.Inline e)) = exprDeps e
    deciderDeps (Opt.Leaf (Opt.Jump _)) = MultiSet.empty
    deciderDeps (Opt.Chain { Opt._success=s , Opt._failure=f , Opt._testChain=_ }) =
      deciderDeps s <> deciderDeps f
    deciderDeps (Opt.FanOut { Opt._tests=t , Opt._fallback=f , Opt._path=_ }) =
      deciderDeps f <> (MultiSet.unions $ map (deciderDeps . snd) t)
exprDeps (Opt.Access record _) = exprDeps record
exprDeps (Opt.Update e record) =
  exprDeps e <>
  (MultiSet.unions $ map exprDeps $ Map.elems record)
exprDeps (Opt.Record record) =
  MultiSet.unions $ map exprDeps $ Map.elems record
exprDeps (Opt.Unit) =
  MultiSet.singleton (Opt.toKernelGlobal Name.utils)
exprDeps (Opt.Tuple e1 e2 me3) =
  MultiSet.singleton (Opt.toKernelGlobal Name.utils) <>
  exprDeps e1 <> exprDeps e2 <> maybe MultiSet.empty exprDeps me3
exprDeps _ = MultiSet.empty

countUses :: Opt.Expr -> MultiSet Name
countUses (Opt.VarLocal n) = MultiSet.singleton n
countUses (Opt.List exprs) = MultiSet.unions $ map countUses exprs
countUses (Opt.Function argNames body) = countUses body
countUses (Opt.Call func args) =
  countUses func <> (MultiSet.unions $ map countUses args)
countUses (Opt.TailCall n es) =
  MultiSet.unions $ map (countUses . snd) es
countUses (Opt.If branches final) =
  countUses final <>
  (MultiSet.unions $ map (countUses . fst) branches) <>
  (MultiSet.unions $ map (countUses . snd) branches)
countUses (Opt.Let (Opt.Def _ expr) body) = countUses expr <> countUses body
countUses (Opt.Let (Opt.TailDef _ _ expr) body) = countUses expr <> countUses body
countUses (Opt.Destruct (Opt.Destructor _ path) expr) =
  MultiSet.singleton (rootOfPath path) <> countUses expr
countUses (Opt.Case _ _ decider choices) =
  deciderUses decider <> (MultiSet.unions $ map (countUses . snd) choices)
  where
    deciderUses :: Opt.Decider Opt.Choice -> MultiSet.MultiSet Name
    deciderUses (Opt.Leaf (Opt.Inline e)) = countUses e
    deciderUses (Opt.Leaf (Opt.Jump _)) = MultiSet.empty
    deciderUses (Opt.Chain { Opt._success=s , Opt._failure=f , Opt._testChain=_ }) =
      deciderUses s <> deciderUses f
    deciderUses (Opt.FanOut { Opt._tests=t , Opt._fallback=f , Opt._path=_ }) =
      deciderUses f <> (MultiSet.unions $ map (deciderUses . snd) t)
countUses (Opt.Access e _) = countUses e
countUses (Opt.Update e record) =
  countUses e <> (MultiSet.unions $ map countUses $ Map.elems record)
countUses (Opt.Record record) =
  (MultiSet.unions $ map countUses $ Map.elems record)
countUses (Opt.Tuple e1 e2 me3) =
  countUses e1 <> countUses e2 <> fromMaybe MultiSet.empty (countUses <$> me3)
countUses _ = MultiSet.empty

rootOfPath :: Opt.Path -> Name
rootOfPath (Opt.Index _ path) = rootOfPath path
rootOfPath (Opt.Field _ path) = rootOfPath path
rootOfPath (Opt.Unbox path) = rootOfPath path
rootOfPath (Opt.Root var) = var

{- MAPPING -}

mapNode :: (Opt.Expr -> Opt.Expr) -> Opt.Node -> Opt.Node
mapNode f (Opt.Define e set) = Opt.Define (f e) set
mapNode f (Opt.DefineTailFunc names e set) = Opt.DefineTailFunc names (f e) set
mapNode f (Opt.Cycle names l defs set) =
  Opt.Cycle names l' defs set
  where l' = map (\(n, e) -> (n, f e)) l
mapNode f (Opt.PortIncoming e set) = Opt.PortIncoming (f e) set
mapNode f (Opt.PortOutgoing e set) = Opt.PortOutgoing (f e) set
mapNode f n = n

mapNodeDependencies :: (MultiSet Opt.Global -> MultiSet Opt.Global) -> Opt.Node -> Opt.Node
mapNodeDependencies f (Opt.Define e set) = Opt.Define e (f set)
mapNodeDependencies f (Opt.DefineTailFunc names e set) = Opt.DefineTailFunc names e (f set)
mapNodeDependencies f (Opt.Cycle names l defs set) = Opt.Cycle names l defs (f set)
mapNodeDependencies f (Opt.PortIncoming e set) = Opt.PortIncoming e (f set)
mapNodeDependencies f (Opt.PortOutgoing e set) = Opt.PortOutgoing e (f set)
mapNodeDependencies f n = n

mapExprInDef :: (Opt.Expr -> Opt.Expr) -> Opt.Def -> Opt.Def
mapExprInDef f (Opt.Def name e) = Opt.Def name (f e)
mapExprInDef f (Opt.TailDef name names e) = Opt.TailDef name names (f e)

mapExprInChoice :: (Opt.Expr -> Opt.Expr) -> Opt.Choice -> Opt.Choice
mapExprInChoice f (Opt.Inline e) = Opt.Inline (f e)
mapExprInChoice _ (Opt.Jump i) = Opt.Jump i

instance Functor Opt.Decider where
  fmap f (Opt.Leaf a) = Opt.Leaf (f a)
  fmap f (Opt.Chain testChain success failure) = Opt.Chain testChain (fmap f success) (fmap f failure)
  fmap f (Opt.FanOut path tests fallback) = Opt.FanOut path (List.map (second (fmap f)) tests) (fmap f fallback)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

mapGlobalVarInExpr :: Opt.Global -> Opt.Expr -> Opt.Expr -> Opt.Expr
mapGlobalVarInExpr var replacement = go
  where
    go (Opt.VarGlobal v) | v == var = replacement
    go (Opt.List es) = Opt.List (List.map go es)
    -- TODO is it always good to inline within function bodies?
    go (Opt.Function args body) = Opt.Function args (go body)
    go (Opt.Call e es) = Opt.Call (go e) (List.map go es)
    go (Opt.TailCall name args) = Opt.TailCall name (List.map (second go) args)
    go (Opt.If cases defaultCase) = Opt.If (List.map (mapBoth go) cases) (go defaultCase)
    -- TODO here's where we might do local inlining?
    go (Opt.Let def e) = Opt.Let (mapExprInDef go def) (go e)
    go (Opt.Destruct destructor e) = Opt.Destruct destructor (go e)
    go (Opt.Case name1 name2 decider cases) =
      Opt.Case name1 name2 (fmap (mapExprInChoice go) decider) (List.map (second go) cases)
    go (Opt.Access e name) = Opt.Access (go e) name
    go (Opt.Update e fields) = Opt.Update (go e) (Map.map go fields)
    go (Opt.Record fields) = Opt.Record (Map.map go fields)
    go (Opt.Tuple e1 e2 e3) = Opt.Tuple (go e1) (go e2) (fmap go e3)
    go e = e

{- PRINTING -}

showSet :: Show a => Set.Set a -> String
showSet = ("{" ++) . (++ "}") . List.intercalate ", " . List.map show . Set.elems

showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = ("{" ++) . (++ "}") . List.intercalate ", " . List.map (\(k, v) -> show k ++ ": " ++ show v) . Map.assocs
