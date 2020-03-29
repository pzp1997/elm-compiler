module Optimize.Simplify
  ( simplify
  )
  where

import AST.Display ()

import qualified AST.Optimized as Opt
import Control.Arrow (second)
import qualified Debug.Trace as Debug
import qualified Data.Name as Name
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import qualified Data.Utf8 as Utf8

showUses :: Set.Set Opt.Global -> String
showUses uses = "{" ++ List.intercalate ", " (List.map show (Set.elems uses)) ++ "}"

buildUses :: Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (Set.Set Opt.Global)
buildUses graph =
  let usesList = Map.map (const Set.empty) graph in
  Map.foldrWithKey (\caller node uses ->
    let callees = nodeDeps node in
    Set.foldr (Map.alter (Just . \mv ->
      case mv of
        Just v -> Set.insert caller v
        Nothing -> Set.singleton caller
    )) uses callees
  ) usesList graph

mapNode :: (Opt.Expr -> Opt.Expr) -> Opt.Node -> Opt.Node
mapNode f (Opt.Define e set) = Opt.Define (f e) set
mapNode f (Opt.DefineTailFunc names e set) = Opt.DefineTailFunc names (f e) set
mapNode f (Opt.Cycle names l defs set) =
  Opt.Cycle names l' defs set
  where l' = map (\(n, e) -> (n, f e)) l
mapNode f (Opt.PortIncoming e set) = Opt.PortIncoming (f e) set
mapNode f (Opt.PortOutgoing e set) = Opt.PortOutgoing (f e) set
mapNode f n = n

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

basisFunctions :: [String]
basisFunctions =
  [ "not", "negate", "toFloat", "truncate"
  , "append", "apL", "apR"
  , "add", "sub", "mul", "fdiv", "idiv"
  , "eq", "neq", "lt", "gt", "le", "ge"
  , "or", "and", "xor"
  , "remainderBy"
  ]

isBasicsFunction :: Opt.Global -> Bool
isBasicsFunction global@(Opt.Global (ModuleName.Canonical (Pkg.Name author project) module_) name) =
  Debug.trace (show global) $
  Utf8.toChars author == "elm"
    && Utf8.toChars project == "core"
    && Utf8.toChars module_ == "Basics"
    && List.elem (Utf8.toChars name) basisFunctions

isSimpleExpr :: Opt.Expr -> Bool
isSimpleExpr (Opt.Bool _) = True
isSimpleExpr (Opt.Chr _) = True
isSimpleExpr (Opt.Str _) = True
isSimpleExpr (Opt.Int _) = True
isSimpleExpr (Opt.Float _) = True
isSimpleExpr (Opt.VarLocal name) = True
isSimpleExpr (Opt.VarGlobal name) = True
isSimpleExpr (Opt.VarEnum _ _) = True
isSimpleExpr (Opt.VarBox _) = True
isSimpleExpr (Opt.VarCycle _ _) = True
isSimpleExpr (Opt.VarDebug _ _ _ _) = True
isSimpleExpr (Opt.VarKernel _ _) = True
isSimpleExpr (Opt.List _) = False
-- TODO should we inline constant lists? Depends on how good our constant folding for lists is...
-- Same question applies to other types below
-- isSimpleExpr (Opt.List es) = List.all isSimpleExpr es
isSimpleExpr (Opt.Function _ _) = False
isSimpleExpr (Opt.Call _ _) = False
isSimpleExpr (Opt.TailCall _ _) = False
isSimpleExpr (Opt.If _ _) = False
isSimpleExpr (Opt.Let _ _) = False
isSimpleExpr (Opt.Destruct _ _) = False
isSimpleExpr (Opt.Case _ _ _ _) = False
isSimpleExpr (Opt.Accessor _) = True
isSimpleExpr (Opt.Access _ _) = False
isSimpleExpr (Opt.Update _ _) = False
isSimpleExpr (Opt.Record _) = False
isSimpleExpr Opt.Unit = True
isSimpleExpr (Opt.Tuple _ _ _) = False
-- isSimpleExpr (Opt.Tuple e1 e2 e3) = isSimpleExpr e1 && isSimpleExpr e2 && fmap isSimpleExpr e3
isSimpleExpr (Opt.Shader _ _ _) = False

inlineHelp :: Opt.Global -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (Set.Set Opt.Global) -> Opt.Global -> Opt.Node -> Opt.Node
inlineHelp name deps uses d node =
  case nodeExpr (deps ! d) of
    _ | isBasicsFunction d -> node
    Just replacement | isSimpleExpr replacement || uses ! d == Set.singleton name ->
          mapNode (mapGlobalVarInExpr d replacement) node
    _ -> node

nodeExpr :: Opt.Node -> Maybe Opt.Expr
nodeExpr (Opt.Define e _) = Just e
nodeExpr (Opt.DefineTailFunc _ e _) = Just e
-- nodeExpr (Opt.Cycle _ _ defs _) = Just
nodeExpr (Opt.PortIncoming e _) = Just e
nodeExpr (Opt.PortOutgoing e _) = Just e
nodeExpr _ = Nothing

nodeDeps :: Opt.Node -> Set.Set Opt.Global
nodeDeps (Opt.Define _ ds) = ds
nodeDeps (Opt.DefineTailFunc _ _ ds) = ds
nodeDeps (Opt.Cycle _ _ _ ds) = ds
nodeDeps _ = Set.empty

nodeNames :: Opt.Node -> [Name.Name]
nodeNames (Opt.DefineTailFunc names _ _) = names
nodeNames (Opt.Cycle names _ _ _) = names
nodeNames _ = []

simplify :: Opt.GlobalGraph -> Opt.GlobalGraph
simplify (Opt.GlobalGraph deps fields) =
  Opt.GlobalGraph (Map.foldrWithKey aux Map.empty deps) fields
  where
    uses = buildUses deps
    aux name node graph =
      let ds = nodeDeps node in
      let node' = Set.foldr (inlineHelp name deps uses) node ds in
      Map.insert name node' graph
