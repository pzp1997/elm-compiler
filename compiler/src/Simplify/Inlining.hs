module Simplify.Inlining (inline) where

import qualified AST.Optimized as Opt
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Simplify.Utils

-- deps :: Map Global Node ~ Map Global Expr + Map Global Dependencies
-- some Node contain Set Globals
inline :: Opt.GlobalGraph -> Opt.GlobalGraph
inline (Opt.GlobalGraph deps fields) =
  Opt.GlobalGraph (Map.foldrWithKey aux Map.empty deps) fields
  where
    uses = buildUses deps
    aux name node graph =
      let ds = nodeDeps node in
      let node' = Set.foldr (inlineHelp name deps uses) node ds in
      Map.insert name node' graph

inlineHelp :: Opt.Global -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (Set.Set Opt.Global) -> Opt.Global -> Opt.Node -> Opt.Node
inlineHelp name deps uses d node =
  case nodeExpr (deps ! d) of
    _ | isBasicsFunction d -> node
    Just replacement
      | isSimpleExpr replacement || uses ! d == Set.singleton name ->
        mapNode (mapGlobalVarInExpr d replacement) node
    _ -> node

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



basicsFunctions :: [String]
basicsFunctions =
  [ "not", "negate", "toFloat", "truncate"
  , "append", "apL", "apR"
  , "add", "sub", "mul", "fdiv", "idiv"
  , "eq", "neq", "lt", "gt", "le", "ge"
  , "or", "and", "xor"
  , "remainderBy"
  ]

isBasicsFunction :: Opt.Global -> Bool
isBasicsFunction global@(Opt.Global (ModuleName.Canonical (Pkg.Name author project) module_) name) =
  Utf8.toChars author == "elm"
    && Utf8.toChars project == "core"
    && Utf8.toChars module_ == "Basics"
    && List.elem (Utf8.toChars name) basicsFunctions

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
