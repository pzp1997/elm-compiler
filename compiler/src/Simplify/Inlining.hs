module Simplify.Inlining (inline) where

import qualified Debug.Trace as Debug

import qualified AST.Optimized as Opt
import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import Data.Map.Strict ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Simplify.Utils

-- usesBy :: Map Global Node ~ Map Global Expr + Map Global (MultiSet Global)
-- some Node contain Set Globals
inline :: Opt.GlobalGraph -> Opt.GlobalGraph
inline (Opt.GlobalGraph graph fields) =
  -- Debug.trace (showMap graph) $
  let usesOf = invertUses graph in
  Debug.trace (showMap usesOf) $
  let (graph', _) = Map.foldrWithKey aux (Map.empty, usesOf) graph in
  Opt.GlobalGraph graph' fields
  where
    aux caller node (usesBy, usesOf) =
      let usesByNode = MultiSet.toSet (defsUsedByNode node) in
      let (usesBy', usesOf', node') = Set.foldr (inlineHelp caller) (usesBy, usesOf, node) usesByNode in
      (Map.insert caller node' usesBy', usesOf')

inlineHelp :: Opt.Global -> Opt.Global -> (Map.Map Opt.Global Opt.Node, Map.Map Opt.Global (MultiSet Opt.Global), Opt.Node) -> (Map.Map Opt.Global Opt.Node, Map.Map Opt.Global (MultiSet Opt.Global), Opt.Node)
inlineHelp caller callee acc@(usesBy, usesOf, callerNode) =
  if isBasicsFunction callee then acc
  else Maybe.fromMaybe acc $ do
    calleeNode <- Map.lookup callee usesBy
    replacement <- nodeExpr calleeNode
    usesOfCallee <- Map.lookup callee usesOf
    guard (isSimpleExpr replacement || usesOfCallee == MultiSet.singleton caller)
    let usedByCallee = defsUsedByNode calleeNode
    -- inline callee in caller node
    let callerNode' = mapNode (mapGlobalVarInExpr callee replacement) callerNode
    -- update dependencies of caller node
    let callerNode'' = mapNodeDependencies (MultiSet.union usedByCallee . Map.delete callee) callerNode'
    let usesBy' = Map.insert caller callerNode'' usesBy
    -- update dependents of callee node's dependencies
    let usesOf' = Map.foldlWithKey (\acc dep numberOfUses ->
                    Map.adjust (\usesOfDep ->
                        MultiSet.union (Map.singleton caller numberOfUses) $
                          Map.delete callee usesOfDep
                      ) dep acc
                  ) usesOf usedByCallee

    -- remove callee from maps

    let usesBy'' = Map.adjust (mapNodeDependencies (const MultiSet.empty)) callee usesBy'
    -- TODO we should probably be doing this instead but it's causing a problem with the artifacts
    -- let usesBy'' = Map.delete callee usesBy'
    let usesOf'' = Map.delete callee usesOf'

    return (usesBy'', usesOf'', callerNode'')

{- The global graph contains info of the form Map caller (Map callee Int). We
refer to this as a usesBy map since usesBy[node] gives you a map of the
definitions that node uses and the number of times it uses each one. For the
purposes of inlining, it is important to have this information organized as a
Map callee (Map caller Int). We refer to this as a usesOf map since usesOf[node]
gives you a map of the definitions that use that node and the number of times
it uses each one. Transforming a usesBy map into a usesOf map amounts to
flipping the edges in the GlobalGraph.
-}
invertUses :: Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (MultiSet Opt.Global)
invertUses usesBy =
  let usesOf = Map.map (const MultiSet.empty) usesBy in
  Map.foldrWithKey (\caller node usesOf ->
    let usesByCaller = defsUsedByNode node in
    Map.foldrWithKey (\callee numberOfUses usesOf ->
      Map.alter (Just . \maybeUsesOfCallee ->
        let usesOfCallee = Maybe.fromMaybe Map.empty maybeUsesOfCallee in
        Map.insert caller numberOfUses usesOfCallee
      ) callee usesOf
    ) usesOf usesByCaller
  ) usesOf usesBy

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
isSimpleExpr (Opt.Accessor _) = True -- TODO should we consider this simple?
isSimpleExpr (Opt.Access _ _) = False
isSimpleExpr (Opt.Update _ _) = False
isSimpleExpr (Opt.Record _) = False
isSimpleExpr Opt.Unit = True
isSimpleExpr (Opt.Tuple _ _ _) = False
-- isSimpleExpr (Opt.Tuple e1 e2 e3) = isSimpleExpr e1 && isSimpleExpr e2 && fmap isSimpleExpr e3
isSimpleExpr (Opt.Shader _ _ _) = False
