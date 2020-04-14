module Simplify.Inlining (inline, invertUses) where

import qualified Debug.Trace as Debug

import qualified AST.Optimized as Opt
import Control.Monad (guard)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import Data.Map.Strict ((!))
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

import Simplify.Utils

-- usesBy :: Map Global Node ~ Map Global Expr + Map Global (MultiSet Global)
-- some Node contain Set Globals
inline :: Map.Map Opt.Global (MultiSet Opt.Global) -> Opt.GlobalGraph -> Opt.GlobalGraph
inline usesOf (Opt.GlobalGraph graph fields) =
  let (graph', _) = Map.foldlWithKey aux (Map.empty, usesOf) graph in
  Opt.GlobalGraph graph' fields
  where
    aux (usesBy, usesOf) caller node =
      let usesByNode = MultiSet.toSet (defsUsedByNode node) in
      let (usesBy', usesOf', node') = Set.foldr (inlineHelp caller) (usesBy, usesOf, node) usesByNode in
      (Map.insert caller node' usesBy', usesOf')

inlineHelp :: Opt.Global -> Opt.Global -> (Map.Map Opt.Global Opt.Node, Map.Map Opt.Global (MultiSet Opt.Global), Opt.Node) -> (Map.Map Opt.Global Opt.Node, Map.Map Opt.Global (MultiSet Opt.Global), Opt.Node)
inlineHelp caller callee@(Opt.Global calleeModuleName _) acc@(usesBy, usesOf, callerNode) =
  if isBasicsFunction callee then
    -- Basics functions already get optimized during code generation
    acc
  else Maybe.fromMaybe acc $ do
    { calleeNode <- Map.lookup callee usesBy
    ; replacement <- nodeExpr calleeNode
    ; usesOfCallee <- Map.lookup callee usesOf
    ; -- anyone who is using me should use my definition instead (since it is simple)
      let calleeIsSimple = isSimpleExpr replacement
    ; -- if the thing I am using (i.e. the callee) is only used in my body
      -- (i.e. the caller), substitute its definition in my body
      let calleeOnlyUsedOnce = usesOfCallee == MultiSet.singleton caller
    ; let fromJsonDecodeModule = calleeModuleName == ModuleName.jsonDecode
    ; guard ((calleeIsSimple || calleeOnlyUsedOnce) && not fromJsonDecodeModule)
    ; let usedByCallee = defsUsedByNode calleeNode
    ; -- inline callee in caller node
      let callerNode' = mapNode (mapGlobalVarInExpr callee replacement) callerNode
    ; -- update dependencies of caller node
      let callerNode'' = mapNodeDependencies
                          (MultiSet.union usedByCallee . MultiSet.removeAll callee)
                          callerNode'
    ; let usesBy' = Map.insert caller callerNode'' usesBy
    ; -- update dependents of callee node's dependencies
      let usesOf' = MultiSet.foldlWithKey
                      (\acc dep numberOfUses ->
                        Map.adjust
                          (MultiSet.insertMany caller numberOfUses
                            . MultiSet.removeMany callee numberOfUses)
                          dep acc
                      ) usesOf usedByCallee
    ; if calleeIsSimple then
        Just (usesBy', usesOf', callerNode'')
      else if calleeOnlyUsedOnce then
        -- remove callee from maps
        let usesBy'' = Map.adjust (mapNodeDependencies (const MultiSet.empty)) callee usesBy' in
        -- TODO we should probably be doing this instead but it's causing a problem with the artifacts
        -- let usesBy'' = Map.delete callee usesBy'
        let usesOf'' = Map.delete callee usesOf' in
        Just (usesBy'', usesOf'', callerNode'')
      else Nothing
    }

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
    MultiSet.foldrWithKey (\callee numberOfUses usesOf ->
      Map.alter (Just . \maybeUsesOfCallee ->
        let usesOfCallee = Maybe.fromMaybe MultiSet.empty maybeUsesOfCallee in
        MultiSet.insertMany caller numberOfUses usesOfCallee
      ) callee usesOf
    ) usesOf usesByCaller
  ) usesOf usesBy

basicsFunctions :: [Name.Name]
basicsFunctions = Name.fromChars <$>
  [ "not", "negate", "toFloat", "truncate"
  , "append", "apL", "apR"
  , "add", "sub", "mul", "fdiv", "idiv"
  , "eq", "neq", "lt", "gt", "le", "ge"
  , "or", "and", "xor"
  , "remainderBy"
  ]

isBasicsFunction :: Opt.Global -> Bool
isBasicsFunction global@(Opt.Global moduleName name) =
  moduleName == ModuleName.basics && List.elem name basicsFunctions

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
