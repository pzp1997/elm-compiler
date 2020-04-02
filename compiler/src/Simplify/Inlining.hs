module Simplify.Inlining (inline) where

import qualified AST.Optimized as Opt
import qualified Debug.Trace as Debug
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import qualified Data.Set as Set

import Simplify.Utils

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


inlineHelp :: Opt.Global -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (Set.Set Opt.Global) -> Opt.Global -> Opt.Node -> Opt.Node
inlineHelp name deps uses d@(Opt.Global _ dLocalName) node
  | (uses ! d) == Set.singleton name =
      case nodeExpr (deps ! d) of
        Just replacement -> mapNode (mapGlobalVarInExpr d replacement) node
        Nothing -> node
  | otherwise = node


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
