module Simplify
  ( simplify
  )
  where

import qualified AST.Optimized as Opt
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Data.MultiSet as MultiSet
import Simplify.Inlining (inline, invertUses)
import Simplify.RewriteRules (rewrite)
import Simplify.Utils (Edited(..), exprDeps)

buildUsesOf :: Map.Map ModuleName.Canonical Opt.Main -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (MultiSet.MultiSet Opt.Global)
buildUsesOf mains usesBy =
  Map.foldlWithKey'
    (\acc moduleName main ->
      case main of
        Opt.Static -> acc
        Opt.Dynamic _ decoder ->
          let additionalUsesOf = exprDeps decoder in
          MultiSet.foldlWithKey
            (\acc name numberOfUses ->
              Map.adjust
                (MultiSet.insertMany
                  (Opt.Global moduleName Name._main)
                  numberOfUses
                ) name acc
            ) acc additionalUsesOf
    ) (invertUses usesBy) mains

simplify :: Map.Map ModuleName.Canonical Opt.Main -> Opt.GlobalGraph -> Opt.GlobalGraph
simplify mains graph =
  applyN 1 simplifyStep graph
  where
    simplifyStep graph@(Opt.GlobalGraph usesBy _) =
      let usesOf = buildUsesOf mains usesBy in
      let Edited ((_, graph'), _) = inline usesOf graph in
      rewrite graph'

applyN :: Int -> (a -> a) -> a -> a
applyN n f x =
  if n <= 0 then x
  else aux n x
  where
    aux 1 x = f x
    aux n x = aux (n - 1) (f x)
