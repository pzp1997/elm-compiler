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
import Simplify.Utils (exprDeps)

simplify :: Map.Map ModuleName.Canonical Opt.Main -> Opt.GlobalGraph -> Opt.GlobalGraph
simplify mains graph@(Opt.GlobalGraph usesBy _) =
  let usesOf = invertUses usesBy in
  let usesOf' = Map.foldlWithKey'
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
                  ) usesOf mains
  in
  inline usesOf' graph
