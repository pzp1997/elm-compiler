module Simplify
  ( simplify
  )
  where

import Control.Monad ((<=<))

import qualified AST.Optimized as Opt
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Data.MultiSet as MultiSet

import Simplify.Inlining (inline, invertUses, buildUsesOf)
import Simplify.RewriteRules (rewrite)
import Simplify.Utils (Edited(..), exprDeps, editUntilFixpoint, fromEdit)

simplify :: Map.Map ModuleName.Canonical Opt.Main -> Opt.GlobalGraph -> Opt.GlobalGraph
simplify mains graph =
  let inlineWithMain g = snd <$> inline mains g
  in fromEdit $ editUntilFixpoint (rewrite <=< inlineWithMain) graph
