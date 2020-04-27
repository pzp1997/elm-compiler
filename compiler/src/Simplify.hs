module Simplify
  ( simplify
  )
  where

import Control.Monad ((<=<))

import qualified AST.Optimized as Opt
import Control.Arrow (first)
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified Data.MultiSet as MultiSet

import Simplify.Inlining (inline, invertUses, buildUsesOf)
import Simplify.RewriteRules (rewrite)
import Simplify.Utils (Edited(..), exprDeps, editUntilFixpoint, fromEdit)

simplify :: Maybe Int -> Map.Map ModuleName.Canonical Opt.Main -> Opt.GlobalGraph -> (Opt.GlobalGraph, Int)
simplify limit mains graph =
  let inlineWithMain g = snd <$> inline mains g in
  first fromEdit $ editUntilFixpoint limit (rewrite <=< inlineWithMain) graph
