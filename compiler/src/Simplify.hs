module Simplify
  ( simplify
  )
  where

import qualified AST.Optimized as Opt
import Simplify.Inlining (inline)
import Simplify.RewriteRules (rewrite)

simplify :: Opt.GlobalGraph -> Opt.GlobalGraph
simplify = rewrite
