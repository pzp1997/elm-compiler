module Simplify.RewriteRules (rewrite) where

import qualified AST.Optimized as Opt

rewrite :: Opt.GlobalGraph -> Opt.GlobalGraph
rewrite = id
