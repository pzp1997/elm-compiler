module Simplify.Module
  ( simplify
  )
  where

import qualified AST.Canonical as Can
import qualified Simplify.Expression as Expr


simplify :: Can.Module -> Can.Module
simplify modul = modul
