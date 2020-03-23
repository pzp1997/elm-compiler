{-# LANGUAGE StandaloneDeriving #-}

module Simplify
  ( mapNode
  , simplify
  )
where

import AST.Optimized

import qualified Data.Name as Name
import Elm.ModuleName as ModuleName
import Elm.Package as Package

mapNode :: (Expr -> Expr) -> Node -> Node
mapNode f (Define e set) = Define (f e) set
mapNode f (DefineTailFunc names e set) = DefineTailFunc names (f e) set
mapNode f (Cycle names l defs set) =
  Cycle names l' defs set
  where l' = map (\(n, e) -> (n, f e)) l
mapNode f (PortIncoming e set) = PortIncoming (f e) set
mapNode f (PortOutgoing e set) = PortOutgoing (f e) set
mapNode f n = n

isFxn :: Name -> String -> String -> Expr -> Bool
isFxn pkg _module name (VarGlobal g') =
  g == g'
  where g = Global
          (ModuleName.Canonical {
              _package = pkg
              , _module = (Name.fromChars _module)
              })
          (Name.fromChars name)
isFxn _ _ _ _ = False

revFxn =
  Global
   (ModuleName.Canonical {
       _package = Package.core
       , _module = (Name.fromChars "List")
       })
   (Name.fromChars "reverse")

-- rewrite' :: Expr -> Maybe Expr
-- rewrite' (Call f [List l])
--   if isFxn Package.core "List" "reverse" f then
--     List $ reverse l
--   else (Call f [List l])
-- rewrite' x = x

-- rewrite :: Expr -> Expr
-- rewrite e = maybe e rewrite (rewrite' e)

-- applyOverExprs :: Expr -> Expr
-- applyOverExprs (List es) =
--   List (map rewrite es)
-- applyOverExprs (Function l e) = Function l (rewrite e)
-- applyOverExprs (Call e es)

-- TODO: Mess with TailCall, Case, Access, Update, Record
simplify :: Expr -> Expr
simplify (Function l e) = Function l (simplify e)
simplify (Call e es) =
  case (simplify e, map simplify es) of
    (f, [List l]) ->
      if isFxn Package.core "List" "reverse" f then
        List $ reverse l
      else (Call f [List l])
    (f, es) -> Call f es
simplify (If es e) =
  case (map (\(e1, e2) -> (simplify e1, simplify e2)) es, simplify e) of
    (es, e) -> If es e
simplify (Let d e) = Let d (simplify e)
simplify (Tuple e1 e2 m) = Tuple e1 e2 (simplify <$> m)
simplify x = x
