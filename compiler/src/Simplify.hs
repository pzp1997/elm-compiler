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

isFxn :: Global -> Expr -> Bool
isFxn g (VarGlobal g') = g == g'
isFxn _ _ = False

revFxn =
  Global
   (ModuleName.Canonical {
       _package = Package.core
       , _module = (Name.fromChars "List")
       })
   (Name.fromChars "reverse")

simplify :: Expr -> Expr
simplify (Call f [List l]) =
  if isFxn revFxn f then List $ reverse l else (Call f [List l])
simplify x = x
