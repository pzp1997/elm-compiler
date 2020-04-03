{-# LANGUAGE StandaloneDeriving #-}

module AST.Display where

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Utils.Binop as Binop
import qualified AST.Utils.Shader as Shader

import qualified Reporting.Annotation as Annot
import qualified Data.Index as Index
import qualified Data.Utf8 as Utf8
import qualified Elm.Kernel as Kernel
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Package
import qualified Parse.Primitives as Primitives
import qualified Optimize.DecisionTree as DecisionTree

import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import qualified Data.String as String

-- Reporting
deriving instance Show Annot.Position
deriving instance Show Annot.Region
instance Show a => Show (Annot.Located a) where
  show (Annot.At _ t) = show t

-- Data
instance Show (Utf8.Utf8 a) where
  show b = Utf8.toChars b
instance Show Index.ZeroBased where
  show x = "INDEX ZEROBASED"

-- Elm
deriving instance Show Primitives.Snippet
instance Show Package.Name where
  show (Package.Name author project) = Utf8.toChars author ++ "/" ++ Utf8.toChars project
deriving instance Show Kernel.Chunk
instance Show ModuleName.Canonical where
  show (ModuleName.Canonical packageName moduleName) = show packageName ++ "." ++ show moduleName

-- Optimize
deriving instance Show DecisionTree.Path
deriving instance Show DecisionTree.Test

--AST.Utils
instance Show Shader.Source where
  show _ = "SHADER.SOURCE SHOW"
deriving instance Show Shader.Types
deriving instance Show Shader.Type
deriving instance Show Binop.Associativity
deriving instance Show Binop.Precedence

-- AST.Source
deriving instance Show Src.Alias
deriving instance Show Src.Comment
deriving instance Show Src.Def
deriving instance Show Src.Docs
deriving instance Show Src.Effects
deriving instance Show Src.Exposed
deriving instance Show Src.Exposing
deriving instance Show Src.Import
deriving instance Show Src.Infix
deriving instance Show Src.Manager
deriving instance Show Src.Port
deriving instance Show Src.Privacy
deriving instance Show Src.Union
deriving instance Show Src.Value
deriving instance Show Src.VarType

deriving instance Show Src.Expr_
deriving instance Show Src.Pattern_
deriving instance Show Src.Type_

-- AST.Canonical
deriving instance Show Can.Alias
deriving instance Show Can.AliasType
deriving instance Show Can.Annotation
deriving instance Show Can.Binop
deriving instance Show Can.CaseBranch
deriving instance Show Can.Ctor
deriving instance Show Can.CtorOpts
deriving instance Show Can.Decls
deriving instance Show Can.Def
deriving instance Show Can.Expr_
deriving instance Show Can.Effects
deriving instance Show Can.Export
deriving instance Show Can.Exports
deriving instance Show Can.FieldType
deriving instance Show Can.FieldUpdate
deriving instance Show Can.Manager
deriving instance Show Can.PatternCtorArg
deriving instance Show Can.Port
deriving instance Show Can.Pattern_
deriving instance Show Can.Type
deriving instance Show Can.Union

-- AST.Optimize
deriving instance Show a => Show (Opt.Decider a)
deriving instance Show Opt.Choice
deriving instance Show Opt.Def
deriving instance Show Opt.Destructor
deriving instance Show Opt.EffectsType
deriving instance Show Opt.Expr
instance Show Opt.Global where
  show (Opt.Global moduleName name) = show moduleName ++ "." ++ show name
deriving instance Show Opt.Main
deriving instance Show Opt.Node
deriving instance Show Opt.Path

deriving instance Show Src.Module
deriving instance Show Can.Module
instance Show Opt.LocalGraph where
  show (Opt.LocalGraph { Opt._l_main=m , Opt._l_nodes=n , Opt._l_fields=f }) =
    "\n" ++
    case m of
      Nothing -> "no main"
      Just m -> "yes main"
    ++ "\n\n" ++ (
    concat $
    map (\(k, a) -> show k ++ ": " ++ show a ++ "\n") $ Map.assocs f)
    ++ "\n\n\n\n" ++ (
    concat $
    map (\(k, a) -> show k ++ ": " ++ show a ++ "\n\n") $ Map.assocs n)

instance Show Opt.GlobalGraph where
  show (Opt.GlobalGraph { Opt._g_nodes=n , Opt._g_fields=f }) =
    (concat $
    map (\(k, a) -> show k ++ ": " ++ show a ++ "\n") $ Map.assocs f)
    ++ "\n\n\n\n" ++ (
    concat $
    map (\(k, a) -> show k ++ ": " ++ show a ++ "\n\n") $ Map.assocs n)

deriving instance Eq Opt.Choice
deriving instance Eq Opt.Def
deriving instance Eq Opt.Destructor
deriving instance Eq Opt.EffectsType
deriving instance Eq Opt.Path
deriving instance Eq Kernel.Chunk
instance Eq Shader.Source where
  (==) _ _ = True

deriving instance Eq Opt.Expr
deriving instance Eq Opt.Node

nodeDeps (Opt.Define _ deps) =  deps
nodeDeps (Opt.DefineTailFunc _ _ deps) =  deps
nodeDeps (Opt.Cycle _ _ _ deps) =  deps
nodeDeps (Opt.PortIncoming _ deps) =  deps
nodeDeps (Opt.PortOutgoing _ deps) =  deps
nodeDeps _ = Map.empty

-- diffGraphs :: Opt.GlobalGraph -> Opt.GlobalGraph -> Map.Map Opt.Node (Mu)
diffGraphs g1 g2 = concat $
  map (\(k, a) -> show k ++ ": " ++ show a ++ "\n\n") $ Map.assocs $
  Map.map (\(s1, s2) -> (s1, s2, Map.difference (nodeDeps s2) (nodeDeps s1),
                         Map.difference (nodeDeps s1) (nodeDeps s2))) $
  Map.differenceWith (\(s1,_) s2 -> if s1 == s2 then Nothing else Just (s1, s2))
  (Map.map (\x -> (x,Opt.Box)) $ Opt._g_nodes g1) (Opt._g_nodes g2)
