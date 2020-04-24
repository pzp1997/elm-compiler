
module Simplify.SimpleRule (SimpleRule(..), simpleRules) where

import AST.Optimized
import qualified Data.Name as Name
import Data.Name (Name)
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg

data SimpleRule = SimpleRule { func :: Global
                             , replace :: [Expr] -> Maybe Expr
                             }

global :: Pkg.Name -> String -> String -> Global
global pkg _module funcName =
  Global
  (ModuleName.Canonical pkg (Name.fromChars _module))
  (Name.fromChars funcName)

revFxn = global Pkg.core "List" "reverse"
andBop = global Pkg.core "Basics" "and"
mapFxn = global Pkg.core "List" "map"
composeFxn = global Pkg.core "Basics" "composeL"
apRFxn = global Pkg.core "Basics" "apR"
foldlFxn = global Pkg.core "List" "foldl"
foldrFxn = global Pkg.core "List" "foldr"
addFxn = global Pkg.core "Basics" "add"
subFxn = global Pkg.core "Basics" "sub"
mulFxn = global Pkg.core "Basics" "mul"
fdivFxn = global Pkg.core "Basics" "fdiv"
idivFxn = global Pkg.core "Basics" "idiv"
powFxn = global Pkg.core "Basics" "pow"

reverseLiteral :: SimpleRule
reverseLiteral = SimpleRule revFxn rewrite
  where
    rewrite [List l] = Just $ List (reverse l)
    rewrite _ = Nothing

applyAnd :: SimpleRule
applyAnd = SimpleRule andBop rewrite
  where
    rewrite [Bool b1, Bool b2] = Just $ Bool (b1 && b2)
    rewrite [Bool False, expr] = Just $ Bool False
    rewrite [expr, Bool False] = Just $ Bool False
    rewrite [Bool True, expr] = Just $ expr
    rewrite [expr, Bool True] = Just $ expr
    rewrite _ = Nothing
    
-- List.map (fun x -> to_string x) (fun (y -> y + 1) []) ==> rewrites to
-- List.map (fun y ->  (Call y body)
mapComposition :: SimpleRule
mapComposition = SimpleRule mapFxn rewrite 
  where
    rewrite [Function outerArgs outerBody, Call (VarGlobal mapFxn) [Function innerArgs innerBody, rest]] =
      let compose = Call (VarGlobal composeFxn) [Function outerArgs outerBody, Function innerArgs innerBody] in
      let mapper = Call (VarGlobal mapFxn) [compose, rest] in
      Just $ mapper
    rewrite _ = Nothing

-- l |> List.map f
-- List.map l
-- Rewrite simple maps
pipeMap :: SimpleRule
pipeMap = SimpleRule apRFxn rewrite
  where
    rewrite [l, Call (VarGlobal mapFxn) [f] ] =
      Just $ Call (VarGlobal mapFxn) [f, l]
    rewrite _ = Nothing

-- Constant addition
constantAdd = SimpleRule addFxn rewrite
  where
    rewrite [Int i, Int j] = Just $ Int (i + j)
    rewrite _ = Nothing

-- Constant subtraction
constantSub = SimpleRule subFxn rewrite
  where
    rewrite [Int i, Int j] = Just $ Int (i - j)
    rewrite _ = Nothing

-- Constant Multiplication
constantMult = SimpleRule mulFxn rewrite
  where
    rewrite [Int i, Int j] = Just $ Int (i * j)
    rewrite _ = Nothing

-- Constant Integer Division
constantIDiv = SimpleRule idivFxn rewrite
  where
    rewrite [Int i, Int j] = Just $ Int (i `div` j)
    rewrite _ = Nothing

simpleRules = [reverseLiteral, applyAnd, pipeMap, mapComposition, constantAdd, constantSub, constantMult, constantIDiv]
