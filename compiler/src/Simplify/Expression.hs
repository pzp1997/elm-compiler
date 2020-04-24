module Simplify.Expression
  ( simplify
  )
  where

import qualified Data.Map as Map

import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A

simplify :: Can.Expr -> Can.Expr
simplify (A.At region expr) =
  A.At region $ simplifyHelp expr

simplifyHelp :: Can.Expr_ -> Can.Expr_
simplifyHelp expr =
  case expr of
    -- VAR
    Can.VarLocal name -> expr -- TODO lookup in useMap
    Can.VarTopLevel home name -> expr
    Can.VarKernel home name -> expr
    Can.VarForeign home name annotation -> expr
    Can.VarCtor opts home name index annotation -> expr
    Can.VarDebug home name annotation -> expr
    Can.VarOperator op home name annotation -> expr

    -- LITERAL
    Can.Chr chr -> expr
    Can.Str str -> expr
    Can.Int int -> expr
    Can.Float float -> expr
    Can.Accessor field -> expr
    Can.Unit -> expr
    Can.Shader src types -> expr

    -- REST
    Can.List entries ->
      Can.List (simplify <$> entries)
    Can.Negate expr ->
      Can.Negate (simplify expr)
    Can.Binop op home name annotation left right ->
      Can.Binop op home name annotation (simplify left) (simplify right)
    Can.Lambda args body -> Can.Lambda args (simplify body) -- TODO maybe don't simplify inside functions
    Can.Call func args ->
      case func of
        _ -> Can.Call func (simplify <$> args) -- TODO do something with func

    -- TODO deal with these
    Can.If branches finally -> expr
    Can.Let def body -> expr
    Can.LetRec defs body -> expr
    Can.LetDestruct pattern expr_ body -> expr
    Can.Case expr_ branches -> expr

    Can.Access record name ->
      Can.Access (simplify record) name
    Can.Update name record updates -> expr -- TODO
    Can.Record fields ->
      Can.Record (Map.map simplify fields)
    Can.Tuple a b maybeC ->
      Can.Tuple (simplify a) (simplify b) (simplify <$> maybeC)
