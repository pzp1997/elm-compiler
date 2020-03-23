module Optimize.Simplify
  ( simplify
  )
  where

import AST.Display ()

import qualified AST.Optimized as Opt
import Control.Arrow (second)
import qualified Debug.Trace as Debug
import qualified Data.Name as Name
import qualified Data.Map as Map
import Data.Map.Strict ((!))
import qualified Data.Set as Set
import qualified Data.List as List

showUses :: Set.Set Opt.Global -> String
showUses uses = "{" ++ List.intercalate ", " (List.map show (Set.elems uses)) ++ "}"

buildUses :: Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (Set.Set Opt.Global)
buildUses graph =
  let usesList = Map.map (const Set.empty) graph in
  Map.foldrWithKey (\caller node uses ->
    case node of
      Opt.Define _ callees ->
        -- Set.foldr (Map.adjust (Set.insert caller)) uses callees
        Set.foldr (Map.alter (Just . \mv ->
          case mv of
            Just v -> Set.insert caller v
            Nothing -> Set.singleton caller
        )) uses callees
      Opt.DefineTailFunc _ _ callees ->
        -- Set.foldr (Map.adjust (Set.insert caller)) uses callees
        Set.foldr (Map.alter (Just . \mv ->
          case mv of
            Just v -> Set.insert caller v
            Nothing -> Set.singleton caller
        )) uses callees
      Opt.Cycle _ _ _ callees ->
        -- Set.foldr (Map.adjust (Set.insert caller)) uses callees
        Set.foldr (Map.alter (Just . \mv ->
          case mv of
            Just v -> Set.insert caller v
            Nothing -> Set.singleton caller
        )) uses callees


        -- Set.foldr (\callee uses ->
        --   Map.alter (Just . \maybeCalleeUses ->
        --     case maybeCalleeUses of
        --       Just calleeUses -> Set.insert caller calleeUses
        --       Nothing -> Set.singleton caller
        --   ) callee uses
        -- ) uses callees
      _ -> uses -- TODO
  ) usesList graph

mapNode :: (Opt.Expr -> Opt.Expr) -> Opt.Node -> Opt.Node
mapNode f (Opt.Define e set) = Opt.Define (f e) set
mapNode f (Opt.DefineTailFunc names e set) = Opt.DefineTailFunc names (f e) set
mapNode f (Opt.Cycle names l defs set) =
  Opt.Cycle names l' defs set
  where l' = map (\(n, e) -> (n, f e)) l
mapNode f (Opt.PortIncoming e set) = Opt.PortIncoming (f e) set
mapNode f (Opt.PortOutgoing e set) = Opt.PortOutgoing (f e) set
mapNode f n = n


mapExprInDef :: (Opt.Expr -> Opt.Expr) -> Opt.Def -> Opt.Def
mapExprInDef f (Opt.Def name e) = Opt.Def name (f e)
mapExprInDef f (Opt.TailDef name names e) = Opt.TailDef name names (f e)

mapExprInChoice :: (Opt.Expr -> Opt.Expr) -> Opt.Choice -> Opt.Choice
mapExprInChoice f (Opt.Inline e) = Opt.Inline (f e)
mapExprInChoice _ (Opt.Jump i) = Opt.Jump i

instance Functor Opt.Decider where
  fmap f (Opt.Leaf a) = Opt.Leaf (f a)
  fmap f (Opt.Chain testChain success failure) = Opt.Chain testChain (fmap f success) (fmap f failure)
  fmap f (Opt.FanOut path tests fallback) = Opt.FanOut path (List.map (second (fmap f)) tests) (fmap f fallback)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

mapGlobalVarInExpr :: Opt.Global -> Opt.Expr -> Opt.Expr -> Opt.Expr
mapGlobalVarInExpr var replacement = go
  where
    go (Opt.VarGlobal v) | v == var = replacement
    go (Opt.List es) = Opt.List (List.map go es)
    -- TODO is it always good to inline within function bodies?
    go (Opt.Function args body) = Opt.Function args (go body)
    go (Opt.Call e es) = Opt.Call (go e) (List.map go es)
    go (Opt.TailCall name args) = Opt.TailCall name (List.map (second go) args)
    go (Opt.If cases defaultCase) = Opt.If (List.map (mapBoth go) cases) (go defaultCase)
    -- TODO here's where we might do local inlining?
    go (Opt.Let def e) = Opt.Let (mapExprInDef go def) (go e)
    go (Opt.Destruct destructor e) = Opt.Destruct destructor (go e)
    go (Opt.Case name1 name2 decider cases) =
      Opt.Case name1 name2 (fmap (mapExprInChoice go) decider) (List.map (second go) cases)
    go (Opt.Access e name) = Opt.Access (go e) name
    go (Opt.Update e fields) = Opt.Update (go e) (Map.map go fields)
    go (Opt.Record fields) = Opt.Record (Map.map go fields)
    go (Opt.Tuple e1 e2 e3) = Opt.Tuple (go e1) (go e2) (fmap go e3)
    go e = e

-- mapExpr :: (Expr -> Expr) -> Expr -> Expr

inlineHelp :: Opt.Global -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global (Set.Set Opt.Global) -> Opt.Global -> Opt.Node -> Opt.Node
inlineHelp name deps uses d@(Opt.Global _ dLocalName) node
  | (uses ! d) == Set.singleton name =
      case nodeExpr (deps ! d) of
        Just replacement -> mapNode (mapGlobalVarInExpr d replacement) node
        -- Just replacementExpr ->
          -- Debug.trace ("extracted replacement expr: " ++ show replacementExpr) $
          -- mapNode (\e ->
          --   Debug.trace ("attempting to inline in this expr: " ++ show e) $
          --   case e of
          --     Opt.VarGlobal v ->
          --       if v == d then
          --         replacementExpr
          --       else
          --         e

          --     Opt.VarLocal v ->
          --       if v == dLocalName then
          --         replacementExpr
          --       else
          --         e

          --     _ ->
          --       e
          -- ) node
        Nothing -> node
  | otherwise = node

nodeExpr :: Opt.Node -> Maybe Opt.Expr
nodeExpr (Opt.Define e _) = Just e
nodeExpr (Opt.DefineTailFunc _ e _) = Just e
-- nodeExpr (Opt.Cycle _ _ defs _) = Just
nodeExpr (Opt.PortIncoming e _) = Just e
nodeExpr (Opt.PortOutgoing e _) = Just e
nodeExpr _ = Nothing

nodeDeps :: Opt.Node -> Set.Set Opt.Global
nodeDeps (Opt.Define _ ds) = ds
nodeDeps (Opt.DefineTailFunc _ _ ds) = ds
nodeDeps (Opt.Cycle _ _ _ ds) = ds
nodeDeps _ = Set.empty

nodeNames :: Opt.Node -> [Name.Name]
nodeNames (Opt.DefineTailFunc names _ _) = names
nodeNames (Opt.Cycle names _ _ _) = names
nodeNames _ = []

simplify :: Opt.GlobalGraph -> Opt.GlobalGraph
simplify (Opt.GlobalGraph deps fields) =
  -- Debug.trace ("simplifying global graph") $
  Opt.GlobalGraph (Map.foldrWithKey aux Map.empty deps) fields
  where
    uses = buildUses deps
    -- log name us ds names =
    --   Debug.trace (showUses ds ++ " -> " ++ show name ++ " -> " ++ showUses us ++ " " ++ show names)
    aux name node graph =
      -- let us = uses ! name in
      let ds = nodeDeps node in
      let names = nodeNames node in
      let node' = Set.foldr (inlineHelp name deps uses) node ds in
      -- log name us ds name $
      Map.insert name node' graph

-- VarGlobal name

-- updateExprInExpr :: (Opt.Expr -> Opt.Expr) -> Opt.Expr -> Opt.Expr
-- updateExprInExpr f expr =
--   Bool _
--   Chr _
--   Str _
--   Int _
--   Float _

--   VarLocal Name
--   VarGlobal Global
--   VarEnum Global Index.ZeroBased
--   VarBox Global
--   VarCycle ModuleName.Canonical Name
--   VarDebug Name ModuleName.Canonical A.Region (Maybe Name)
--   VarKernel Name Name
--   List [Expr]
--   Function [Name] Expr
--   Call Expr [Expr]
--   TailCall Name [(Name, Expr)]
--   If [(Expr, Expr)] Expr
--   Let Def Expr
--   Destruct Destructor Expr
--   Case Name Name (Decider Choice) [(Int, Expr)]
--   Accessor Name
--   Access Expr Name
--   Update Expr (Map.Map Name Expr)
--   Record (Map.Map Name Expr)
--   Unit
--   Tuple Expr Expr (Maybe Expr)
--   Shader Shader.Source (Set.Set Name) (Set.Set Name)

-- simplifyNode :: Opt.Global -> Map.Map Opt.Global Opt.Node -> Map.Map Opt.Global Opt.Node
-- simplifyNode name graph =
--   let node = graph ! name in
--   case node of
--     Opt.Define e uses ->
--       Debug.trace (show name ++ show uses) $
--         Set.foldr simplifyNode graph uses

    -- DefineTailFunc [Name] Expr (Set.Set Global) -- TODO
    -- Cycle [Name] [(Name, Expr)] [Def] (Set.Set Global) -- TODO
    -- Ctor Index.ZeroBased Int
    -- Enum Index.ZeroBased
    -- Box
    -- Link Global
    -- Manager EffectsType
    -- Kernel [K.Chunk] (Set.Set Global)
    -- PortIncoming Expr (Set.Set Global)
    -- PortOutgoing Expr (Set.Set Global)
    -- _ -> graph
