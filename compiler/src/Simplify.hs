module Simplify
  ( mapNode
  , simplifyGraph
  )
where

import AST.Optimized

import qualified Data.Name as Name
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import qualified Control.Monad.State.Lazy as State

import Elm.ModuleName as ModuleName
import Elm.Package as Package

-- USAGE TRACKER

type UsageTracker = State.State ((MultiSet.MultiSet Global), (MultiSet.MultiSet Global))

addVar :: Global -> UsageTracker ()
addVar g = State.modify (\(add, rem) -> ((MultiSet.insert g add), rem))

addVars :: MultiSet.MultiSet Global -> UsageTracker ()
addVars gs = State.modify (\(add, rem) -> ((MultiSet.union gs add), rem))

remVar :: Global -> UsageTracker ()
remVar g = State.modify (\(add, rem) -> (add, (MultiSet.insert g rem)))

remVars :: MultiSet.MultiSet Global -> UsageTracker ()
remVars gs = State.modify (\(add, rem) -> (add, (MultiSet.union gs rem)))

flip :: UsageTracker ()
flip = State.modify (\(add, rem) -> (rem, add))

getUsages :: Expr -> MultiSet.MultiSet Global
getUsages (VarGlobal g) = MultiSet.singleton g
getUsages (VarEnum g _) = MultiSet.singleton g -- TODO: ?
getUsages (VarBox g) = MultiSet.singleton g -- TODO: ?
getUsages (List entries) = MultiSet.unions $ map getUsages entries
getUsages (Function _ body) = getUsages body
getUsages (Call func args) =
  getUsages func `MultiSet.union`
  (MultiSet.unions $ map getUsages args)
getUsages (TailCall _ args) =
  MultiSet.unions $ map (getUsages . snd) args
getUsages (If branches final) =
  getUsages final `MultiSet.union`
  (MultiSet.unions $ map (\(cond, body) ->
                           getUsages cond `MultiSet.union` getUsages body
                        ) branches)
getUsages (Let _ body) = getUsages body
getUsages (Destruct _ body) = getUsages body
getUsages (Case _ _ _ jumps) =
  MultiSet.unions $ map (getUsages . snd) jumps
getUsages (Access record _) = getUsages record
getUsages _ = MultiSet.empty
-- TODO: Missing some cases

-- MAP NODE

updateDeps ::
  MultiSet.MultiSet Global
  -> (MultiSet.MultiSet Global, MultiSet.MultiSet Global)
  -> MultiSet.MultiSet Global
-- updateDeps g (add, rem) = g
updateDeps g (add, rem) = MultiSet.difference (MultiSet.union g add) rem

mapNode :: (Expr -> UsageTracker Expr) -> Node -> Node
mapNode f (Define expr deps) =
  Define expr (updateDeps deps depsUpdate)
  where (expr, depsUpdate) = State.runState (f expr) (MultiSet.empty, MultiSet.empty)
mapNode f (DefineTailFunc argNames body deps) =
  DefineTailFunc argNames body (updateDeps deps depsUpdate)
  where (body, depsUpdate) = State.runState (f body) (MultiSet.empty, MultiSet.empty)
mapNode f (Cycle names values functions deps) =
  Cycle names values functions (updateDeps deps depsUpdate)
  where
    valuesM = (mapM (\(n, e) -> f e >>= \e -> return $ (n, e)) values)
    (values, depsUpdate) = State.runState valuesM (MultiSet.empty, MultiSet.empty)
mapNode f (PortIncoming decoder deps) =
  PortIncoming decoder (updateDeps deps depsUpdate)
  where (decoder, depsUpdate) = State.runState (f decoder) (MultiSet.empty, MultiSet.empty)
mapNode f (PortOutgoing encoder deps) =
  PortOutgoing encoder (updateDeps deps depsUpdate)
  where (encoder, depsUpdate) = State.runState (f encoder) (MultiSet.empty, MultiSet.empty)
mapNode _ n = n

-- SIMPLIFY

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

-- TODO: Mess with TailCall, Case, Access, Update, Record
simplify :: Expr -> UsageTracker Expr
simplify (Function l e) = do
  e <- simplify e
  return $ Function l e
simplify (Call e args) = do
  e <- simplify e
  args <- mapM simplify args
  case (e, args) of
    (f, [List l]) ->
      if isFxn Package.core "List" "reverse" f then
        remVar revFxn >> (return $ List (reverse l))
      else
        return $ (Call f [List l])
    (f, es) -> return $ Call f es
simplify (If branches final) = do
  branches <- mapM (\(cond, body) -> do
                       cond <- simplify cond
                       body <- simplify body
                       return $ (cond, body)
                       ) branches
  (branches, maybeFinal) <- shortcircuit branches
  case maybeFinal of
    Nothing -> do
      final <- simplify final
      return $ If branches final
    Just final -> do
      remVars $ getUsages final
      return $ If branches final
  where
    shortcircuit :: [(Expr, Expr)] -> UsageTracker ([(Expr, Expr)], Maybe Expr)
    shortcircuit [] = return ([], Nothing)
    shortcircuit (((Bool True), e) : tl) = do
      remVars $ MultiSet.unions
        (map (\(cond, body) ->
                getUsages cond `MultiSet.union` getUsages body
             ) tl)
      -- TODO: remVar Bool True (also do for false below)
      return ([], Just e)
    shortcircuit (((Bool False), e) : tl) = do
      remVars $ getUsages e
      shortcircuit tl
    shortcircuit (branch : tl) = do
      (branches, final) <- shortcircuit tl
      return $ (branch : branches, final)
simplify (Let def body) = do
  body <- simplify body
  return $ Let def body
simplify (Tuple e1 e2 m) = do
  e1 <- simplify e1
  e2 <- simplify e2
  -- TODO: Use monads
  case m of
    Nothing -> return $ Tuple e1 e2 Nothing
    Just m -> do
      m <- simplify m
      return $ Tuple e1 e2 (Just m)
simplify x = return x

simplifyGraph :: GlobalGraph -> GlobalGraph
simplifyGraph g =
  g { _g_nodes=nodes' }
  where nodes' = Map.map (mapNode simplify) $ _g_nodes g
