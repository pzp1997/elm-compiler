module Simplify.RewriteRules (rewrite) where

import qualified AST.Optimized as Opt
import qualified Data.Name as Name
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import qualified Control.Monad.State.Strict as State

import Elm.ModuleName as ModuleName
import Elm.Package as Pkg

import Simplify.Utils hiding (mapNode)

rewrite :: Opt.GlobalGraph -> Opt.GlobalGraph
rewrite g =
  g { Opt._g_nodes=nodes' }
  where nodes' = Map.map (mapNode simplify) $ Opt._g_nodes g


-- USAGE TRACKER

type UsageTracker = State.State (MultiSet Opt.Global, MultiSet Opt.Global)

-- addVar :: Opt.Global -> UsageTracker ()
-- addVar g = State.modify (\(add, rem) -> ((MultiSet.insert g add), rem))

-- addVars :: MultiSet Opt.Global -> UsageTracker ()
-- addVars gs = State.modify (\(add, rem) -> ((MultiSet.union gs add), rem))

-- remVar :: Opt.Global -> UsageTracker ()
-- remVar g = State.modify (\(add, rem) -> (add, (MultiSet.insert g rem)))

-- remVars :: MultiSet Opt.Global -> UsageTracker ()
-- remVars gs = State.modify (\(add, rem) -> (add, (MultiSet.union gs rem)))

-- flip :: UsageTracker ()
-- flip = State.modify (\(add, rem) -> (rem, add))

-- getUsages :: Opt.Expr -> MultiSet Opt.Global
-- getUsages (Opt.VarGlobal g) = MultiSet.singleton g
-- getUsages (Opt.VarEnum g _) = MultiSet.singleton g -- TODO: ?
-- getUsages (Opt.VarBox g) = MultiSet.singleton g -- TODO: ?
-- getUsages (Opt.List entries) = MultiSet.unions $ map getUsages entries
-- getUsages (Opt.Function _ body) = getUsages body
-- getUsages (Opt.Call func args) =
--   getUsages func `MultiSet.union`
--   (MultiSet.unions $ map getUsages args)
-- getUsages (Opt.TailCall _ args) =
--   MultiSet.unions $ map (getUsages . snd) args
-- getUsages (Opt.If branches final) =
--   getUsages final `MultiSet.union`
--   (MultiSet.unions $ map (\(cond, body) ->
--                            getUsages cond `MultiSet.union` getUsages body
--                         ) branches)
-- getUsages (Opt.Let _ body) = getUsages body
-- getUsages (Opt.Destruct _ body) = getUsages body
-- getUsages (Opt.Case _ _ _ jumps) =
--   MultiSet.unions $ map (getUsages . snd) jumps
-- getUsages (Opt.Access record _) = getUsages record
-- getUsages _ = MultiSet.empty
-- -- TODO: Missing some cases

-- -- MAP NODE

-- updateDeps ::
--   MultiSet Opt.Global
--   -> (MultiSet Opt.Global, MultiSet Opt.Global)
--   -> MultiSet Opt.Global
-- -- updateDeps g (add, rem) = g
-- updateDeps g (add, rem) = MultiSet.difference (MultiSet.union g add) rem

updateDeps x _ = x

mapNode :: (Opt.Expr -> UsageTracker Opt.Expr) -> Opt.Node -> Opt.Node
mapNode f (Opt.Define expr deps) =
    Opt.Define expr (updateDeps deps depsUpdate)
    where depsUpdate = MultiSet.empty
--   Opt.Define expr (updateDeps deps depsUpdate)
--   where (expr, depsUpdate) = State.runState (f expr) (MultiSet.empty, MultiSet.empty)
-- mapNode f (Opt.DefineTailFunc argNames body deps) =
--   Opt.DefineTailFunc argNames body (updateDeps deps depsUpdate)
--   where (body, depsUpdate) = State.runState (f body) (MultiSet.empty, MultiSet.empty)
-- mapNode f (Opt.Cycle names values functions deps) =
--   Opt.Cycle names values functions (updateDeps deps depsUpdate)
--   where
--     valuesM = (mapM (\(n, e) -> f e >>= \e -> return $ (n, e)) values)
--     (values, depsUpdate) = State.runState valuesM (MultiSet.empty, MultiSet.empty)
-- mapNode f (Opt.PortIncoming decoder deps) =
--   Opt.PortIncoming decoder (updateDeps deps depsUpdate)
--   where (decoder, depsUpdate) = State.runState (f decoder) (MultiSet.empty, MultiSet.empty)
-- mapNode f (Opt.PortOutgoing encoder deps) =
--   Opt.PortOutgoing encoder (updateDeps deps depsUpdate)
--   where (encoder, depsUpdate) = State.runState (f encoder) (MultiSet.empty, MultiSet.empty)
mapNode _ n = n

-- -- SIMPLIFY

-- isFxn :: Name -> String -> String -> Opt.Expr -> Bool
-- isFxn pkg _module name (Opt.VarGlobal g') =
--   g == g'
--   where g = Opt.Global
--           (ModuleName.Canonical {
--               _package = pkg
--               , _module = (Name.fromChars _module)
--               })
--           (Name.fromChars name)
-- isFxn _ _ _ _ = False

-- revFxn =
--   Opt.Global
--    (ModuleName.Canonical {
--        _package = Pkg.core
--        , _module = (Name.fromChars "List")
--        })
--    (Name.fromChars "reverse")

simplify :: Opt.Expr -> UsageTracker Opt.Expr
simplify = return

-- TODO: Mess with TailCall, Case, Access, Update, Record
-- simplify :: Opt.Expr -> UsageTracker Opt.Expr
-- simplify (Opt.Function l e) = do
--   e <- simplify e
--   return $ Opt.Function l e
-- simplify (Opt.Call e args) = do
--   e <- simplify e
--   args <- mapM simplify args
--   case (e, args) of
--     (f, [Opt.List l]) ->
--       if isFxn Pkg.core "List" "reverse" f then
--         remVar revFxn >> (return $ Opt.List (reverse l))
--       else
--         return $ (Opt.Call f [Opt.List l])
--     (f, es) -> return $ Opt.Call f es
-- simplify (Opt.If branches final) = do
--   branches <- mapM (\(cond, body) -> do
--                        cond <- simplify cond
--                        body <- simplify body
--                        return $ (cond, body)
--                        ) branches
--   (branches, maybeFinal) <- shortcircuit branches
--   case maybeFinal of
--     Nothing -> do
--       final <- simplify final
--       return $ Opt.If branches final
--     Just final -> do
--       remVars $ getUsages final
--       return $ Opt.If branches final
--   where
--     shortcircuit :: [(Opt.Expr, Opt.Expr)] -> UsageTracker ([(Opt.Expr, Opt.Expr)], Maybe Opt.Expr)
--     shortcircuit [] = return ([], Nothing)
--     shortcircuit (((Opt.Bool True), e) : tl) = do
--       remVars $ MultiSet.unions
--         (map (\(cond, body) ->
--                 getUsages cond `MultiSet.union` getUsages body
--              ) tl)
--       -- TODO: remVar Bool True (also do for false below)
--       return ([], Just e)
--     shortcircuit (((Opt.Bool False), e) : tl) = do
--       remVars $ getUsages e
--       shortcircuit tl
--     shortcircuit (branch : tl) = do
--       (branches, final) <- shortcircuit tl
--       return $ (branch : branches, final)
-- simplify (Opt.Let def body) = do
--   body <- simplify body
--   return $ Opt.Let def body
-- simplify (Opt.Tuple e1 e2 m) = do
--   e1 <- simplify e1
--   e2 <- simplify e2
--   -- TODO: Use monads
--   case m of
--     Nothing -> return $ Opt.Tuple e1 e2 Nothing
--     Just m -> do
--       m <- simplify m
--       return $ Opt.Tuple e1 e2 (Just m)
-- simplify x = return x
