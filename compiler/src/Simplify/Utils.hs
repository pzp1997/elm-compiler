{-# LANGUAGE TupleSections, NamedFieldPuns #-}
module Simplify.Utils where

import AST.Display ()

import Control.Arrow (second)
import Data.Functor.Identity (runIdentity, Identity)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.MultiSet as MultiSet
import Data.MultiSet (MultiSet)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromMaybe)

import qualified AST.Optimized as Opt
import AST.Optimized
import Data.Name (Name)
import qualified Data.Name as Name
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import qualified Elm.ModuleName as ModuleName

{- MONADS -}

mapExprM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapExprM f expr =
  let f' = mapExprM f in
  f =<< case expr of
    (List es) -> List <$> mapM f' es
    (Function argNames body) -> Function argNames <$> f' body
    (Call func args) -> do
      args <- mapM f' args
      func <- f' func
      return $ Call func args
    (TailCall n args) -> do
      es <- mapM f' $ map snd args
      return $ TailCall n (zip (map fst args) es)
    (If branches final) -> do
      branches <- mapM mapBoth branches
      final <- f' final
      return $ If branches final
        where mapBoth (x, y) = do
                x <- f' x
                y <- f' y
                return $ (x, y)
    (Let def expr) -> do
      def <- case def of
        Def n e -> Def n <$> f' e
        TailDef n ns e -> TailDef n ns <$> f' e
      expr <- f' expr
      return $ Let def expr
    (Destruct d e) -> Destruct d <$> f' e
    (Case n1 n2 d es) -> do
      es' <- mapM f' $ map snd es
      d <- deciderHelper f' d
      return $ Case n1 n2 d (zip (map fst es) es')
    (Access e n) -> do
      e <- f' e
      return $ Access e n
    (Update expr updates) -> do
      expr <- f' expr
      updates <- mapFields f' updates
      return $ Update expr updates
    (Record fields) -> Record <$> mapFields f' fields
    (Tuple e1 e2 maybeExpr) -> do
      e1 <- f' e1
      e2 <- f' e2
      maybeExpr <- case maybeExpr of
                     Nothing -> return Nothing
                     Just e3 -> Just <$> f' e3
      return $ Tuple e1 e2 maybeExpr
    _ -> return expr

deciderHelper :: Monad m => (Expr -> m Expr) -> Decider Choice -> m (Decider Choice)
deciderHelper f (Leaf (Inline expr)) = (Leaf . Inline) <$> f expr
deciderHelper f l@(Leaf _) = return l
deciderHelper f (Chain { _success, _failure, _testChain }) = do
  _success <- deciderHelper f _success
  _failure <- deciderHelper f _failure
  return $ Chain { _success, _failure, _testChain }
deciderHelper f (FanOut { _path, _tests, _fallback }) = do
  _fallback <- deciderHelper f _fallback
  _tests <- mapM (\(t, d) -> (t, ) <$> deciderHelper f d) _tests
  return $ FanOut { _path, _tests, _fallback }

mapFields :: (Monad m, Ord k) => (a -> m a) -> Map k a -> m (Map k a)
mapFields f updates =
  Map.fromList <$> (mapM (\(k, a) -> (k, ) <$> f a) $ Map.assocs updates)

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f = runIdentity . mapExprM (return . f)

newtype Collect a b = Collect (b, [a])

instance Functor (Collect a) where
  fmap f (Collect (x, b)) = (Collect (f x, b))

instance Applicative (Collect a) where
  pure x = Collect (x, [])
  (<*>) (Collect (f, l)) (Collect (x, l')) =
    Collect (f x, l ++ l')

instance Monad (Collect a) where
  return x = Collect (x, [])
  (>>=) (Collect (x, l)) f = Collect (x', l ++ l')
    where (Collect (x', l')) = f x

foldExpr :: (Expr -> a) -> (a -> a -> a) -> a -> Expr -> a
foldExpr f combine base e = foldr ($) base final
  where collect expr = Collect (expr, [combine (f expr)])
        Collect (_, final) = mapExprM collect e


newtype Edited a = Edited (a, Bool)

instance Functor Edited where
  fmap f (Edited (x, b)) = (Edited (f x, b))

instance Applicative Edited where
  pure x = Edited (x, False)
  (<*>) (Edited (f, b)) (Edited (x, b')) =
    Edited (f x, b || b')

instance Monad Edited where
  return x = Edited (x, False)
  (>>=) (Edited (x, b)) f = Edited (x', b || b')
    where (Edited (x', b')) = f x

liftEdit :: (a -> Maybe a) -> (a -> Edited a)
liftEdit f x =
  case f x of
    Just x' -> Edited (x', True)
    Nothing -> Edited (x, False)

editUntilFixpoint :: Maybe Int -> (a -> Edited a) -> a -> (Edited a, Int)
editUntilFixpoint limit f x =
  let e@(Edited ((_, n), _)) = aux limit f x in ( fst <$> e, n )
  where
    aux limit _ x
      | Maybe.maybe False (\lim -> lim <= 0) limit = Edited ((x, 0), True)
    aux limit f x =
      let e@(Edited (x', b)) = f x in
      let limit' = (\n -> n - 1) <$> limit in
      fmap (second (+1)) $
        if b then e >>= aux limit' f
        else Edited ((x', 0), False)

-- editUntilFixpoint :: Maybe Int -> (a -> Edited a) -> a -> ( Edited a, Int )
-- editUntilFixpoint limit f x =
--   let e@(Edited ((_, n), _)) = aux 0 f x in
--   ( fst <$> e, n )
--   where
--     aux :: Int -> (a -> Edited a) -> a -> Edited ( a, Int )
--     aux n _ x
--       | Maybe.maybe False (\lim -> n >= lim) limit = Edited ((x, n), True)
--     aux n f x =
--       let e@(Edited (x', b)) = f x in
--       let n' = n + 1 in
--       if b then e >>= aux n' f else Edited ((x', n'), False)

fromEdit :: Edited a -> a
fromEdit (Edited (x, _)) = x

{- GETTERS -}

nodeExpr :: Opt.Node -> Maybe Opt.Expr
nodeExpr (Opt.Define e _) = Just e
nodeExpr (Opt.DefineTailFunc _ e _) = Just e
-- nodeExpr (Opt.Cycle _ _ defs _) = Just
-- nodeExpr (Opt.PortIncoming e _) = Just e
-- nodeExpr (Opt.PortOutgoing e _) = Just e
nodeExpr _ = Nothing

-- TODO: Should this include Ports?
defsUsedByNode :: Opt.Node -> MultiSet Opt.Global
defsUsedByNode (Opt.Define _ ds) = ds
defsUsedByNode (Opt.DefineTailFunc _ _ ds) = ds
defsUsedByNode (Opt.Cycle _ _ _ ds) = ds
defsUsedByNode (Opt.Kernel _ ds) = ds
defsUsedByNode (Opt.PortIncoming _ ds) = ds
defsUsedByNode (Opt.PortOutgoing _ ds) = ds
defsUsedByNode _ = MultiSet.empty

-- TODO: Missing some json functions for main? Rewrite with foldExpr and <>?
exprDeps :: Opt.Expr -> MultiSet Opt.Global
exprDeps (Opt.Bool b) =
  MultiSet.singleton (Opt.Global ModuleName.basics $ Name.fromChars $
                      if b then "True" else "False")
exprDeps (Opt.Chr _) = MultiSet.singleton (Opt.toKernelGlobal Name.utils)
exprDeps (Opt.VarGlobal g) = MultiSet.singleton g
exprDeps (Opt.VarEnum g _) = MultiSet.singleton g
exprDeps (Opt.VarBox g) =
  MultiSet.singleton (Opt.Global ModuleName.basics Name.identity) <>
  MultiSet.singleton g
exprDeps (Opt.VarKernel home _) =
  MultiSet.singleton (Opt.toKernelGlobal home)
exprDeps (Opt.List entries) =
  MultiSet.singleton (Opt.toKernelGlobal Name.list) <>
  (MultiSet.unions $ map exprDeps entries)
exprDeps (Opt.Function _ body) = exprDeps body
exprDeps (Opt.Call func args) =
  exprDeps func `MultiSet.union`
  (MultiSet.unions $ map exprDeps args)
exprDeps (Opt.TailCall _ args) =
  MultiSet.unions $ map (exprDeps . snd) args
exprDeps (Opt.If branches final) =
  exprDeps final `MultiSet.union`
  (MultiSet.unions $ map (\(cond, body) ->
                           exprDeps cond `MultiSet.union` exprDeps body
                        ) branches)
exprDeps (Opt.Let def body) =
  exprDeps body <>
  (case def of
     Opt.Def _ e -> exprDeps e
     Opt.TailDef _ _ e -> exprDeps e)
exprDeps (Opt.Destruct _ body) = exprDeps body
exprDeps (Opt.Case _ _ decider jumps) =
  deciderDeps decider <> (MultiSet.unions $ map (exprDeps . snd) jumps)
  where
    deciderDeps :: Opt.Decider Opt.Choice -> MultiSet.MultiSet Opt.Global
    deciderDeps (Opt.Leaf (Opt.Inline e)) = exprDeps e
    deciderDeps (Opt.Leaf (Opt.Jump _)) = MultiSet.empty
    deciderDeps (Opt.Chain { Opt._success=s , Opt._failure=f , Opt._testChain=_ }) =
      deciderDeps s <> deciderDeps f
    deciderDeps (Opt.FanOut { Opt._tests=t , Opt._fallback=f , Opt._path=_ }) =
      deciderDeps f <> (MultiSet.unions $ map (deciderDeps . snd) t)
exprDeps (Opt.Access record _) = exprDeps record
exprDeps (Opt.Update e record) =
  exprDeps e <>
  (MultiSet.unions $ map exprDeps $ Map.elems record)
exprDeps (Opt.Record record) =
  MultiSet.unions $ map exprDeps $ Map.elems record
exprDeps (Opt.Unit) =
  MultiSet.singleton (Opt.toKernelGlobal Name.utils)
exprDeps (Opt.Tuple e1 e2 me3) =
  MultiSet.singleton (Opt.toKernelGlobal Name.utils) <>
  exprDeps e1 <> exprDeps e2 <> maybe MultiSet.empty exprDeps me3
exprDeps _ = MultiSet.empty

countUses :: Opt.Expr -> MultiSet Name
countUses (Opt.VarLocal n) = MultiSet.singleton n
countUses (Opt.List exprs) = MultiSet.unions $ map countUses exprs
countUses (Opt.Function argNames body) = countUses body
countUses (Opt.Call func args) =
  countUses func <> (MultiSet.unions $ map countUses args)
countUses (Opt.TailCall n es) =
  MultiSet.unions $ map (countUses . snd) es
countUses (Opt.If branches final) =
  countUses final <>
  (MultiSet.unions $ map (countUses . fst) branches) <>
  (MultiSet.unions $ map (countUses . snd) branches)
countUses (Opt.Let (Opt.Def _ expr) body) = countUses expr <> countUses body
countUses (Opt.Let (Opt.TailDef _ _ expr) body) = countUses expr <> countUses body
countUses (Opt.Destruct (Opt.Destructor _ path) expr) =
  MultiSet.singleton (rootOfPath path) <> countUses expr
countUses (Opt.Case _ _ decider choices) =
  deciderUses decider <> (MultiSet.unions $ map (countUses . snd) choices)
  where
    deciderUses :: Opt.Decider Opt.Choice -> MultiSet.MultiSet Name
    deciderUses (Opt.Leaf (Opt.Inline e)) = countUses e
    deciderUses (Opt.Leaf (Opt.Jump _)) = MultiSet.empty
    deciderUses (Opt.Chain { Opt._success=s , Opt._failure=f , Opt._testChain=_ }) =
      deciderUses s <> deciderUses f
    deciderUses (Opt.FanOut { Opt._tests=t , Opt._fallback=f , Opt._path=_ }) =
      deciderUses f <> (MultiSet.unions $ map (deciderUses . snd) t)
countUses (Opt.Access e _) = countUses e
countUses (Opt.Update e record) =
  countUses e <> (MultiSet.unions $ map countUses $ Map.elems record)
countUses (Opt.Record record) =
  (MultiSet.unions $ map countUses $ Map.elems record)
countUses (Opt.Tuple e1 e2 me3) =
  countUses e1 <> countUses e2 <> fromMaybe MultiSet.empty (countUses <$> me3)
countUses _ = MultiSet.empty

rootOfPath :: Opt.Path -> Name
rootOfPath (Opt.Index _ path) = rootOfPath path
rootOfPath (Opt.Field _ path) = rootOfPath path
rootOfPath (Opt.Unbox path) = rootOfPath path
rootOfPath (Opt.Root var) = var

{- MAPPING -}

mapNode :: (Opt.Expr -> Opt.Expr) -> Opt.Node -> Opt.Node
mapNode f (Opt.Define e set) = Opt.Define (f e) set
mapNode f (Opt.DefineTailFunc names e set) = Opt.DefineTailFunc names (f e) set
mapNode f (Opt.Cycle names l defs set) =
  Opt.Cycle names l' defs set
  where l' = map (\(n, e) -> (n, f e)) l
mapNode f (Opt.PortIncoming e set) = Opt.PortIncoming (f e) set
mapNode f (Opt.PortOutgoing e set) = Opt.PortOutgoing (f e) set
mapNode f n = n

mapNodeDependencies :: (MultiSet Opt.Global -> MultiSet Opt.Global) -> Opt.Node -> Opt.Node
mapNodeDependencies f (Opt.Define e set) = Opt.Define e (f set)
mapNodeDependencies f (Opt.DefineTailFunc names e set) = Opt.DefineTailFunc names e (f set)
mapNodeDependencies f (Opt.Cycle names l defs set) = Opt.Cycle names l defs (f set)
mapNodeDependencies f (Opt.PortIncoming e set) = Opt.PortIncoming e (f set)
mapNodeDependencies f (Opt.PortOutgoing e set) = Opt.PortOutgoing e (f set)
mapNodeDependencies f n = n

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

getBoundVars :: Opt.Expr -> Set Name
getBoundVars = foldExpr aux (Set.union) Set.empty
  where
    aux :: Opt.Expr -> Set Name
    aux (Let (Def v _) _) = Set.singleton v
    aux (Let (TailDef func args _) _) = Set.singleton func <> Set.fromList args
    aux (Destruct (Destructor v _) _) = Set.singleton v
    aux (Function args _) = Set.fromList args
    aux _ = Set.empty

renameAllLocalVars :: Opt.Expr -> Opt.Expr
renameAllLocalVars expr = mapExpr aux expr
  where
    boundVars = getBoundVars expr
    rename :: Name -> Name
    rename name =
      if Set.member name boundVars then
        Name.fromChars ("$" <> Name.toChars name)
      else name
    aux :: Opt.Expr -> Opt.Expr
    aux (VarLocal v) = VarLocal $ rename v
    aux (Let (Def v e) body) = Let (Def (rename v) e) body
    aux (Let (TailDef func args e) body) =
     Let (TailDef (rename func) (map rename args) e) body
    aux (Destruct (Destructor v path) body) =
      Destruct (Destructor (rename v) (renameRoot path)) body
      where
        renameRoot (Index i p) = Index i $ renameRoot p
        renameRoot (Field n p) = Field n $ renameRoot p
        renameRoot (Unbox p) = Unbox $ renameRoot p
        renameRoot (Root var) = Root $ rename var
    aux (Case temp root branches jumps) =
      Case temp (rename root) branches jumps
    aux (Function args body) =
     Function (map rename args) body
    aux x = x

{- PRINTING -}

showSet :: Show a => Set.Set a -> String
showSet = ("{" ++) . (++ "}") . List.intercalate ", " . List.map show . Set.elems

showMap :: (Show k, Show v) => Map.Map k v -> String
showMap = ("{" ++) . (++ "}") . List.intercalate ", " . List.map (\(k, v) -> show k ++ ": " ++ show v) . Map.assocs
