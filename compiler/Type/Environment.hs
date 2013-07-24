module Type.Environment where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.Traversable as Traverse
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.UnionFind.IO as UF

import qualified SourceSyntax.Type as Src
import Type.Type

data Environment = Environment {
  constructor :: Map.Map String (IO (Int, [Variable], [Type], Type)),
  types :: Map.Map String Type,
  value :: Map.Map String Type
}

initialEnvironment :: [(String, [String], [(String,[Src.Type])])] -> IO Environment
initialEnvironment datatypes = do
    types <- makeTypes datatypes

    return $ Environment {
      constructor = makeConstructors types,
      types = types,
      value = Map.empty
    }

makeTypes :: [(String, [String], [(String, [Src.Type])])] -> IO (Map.Map String Type)
makeTypes datatypes = 
    Map.fromList <$> mapM makeCtor (builtins ++ map nameAndKind datatypes)
  where
    nameAndKind (name, tvars, _) = (name, length tvars)

    makeCtor (name, kind) = do
      ctor <- VarN <$> namedVar Constant name
      return (name, ctor)

    tuple n = ("_Tuple" ++ show n, n)

    kind n names = map (\name -> (name, n)) names

    builtins :: [(String,Int)]
    builtins = concat [ map tuple [0..9]
                      , kind 1 ["_List","Maybe","Signal"]
                      , kind 0 ["Int","Float","Char","Bool","Element"]
                      ]


makeConstructors :: Map.Map String Type -> Map.Map String (IO (Int, [Variable], [Type], Type))
makeConstructors types = Map.fromList builtins
  where
    list  t = (types ! "_List") <| t
    maybe t = (types ! "Maybe") <| t
    bool = types ! "Bool"
    int = types ! "Int"
    float = types ! "Float"

    inst :: Int -> Int -> ([Type] -> ([Type], Type)) -> IO (Int, [Variable], [Type], Type)
    inst kind numTVars tipe = do
      vars <- forM [1..numTVars] $ \_ -> flexibleVar
      let (args, result) = tipe (map VarN vars)
      return (kind, vars, args, result)

    nmbr :: Int -> (Type -> ([Type], Type)) -> IO (Int, [Variable], [Type], Type)
    nmbr kind tipe = do
      var <- number
      let (args, result) = tipe (VarN var)
      return (kind, [var], args, result)

    tupleCtor n =
        let name = "_Tuple" ++ show n
        in  (name, inst n n $ \vs -> (vs, foldl (<|) (types ! name) vs))
    
    builtins :: [ (String, IO (Int, [Variable], [Type], Type)) ]
    builtins = [ ("Nothing", inst 0 1 $ \ [t] -> ([], maybe t))
               , ("Just"   , inst 1 1 $ \ [t] -> ([t], maybe t))
               , ("[]"     , inst 0 1 $ \ [t] -> ([], list t))
               , ("::"     , inst 2 1 $ \ [t] -> ([t, list t], list t))
               , ("div"    , inst 2 0 $ \ [] -> ([int, int], int))
               , ("/"      , inst 2 0 $ \ [] -> ([float, float], float))
               , ("+"      , nmbr 2 $ \t -> ([t, t], t))
               , ("-"      , nmbr 2 $ \t -> ([t, t], t))
               , ("*"      , nmbr 2 $ \t -> ([t, t], t))
               , ("otherwise", inst 0 0 $ \ [] -> ([], bool))
               ] ++ map tupleCtor [0..9]


get :: Environment -> (Environment -> Map.Map String a) -> String -> a
get env subDict key = Map.findWithDefault err key (subDict env)
  where
    err = error $ "Could not find '" ++ key ++ "' in the type environment."


freshDataScheme :: Environment -> String -> IO (Int, [Variable], [Type], Type)
freshDataScheme env name = get env constructor name


instantiateType :: Environment -> Src.Type -> IO Type
instantiateType env sourceType =
    snd <$> instantiateTypeWithContext env sourceType Map.empty

instantiateTypeWithContext :: Environment
                           -> Src.Type
                           -> Map.Map String Variable
                           -> IO ([Variable], Type)
instantiateTypeWithContext env sourceType dict =
  do (tipe, dict') <- State.runStateT (go sourceType) dict
     return (Map.elems dict', tipe)
  where
    go :: Src.Type -> State.StateT (Map.Map String Variable) IO Type
    go sourceType =
      case sourceType of
        Src.Lambda t1 t2 -> TermN <$> (Fun1 <$> go t1 <*> go t2)

        Src.Var x -> do
          dict <- State.get
          case Map.lookup x dict of
            Just var -> return (VarN var)
            Nothing -> do
              var <- State.liftIO $ namedVar Flexible x -- should this be Constant or Flexible?
              State.put (Map.insert x var dict)
              return (VarN var)

        Src.Data name ts -> do
          ts' <- mapM go ts
          return $ foldl (\tyFn ty -> TermN $ App1 tyFn ty) (get env types name) ts'

        Src.EmptyRecord -> return (TermN EmptyRecord1)

        Src.Record fields ext ->
          TermN <$> (Record1 <$> Traverse.traverse (mapM go) fields <*> go ext)
