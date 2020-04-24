data Can.Def
  = Def (A.Located Name) [Pattern] Can.Expr
  | TypedDef (A.Located Name) FreeVars [(Pattern, Type)] Can.Expr Type

data Can.Decls
  = Declare Can.Def Can.Decls
  | DeclareRec Can.Def [Can.Def] Can.Decls
  | SaveTheEnvironment

data ModuleName.Canonical =
  Canonical
    { _package :: !Pkg.Name
    , _module :: !Name.Name
    }

data Opt.Global = Global ModuleName.Canonical Name

data Opt.LocalGraph =
  LocalGraph
    { _l_main :: Maybe Main
    , _l_nodes :: Map.Map Opt.Global Opt.Node  -- PERF profile switching Global to Name
    , _l_fields :: Map.Map Name Int
    }

data Opt.Def
  = Def Name Opt.Expr
  | TailDef Name [Name] Opt.Expr

data Opt.Node
  = Define Opt.Expr (Set.Set Opt.Global)
  | DefineTailFunc [Name] Opt.Expr (Set.Set Opt.Global)
  | Cycle [Name] [(Name, Opt.Expr)] [Opt.Def] (Set.Set Opt.Global)
  | ...



Optimize.Module.addDef :: ModuleName.Canonical -> Annotations -> Can.Def -> Opt.LocalGraph -> Result i [W.Warning] Opt.LocalGraph
Optimize.Module.addDefHelp :: A.Region -> Annotations -> ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> Opt.LocalGraph -> Result i w Opt.LocalGraph
Optimize.Module.addDefNode :: ModuleName.Canonical -> Name.Name -> [Can.Pattern] -> Can.Expr -> Set.Set Opt.Global -> Opt.LocalGraph -> Opt.LocalGraph


Optimize.Expression.optimize :: Cycle -> Can.Expr -> Names.Tracker Opt.Expr
Optimize.Names.run :: Names.Tracker a -> (Set.Set Opt.Global, Map.Map Name.Name Int, a)


-- TODO aggregate usage across modules
-- Definitions from modules are not free locals!
