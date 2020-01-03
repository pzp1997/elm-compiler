Thoughts While Implementing Inlining

Q: How can you tell whether the compiler is running in --optimize mode?
    - Is the Optimize stage only run in --optimize mode?

NotFoundVar errors need to know what definitions are in scope
    - Hopefully, there's a map of [Name -> Definition Expr] that we can use
    - At the very least, they need a set of Names
    - Maybe there's another structure that maps Name -> Definition Expr...

Q: When do we actually mutate the AST?
    - Should we splice the AST?
    - Should we annotate the AST and deal with it at codegen?

Canonical/Env.hs contains the map of [Name -> Region]
    - Maybe we can just add the defining expression to this map?
    - Separate maps for {qualified,unqualified}x{local,top level} variables

Q: What is Can.SaveTheEnvironment?

I don't think we need to worry about removing the definition
    - Dead-code elimination should take care of it if it gets fully inlined

Compile.hs is the entry point

Q: Is Nitpick only for warnings? Could we sneak in an AST transformations there?

Must compute def-use chains in pass before we actually inline!
    - Let's compute the DU's in Canonicalize and store it in some context
    - Add our own AST transform pass between Canonicalize and Optimize to inline definition
    - Eventually, it would be good to integrate that pass with Optimize
    - We should also have access to all the definitions by name in Optimize, i.e. Data.Map Name.Name Can.Def


Be careful about inlining something that's exported?


Unrestricted Kernel use
    - modify compiler/src/Elm/Package.isKernel to always return True