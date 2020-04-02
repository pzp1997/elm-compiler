Optimizations
  - Inlining
    - Inlining single use defs is kind of like generalizing unboxing from constructors to arbitrary data/functions
  - Rewrite rules
  - Constant folding
  - Constant propagation
  - full dead code elimination
  - Uncurrying of fully applied function calls
  - In-place updates

- Inlining High Pri
  - Make sure nested inlining works, i.e. foo = 1, bar = foo + 1, baz = String.fromInt bar, when bar gets inlined first
  - Bind function args when inlining functions

- Inlining Nice to Haves
  - Inline aliases foo = bar, even if foo is used many times.
  - Prevent costly top level data from getting inlined within a function
  - Remove Fn calls when possible (Robin optimization)

- Add more rewrite rules
  - Rewrite Call (String.fromInt) (Int n) —> (String n)
  - Rewrite “hello, ” ++ “world!” —> “hello, world!”
  - Rewrite [1,2,3] ++ [4,5] —> [1,2,3,4,5]
  - Rewrite arithmetic operations on constants (simulate constant folding)
  - Take a look at GHC inliner

- Figure out interleaving of inlining and rewriting
  - probably alternate until we reach a fixed point

- In-place updates of Unique Structures
  - need to figure out when it is safe to do this
  - ```js
        var $elm$core$List$map_INPLACE = F2(
          function (f, list) {
            var xs = list;
            while (xs) {
              xs.a = f(xs.a);
              xs = xs.b;
            }
            return list;
          });
      ```
