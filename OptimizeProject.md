# Optimization Annotations for Elm

## Background

The job of a compiler is to translate code from a source language to equivalent code in a target language. There are many functionally equivalent ways to translate a piece of code in the source language to the target language, and each translation comes with its own set of tradeoffs. So how does the compiler know which translation to use? Traditionally, the compiler designers decide by considering which translation would yield the most desirable tradeoffs in the average case.

## Project Overview

I propose that we add optimization annotations to Elm. These annotations allow the developer to explicitly specify how their code should be optimized, giving them more control over the generated code. If the compiler cannot honor a specified optimization annotation, e.g. attempting to eliminate a tail-call in a function that is not tail recursive, it will emit an error and fail. Optimizations that are always desirable will be applied whenever possible even if no optimization annotation is present (these include all existing optimizations), while optimizations that make tradeoffs will only be applied when the corresponding optimization annotation is present.

## Deliverables
1. Add compiler support for optimization annotations
2. Onboard existing optimizations to the annotations framework
3. Design new optimizations
4. Make it easy to add optimizations in the future

## Design Goals

1. Transparency
Make it easy to determine which optimizations are being applied without having to look at the generated code.

2. Allow for Different Trade-offs
Many optimizations actually make a trade-off, e.g. inlining generally improves performance but increases bundle size. Traditionally, compilers use heuristics to decide whether applying a particular optimization would be beneficial. The consequence of this is that most compilers optimize for the general use-case rather than a specific one. If a developer is developing an application for the web, they might be extremely sensitive to bundle size and care less about performance. Alternatively, if a developer is building a command-line interface, performance might matter way more than bundle size. The point is the compiler can only guess at what the developer considers beneficial, so it would be way better if the developer can specify that explicitly via optimization annotations.

3. Keep "Normal" Compilations Fast
Evan (the creator of Elm) has invested a lot of time into making compilation fast for large projects; we do not want our work to negate those wins. If a developer chooses not to use optimization annotations, the changes we make to the compiler should have negligible impact on their build time. This is a legitimate concern as, for some optimizations, checking whether they can be safely applied requires expensive computations. We want these potentially expensive computations to be opt-in.

## Possible Optimizations

_Current_
ELIMINATE_TAIL_CALL (applies to `Value` declarations, specifically recursive functions)
ELIMINATE_DEAD_CODE (applies to all declarations)
 - adding an annotation would allow developers to safely leave Debug uses in unreachable code
UNBOX_PRIMITIVE (applies to `Type` declarations)

_Proposed_
INLINE (applies to `Value` declarations)
FOLD_CONSTANTS
UNROLL_LOOP (applies to `Value` declarations, specifically recursive functions)

_Stretch Goals_
UPDATE_IN_PLACE
MONOMORPHIZE
UNCURRY

TODO: Look at Haskell's optimizations for more ideas

## Alternate Designs

- Warn/do nothing if optimizations are skipped instead of erroring
- High-level optimization strategies/modes, e.g. `elm make --optimize=BUNDLE_SIZE` or `elm make --optimize=EXECUTION_SPEED`

## Milestones

1. Create AST attribute on a declaration to specify which optimizations should be applied
2. Parse OPTIMIZE annotations into corresponding AST representation
3. Write error messages for when requested optimizations cannot be applied
4. Check that requested optimizations are applied and throw error if not

## General Notes About the Compiler

### Stages

source code
~[Parse]~> literal AST
~[Canonicalize]~> canonicalized AST
~[Optimize]~> optimized AST
~[Generate]~> JavaScript source code

### Module Descriptions

**Ast.Source**
 - literal representation of source code

**Ast.Canonical**
 - cross-module name resolution
 - mostly same structure as Ast.Source

**Ast.Optimized**
 - more efficient encoding
 - special cases for certain patterns that can be optimized
   - identifies tail-calls

**Parse**
 - converts Elm source code to Ast.Source

**Canonicalize**
 - converts Ast.Source to Ast.Canonical

**Optimize**
 - converts Ast.Canonical to Ast.Optimized

**Generate**
 - converts Ast.Optimized to JavaScript source code

**Generate.JavaScript**
 - performs dead code elimination

**Generate.JavaScript.Functions**
 - defines Fn and An functions for currying

**Nitpick.PatternMatches**
 - operates on Ast.Canonical
 - checks that pattern matches are exhaustive

**Nitpick.Debug**
 - operates on Ast.Optimized
 - checks that `Debug` module is not used
 - called by Generate when the production generator is used

### Code Generators and Compilation Modes

The four code generators are production, development, debug, and REPL
The two compilation modes are JavaScript and HTML
