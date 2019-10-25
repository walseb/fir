# Development information

This document provides an overview of the library, from a developer's perspective.    
For explanations regarding usage of the library, refer instead to the [project readme](readme.md)
as well as the ["getting started" guide](getting_started.md).    

__Last updated__: October 2019.

* [Quick summary](#summary)
* [Project structure](#structure)
* [Highlights](#highlights)
* [Tests](#tests)

<a name="summary"></a>
## Quick summary

The project occupies three main namespaces, each reflecting one aspect of compilation.
  
  * __FIR__: types and operations used by this library, usually user-facing.
    - __FIR.AST__ defines the abstract syntax tree used by this library.
    - Modules in the __FIR.Syntax__ namespace provide syntactic sugar for constructing
    and manipulating ASTs.
  * __CodeGen__: code generation, turning ASTs into SPIR-V objects.
  * __SPIRV__: backend types and operations, corresponding to the compilation target: SPIR-V.

To elaborate somewhat, recall from [the project readme](readme.md) that a SPIR-V module is specified in two parts:
  * Type-level definitions specifying the input/output interface for the module,
    as well as other information such as execution modes and decorations.
  * The actual module code, an indexed-monadic value
  (usually constructed using rebindable do-notation for the indexed monad this library uses),
  of type `Codensity AST (AST a := j) i` (this is the McBride codensity representation of a stateful AST).
g
Modules from the __FIR__ namespace give the ability to the user to write such programs,
in large part using typeclass overloading.
The code-generator turns these into SPIR-V instructions and opcodes, which are emitted in binary form.

<a name="structure"></a>
## Project structure

```
FIR
  Exposed module that re-exports the main functionality of the library,
  providing the ability to compile programs.

FIR
 ├╴AST
 │    Definition of the abstract syntax tree used by this library.
 │
 ├╴Definition
 │    Specifies the type-level annotations needed for a module,
 │    e.g. input/output variables, uniforms, and other entry-point information (such as execution modes).
 │
 ├╴ProgramState
 │    This is the type used for indexing with the indexed monad used by this library.
 │    This type keeps track of available bindings (inputs/outputs as well as user-defined bindings),
 │    as well as other contextual information useful for program validation.
 │
 ├╴Module
 │    Defines what constitutes a unit of compilation for this library,
 │    corresponding to the notion of a SPIR-V module.
 │
 ├╴Prim
 │  └╴...
 │    Primitive types and operations defined by this library:
 │      - arrays, structures, images,
 │      - operation names,
 │      - singletons for primitive types, to allow the code-generator to dispatch on types.
 │    Note that vectors and matrices are defined in "Math.Linear" instead.
 │    These user-facing types and operations then map onto the backend SPIR-V types and operations
 │    (see the SPIRV module hierarchy).
 │
 ├╴Syntax
 │  │   User-facing syntax of the library.
 │  │   
 │  ├╴AST
 │  │   Syntax for objects of type 'AST a'.
 │  │   Mostly contains type-class instances, e.g. 'Floating a => Floating (AST a)'.
 │  │  
 │  ├╴Codensity
 │  │   Syntax for objects of type 'Codensity AST (AST a := j) i'.
 │  │   Most importantly, stateful operations:
 │  │     - 'get'/'put'/'use'/'assign',
 │  │     - 'imageRead'/'imageWrite',
 │  │     - control flow: loops, monadic if statement,
 │  │     - 'emitVertex'/'endPrimitive' operations for geometry shaders.
 │  │   Also contains type-class instances, e.g. 'Floating a => Floating (Codensity AST (AST a := i) i)'.
 │  │   
 │  ├╴Labels
 │  │   Optional imperative-like syntax for stateful 'get'/'put' operations,
 │  │   using overloaded labels.
 │  │   
 │  ├╴Optics
 │  │   Instances for getters/setters operating on primitive types, such as vectors, matrices, arrays, structures.
 │  │   Note that instances for objects of type 'AST a' are to be found in "Syntax.AST" instead.
 │  │   
 │  ├╴Swizzle
 │  │   Swizzle optics (as syntactic sugar for product optics).
 │  │   
 │  ├╴Synonyms
 │  │   Various useful type and pattern synonyms.
 │  └╴...
 ├╴Layout
 │    Performs memory layout for vertex, uniform and storage buffers.
 │
 │╴Pipeline
 │    Defines shader pipelines, specifying:
 │      * the shape of vertex input data,
 │      * primitive topology,
 │      * the actual shaders forming the pipeline.
 └╴...

Math
 ├╴Algebra
 │  └╴...
 │    Alternative type classes for arithmetic.
 │    Unfortunately the standard prelude is too limited for the purposes of this library.
 │    See for instance Math.Algebra.Class which includes an in-depth explanation.
 │ 
 ├╴Linear
 │    Vectors, matrices, and the usual operations involving them.
 │ 
 └╴Logic
    └╴...
      Alternative type classes for logical operations (Eq, Ord, Bits,...).

Control
 ├╴Monad
 │  └╴Indexed
 │      McBride-style indexed monads.
 │      Used for constructing stateful shader programs.
 │ 
 └╴Type
    └╴Optic
        Type-level optics and optic combinators used by this library.

CodeGen
 ├╴CodeGen
 │   Main code-generation, turning a value of type 'AST a' into SPIR-V code.
 │   Calls out to various parts of the code generator for different AST constructors.
 │   Most modules are concerned with self-explanatory aspects of the AST (not included below).
 │ 
 ├╴Instruction
 │   Defines what constitutes an instruction: a SPIR-V instruction name together with arguments.
 │   Can return a result, which is given a unique ID number as per the SPIR-V SSA form.
 │   Also specifies how to emit binary corresponding to an instruction.
 │
 ├╴Monad
 │   Defines 'CGMonad', the monad used for code generation.
 │   Consists of a transformer stack providing the following effects:
 │     - emitting binary ('Binary.PutM' monad),
 │     - keeping track of code-generator state ('ReaderT' + 'StateT' transformers),
 │     - exception handling ('ExceptT' transformer),
 │     - keeping a supply of fresh unique ID instruction numbers,
 │       using a custom 'MonadFresh' effect.
 │ 
 ├╴State
 │   Defines the state and context types used for code-generation.
 │ 
 ├╴Application
 │   Patterns for function application, to keep track of how many arguments a function is applied to.
 │ 
 ├╴IDs
 │   Generation of new ID numbers when needed, when defining types and constants.
 │   For instance, if we need to refer to the SPIR-V type corresponding to e.g. 'V 4 Float', we need to:
 │     * Check if this type has already been given a unique ID. If so, use that one.
 │     * Otherwise, we recursively obtain ID numbers for 'Float' as well as the constant '4'.
 │       Once these are known, we can create the SPIR-V type corresponding to 'V 4 Float',
 │       giving it a newly generated ID number.
 │     
 ├╴Binary
 │    Functions that emit binary corresponding to a SPIR-V module.
 └╴...

SPIRV
 └╴...
   Includes backend types and operations, such as:
     * execution models (such as shader stages), with corresponding execution modes,
     * primitive SPIR-V types and operations (and opcodes),
     * images and image formats,
     * SPIR-V decorations (e.g. layout information, binding indices, ...).

```

<a name="highlights"></a>
## Highlights

Here are a few aspects of the backend that I find interesting and would like to highlight:

  * Obtaining unique instruction IDs recursively, see [CodeGen.IDs](src/CodeGen/IDs.hs).
  This is a breeze with traversals and recursion.    
  Note that singletons are used quite crucially in `CodeGen.IDs.constID`, which
  obtains an ID for a constant value. The singleton allows us to do a case analysis
  on the type of the constant whose ID we desire.
  Pattern matching on the singleton specialises the type we are working with,
  so that e.g. for a vector we are gain the ability to traverse it,
  allowing us to generate IDs for its components.

  * Custom lenses in [CodeGen.State](src/CodeGen/State.hs) which update multiple different aspects
  of the code-generator state at once.    
  For instance, performing an assignment using the `_builtin` lens performs the following operations:
    - informs the code-generator that the built-in variable has been used by the user,
    - adds the built-in to the interface of the entry-point it was used within,
    - adds the necessary decorations to the built-in variable.

  * The use of the codensity transformation for McBride-style indexed monads,
  which in conjunction with the `Syntactic` type-class provides
  a convenient way to handle monadic AST tagged with type-level information.    
  See [Control.Monad.Indexed](src/Control/Monad/Indexed.hs) for the definition
  of the codensity transformation, and [FIR.Syntax.Codensity](src/FIR/Syntax/Codensity.hs)
  for the `Syntactic` instance which allow the library to easily internalise and externalise
  stateful code, preserving the ability to do code-generation.

  * An overlapping instances trick to enable usage of the `ImageTexel` optic,
  which allows image read/write/sample operations to be performed using usual lens syntax,
  while maintaining the property that images are void opaque types with no actual values.    
  See [FIR.Syntax.Images](src/FIR/Syntax/Images.hs), which includes a detailed explanation.

  * Code-generation for branching control flow using ϕ-functions.
  See [CodeGen.CodeGen](src/CodeGen/CodeGen.hs), specifically the cases of the function
  `codeGen` for the constructors `IfM` and `While`.

  * _Hall of shame_ Vectorisation of functions by applying a function at the wrong type
  using `unsafeCoerce`, manually fixing up the resulting AST, and then using another
  `unsafeCoerce` to get back to the right type. See the vectorisation hacks
  in [CodeGen.Applicative](src/CodeGen/Applicative.hs).
  

<a name="tests"></a>
## Tests

Some tests are included in the [test subdirectory](test/).    

For the moment, the most convenient way of running the tests is as follows:

```
> cabal build
> cabal repl
> :l test/Tests.hs
> runAllTests
```

It is important to ensure the library is re-built before running the tests,
as the test-suite calls out to `ghc`/`ghci`, which will use the package environment.
If the library has not been re-built, this might cause the tests to run with an old version of the library.
