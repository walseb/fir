# Getting started with FIR

* [Installation instructions](#installation)
  - [Installing the library](#library)
  - [Installing the documentation](#docs)
  - [Installing the built-in examples](#examples)
* [Writing a simple shader](#simple-shader)
* [Compiling shaders](#compiling)
* [Inspecting the AST](#ast)
* [Controlling inlining](#inlining)
* [Using SPIR-V tools](#spirv)
* [Creating a shader pipeline](#pipeline)

<a name="installation"></a>
## Installation instructions

<a name="library"></a>
### Installing the library

The library itself only has Haskell dependencies, so can easily be installed using a Haskell compiler and package manager:
 * FIR requires [GHC](https://www.haskell.org/ghc/), version 8.8 or greater. See the [GHC downloads page](https://www.haskell.org/ghc/download.html).
 * For the package manager, we'll use [cabal](https://www.haskell.org/cabal/download.html), version 3.0 or greater, but other package managers will work too.

To use FIR in a project, with the GHC and cabal executables added to PATH, we can create a new cabal project using `cabal init`. We then specify a dependency on FIR by adding `fir` to the `build-depends` field of the generated `projectName.cabal` file.
We also need to tell cabal to fetch the project from this repository; this can be achieved by creating the following `cabal.project` file:

```
packages: .

source-repository-package
  type:     git
  location: https://gitlab.com/sheaf/fir.git
  tag:      head
```

Alternatively, download the library (either manually or by running `git clone https://gitlab.com/sheaf/fir.git`) and point to it locally in the `cabal.project` file:

```
packages: .
          path/to/fir
```

After running `cabal update` to fetch the latest Haskell package list, run `cabal build` to build the project.


This completes the required setup; you should now be ready to write shaders and compile them to SPIR-V by importing the library.
A basic example of a shader written using this library is given [below](#simple-shader). This example [will compile to a SPIR-V file](#compiling), which can then be used in any Vulkan program, in the same way as if one was writing shaders using GLSL and compiling them with the official `glslangvalidator` front-end.


<a name="docs"></a>
### Installing the documentation

To build the documentation, we use Haddock. The `haddock` executable should be in the PATH, as it is bundled with GHC. Start building the documentation by running

```
> cabal haddock --haddock-options="--show-all --hyperlinked-source" --enable-documentation
```

The flag `--show-all` builds the documentation for all modules in FIR, including the internal ones.
The flag `--hyperlinked-source` allows the source code to be browsed through haddock, including hyperlinking identifiers whenever possible.
The flag `--enable-documentation` builds the documentation for all the dependencies.

Upon completion, the documentation can be found in `fir/dist-newstyle/build/{arch}/{ghc-version}/{fir-version}/doc/html/fir/`; start browsing with `index.html`.


<a name="examples"></a>
### Installing the built-in examples

Some [simple examples](fir-examples/readme.md) are included, which use the [vulkan-api](http://hackage.haskell.org/package/vulkan-api) Haskell bindings.
Refer to the examples page for further information about the examples, including installation instructions.


<a name="simple-shader"></a>
## Writing a simple shader
To write a shader using FIR, create a new module which imports `FIR`.
Additional functionality is provided by the modules `Math.Linear` (vectors and matrices),
`Math.Quaternion` (quaternions) and `FIR.Labels` (optional imperative-like syntax using *OverloadedLabels*).

The important types are:
* __`AST a`__: code for a pure value of type `a`, represented internally as an abstract syntax tree.
Type class overloading allows for simple construction of values of this type, e.g. one has:
```haskell
( \ t -> exp ( - tan t ** 2 ) / ( cos t ** 4 ) ) :: AST Float -> AST Float
```
* __`Program defs a`__: a program that can be compiled to SPIR-V. `defs` is a type-level map which specifies the program inputs/outputs, top-level functions and entry-points; this is the mechanism by which the user specifies shader interfaces and execution modes.
* __`Codensity AST (AST a := j) i`__: an *indexed* monadic expression, starting in state `i` and ending in state `j`. Can be thought of as stateful GPU code producing a value of type `a`. The additional type-level information (the indices) allows the library to enforce program correctness.

Shaders are written using __do__ notation, using this indexed monad (with *RebindableSyntax*).
Stateful operations such as reading/writing data use lens-like syntax, such as:

* `var <- get @"varName"`: get the value of `varName` (within the current monadic context),
* `put @"varName" val`: set the value of `varName` to `val`,
* `var <- def @"varName" @RW val`: define a new readable and writable variable, initialised to `val`, and records its value as `var`. This idiom is useful to preserve sharing, [see below](#inlining).

More general operations such as `use` and `assign` are also available, which take a type-level optic as an (invisible) type-level argument.

To illustrate, consider the following basic vertex shader (required language extensions omitted):

```haskell
import FIR
import Math.Linear

type VertexShaderDefs =
  '[ "ubo"    ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                      ( Struct '[ "mvp" ':->  M 4 4 Float ] )
   , "in_pos" ':-> Input '[ Location 0 ] (V 4 Float)
   , "main"   ':-> EntryPoint '[] Vertex
   ]

vertexShader :: Program VertexShaderDefs ()
vertexShader = Program $ entryPoint @"main" @Vertex do
  mvp <- use @(Name "ubo" :.: Name "mvp") -- (:.:) denotes composition of type-level lenses
  in_pos <- get @"in_pos"
  put @"gl_Position" (mvp !*^ in_pos) -- (!*^) means "matrix times vector"
```

The type-level map `VertexShaderDefs` provides the interface for the vertex shader, in this case specifying that it has access to a uniform buffer object `ubo` (consting of a model-view-projection matrix) and position data (given to the GPU as part of a vertex buffer). The shader writes to `gl_Position`, which is a variable that is built into vertex shaders with Vulkan.

<a name="compiling"></a>
## Compiling shaders

To compile a shader, use the `compile` function:
```haskell
compile :: FilePath -> [CompilerFlag] -> Program defs a -> IO ( Either ShortText ModuleRequirements )
```
To compile the above vertex shader, we can run the function
```haskell
compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compile "vert.spv" [] vertexShader
```
Sometimes it is more convenient to have the shaders be compiled when we compile our graphics application, as opposed to when we run it. To that end, a simple Template Haskell function is also provided:
```haskell
runCompilationsTH :: [ ( ShortText, IO (Either ShortText ModuleRequirements) ) ] -> Q Exp
```
To compile the above vertexShader at compile-time it suffices to perform a TH splice:
```haskell
shaderCompilationResult :: Either ShortText ModuleRequirements
shaderCompilationResult
  = $( runCompilationsTH
        [ ("Simple vertex shader", compileVertexShader) ]
     )
```

This produces the following SPIR-V (see the [section on SPIR-V tools](#spirv)):

```
> spirv-dis vert.spv

               OpCapability Shader
               OpCapability Matrix
               OpMemoryModel Logical GLSL450
               OpEntryPoint Vertex %main "main" %gl_Position %in_pos
               OpName %main "main"
               OpName %gl_Position "gl_Position"
               OpName %in_pos "in_pos"
               OpName %ubo "ubo"
               OpName %_ptr_ubo "_ptr_ubo"
               OpMemberName %ubo 0 "mvp"
               OpDecorate %_ptr_ubo Binding 0
               OpDecorate %_ptr_ubo DescriptorSet 0
               OpDecorate %ubo Block
               OpDecorate %in_pos Location 0
               OpDecorate %gl_Position BuiltIn Position
               OpMemberDecorate %ubo 0 ColMajor
               OpMemberDecorate %ubo 0 MatrixStride 16
               OpMemberDecorate %ubo 0 Offset 0
       %void = OpTypeVoid
          %2 = OpTypeFunction %void
      %float = OpTypeFloat 32
    %v4float = OpTypeVector %float 4
%mat4v4float = OpTypeMatrix %v4float 4
        %ubo = OpTypeStruct %mat4v4float
%_ptr_Uniform_ubo = OpTypePointer Uniform %ubo
       %uint = OpTypeInt 32 0
     %uint_0 = OpConstant %uint 0
%_ptr_Uniform_mat4v4float = OpTypePointer Uniform %mat4v4float
%_ptr_Input_v4float = OpTypePointer Input %v4float
%_ptr_Output_v4float = OpTypePointer Output %v4float
%gl_Position = OpVariable %_ptr_Output_v4float Output
     %in_pos = OpVariable %_ptr_Input_v4float Input
   %_ptr_ubo = OpVariable %_ptr_Uniform_ubo Uniform
       %main = OpFunction %void None %2
          %4 = OpLabel
         %14 = OpInBoundsAccessChain %_ptr_Uniform_mat4v4float %_ptr_ubo %uint_0
         %15 = OpLoad %mat4v4float %14
         %18 = OpLoad %v4float %in_pos
         %19 = OpMatrixTimesVector %v4float %15 %18
               OpStore %gl_Position %19
               OpReturn
               OpFunctionEnd
```

<a name="ast"></a>
## Inspecting the AST

It is possible to view the AST that this library generates, using the `ast` command. For instance:

```
> ast vertexShader

Bind
 ├╴Entry @Vertex
 │  └╴Bind
 │     ├╴Use @(Binding "ubo" :.: Index 0)
 │     └╴Lam %2
 │        └╴Bind
 │           ├╴Use @(Binding "in_pos")
 │           └╴Lam %3
 │              └╴Bind
 │                 ├╴Assign @(Binding "gl_Position")
 │                 │  └╴PrimOp MatrixTimesVector
 │                 │     ├╴%2
 │                 │     └╴%3
 │                 └╴Lam %4
 │                    └╴Return
 │                       └╴%4
 └╴Lam %1
    └╴Return
       └╴%1
```

The AST pretty-printing is performed using the [tree-view](http://hackage.haskell.org/package/tree-view) package.

If you get an error when attempting to pretty-print an AST, such as:

> *** Exception: <stdout>: hPutChar: invalid argument (invalid character)

it might be necessary to set your console to support UTF-8 output.
On Windows, this can be achieved with the command `chcp.com 65001`.

<a name="inlining"></a>
## Controlling inlining

The SPIR-V created by this library tends to be of high quality, e.g. using [phi functions](https://en.wikipedia.org/wiki/Static_single_assignment_form) instead of load/store operations, and vectorised operations whenever possible. However, one needs to be careful about inlining. Consider the following example:

```haskell
inlined :: AST Float -> AST (V 3 Float)
inlined t
  let u = cos ( 2 * pi * t )
  in  Vec3 u u u
```
As `let x = a in e` desugars to `(\x -> e) a`, `u` is inlined, which results in a loss of sharing:
```
%1 = OpFMul %float %float_2 %float_pi
%2 = OpFMul %float %1 %t
%3 = OpExtInst %float %GLSL Cos %2
%4 = OpFMul %float %float_2 %float_pi
%5 = OpFMul %float %4 %t
%6 = OpExtInst %float %GLSL Cos %5
%7 = OpFMul %float %float_2 %float_pi
%8 = OpFMul %float %7 %t
%9 = OpExtInst %float %GLSL Cos %8
%r = OpCompositeConstruct %v3float %3 %6 %9
```

This effect can compound rapidly with successive inlinings, so it is best to be careful.
To circumvent this problem, we define variables that record the result of intermediate computations, as follows:

```haskell
shared :: AST Float -> Codensity AST (AST (V 3 Float) := _j) _i
shared t = do
  u <- def @"u" @R $ cos ( 2 * pi * t )
  pure (Vec3 u u u)
```
Usage of this function compiles to the appropriate SPIR-V code:
```
%1 = OpFMul %float %float_2 %float_pi
%2 = OpFMul %float %1 %t
%3 = OpExtInst %float %GLSL Cos %2
%r = OpCompositeConstruct %v3float %3 %3 %3
 ```


<a name="spirv"></a>
## Using SPIR-V tools

Khronos provides many useful tools to deal with SPIR-V, which are included in the Vulkan SDK.  
See the [examples readme](fir-examples/readme.md#installation) for instructions concerning installation of the Vulkan SDK.

Example usage of SPIR-V tools:

* spirv-val: validate a SPIR-V file:
  - `spirv-val sourceProg.spv` to validate.
* spirv-dis: display the SPIR-V instructions in human-readable format:
  - `spirv-dis sourceProg.spv` to view,
  - `spirv-dis sourceProg.spv -o sourceProg.spv-asm` to create an editable disassembly.
* spirv-as: re-assemble SPIR-V disassembly:
  - `spirv-as dissassembly.spv-asm -o reassembled.spv`.
* spirv-opt: optimise a SPIR-V file:
  - `spirv-opt sourceProg.spv -O -o sourceProg_opt.spv` to optimise for performance,
  - `spirv-opt sourceProg.spv -Os -o sourceProg_opt.spv` to optimise size of SPIR-V binary.
* spirv-cross: cross-compile a SPIR-V file to another shading language (GLSL/HLSL/Metal).
* spirv-cfg: create a control flow graph, output as a DOT graph:
  - `spirv-cfg sourceProv.spv -o sourceProgCFG.dot` to create the DOT graph,
  - `dot -Tpdf sourceProgCFG.dot -o sourceProgCFG.pdf` to render the graph into a PDF (requires GraphViz `dot` executable).

<div align="center">
![Control flow graph of compute shader for FIR logo](img/logo_compute_cfg.png)
</div>


<a name="pipeline"></a>
## Creating a shader pipeline

Graphics shaders, such as vertex or fragment shaders, cannot be used on their own.
Instead, they must be combined into a graphics pipeline, where the output of one shader becomes the input for the next.

Consider for example the following simple pipeline, consisting of a vertex shader fed into a fragment shader:

```haskell
type VertexInput =
  '[ Slot 0 0 ':-> V 3 Float -- a 3-vector specifying position, located in slot 0, component 0
   , Slot 1 0 ':-> V 4 Float -- a 4-vector specifying colour  , located in slot 1, component 0
   ]

type VertexDefs =
  '[ "in_position" ':-> Input  '[ Location 0 ] (V 3 Float)
   , "in_colour"   ':-> Input  '[ Location 1 ] (V 4 Float)
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp" ':-> M 4 4 Float ] )
   , "main"        ':-> EntryPoint '[] Vertex
   ]
vertex :: ShaderStage "main" VertexShader VertexDefs _
vertex = shader do
  ~(Vec3 x y z) <- get @"in_position"
  mvp <- use @(Name "ubo" :.: Name "mvp")
  put @"gl_Position" ( mvp !*^ Vec4 x y z 1 )
  put @"out_colour"  =<< get @"in_colour"

type FragmentDefs =
  '[ "in_colour"  ':-> Input      '[ Location 0 ] (V 4 Float)
   , "out_colour" ':-> Output     '[ Location 0 ] (V 4 Float)
   , "main"       ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]
fragment :: ShaderStage "main" FragmentShader FragmentDefs _
fragment = shader do
  put @"out_colour" =<< get @"in_colour"


shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(Triangle List)
  $    StartPipeline
  :>-> (vertex  , "shaders/vert.spv")
  :>-> (fragment, "shaders/frag.spv")
```

Here, we specify:

  * The type of the input to the pipeline. This consists of the following information:

    - The primitive topology used for vertex input assembly. In this case, the topology is `Triangle List`, meaning that we are specifying a list of vertices, with consecutive groups of three vertices forming a triangle.
    The primitive topology is supplied with type-level data of kind [FIR.Pipeline.PrimitiveTopology Nat](src/FIR/Pipeline.hs), such as `Points`, `Triangle Fan` or `PatchesOfSize 9`.

    - The type of the data attached to each vertex. In this case we are attaching a structure to each vertex, of type `Struct VertexInput`. When it comes to performing a draw call using this pipeline, the Vulkan application will use a vertex buffer, which will need to have been appropriately populated with position and colour data for each vertex in the buffer.
    
    Note that we have to specify layout information using location and component slots, as explained in [FIR.Layout](src/FIR/Layout.hs).
    
    See also the [shaders from the Hopf fibration example](fir-examples/src/Examples/Hopf/Shaders.hs), which illustrate the usage of this layout information.

  * The shaders which make up the pipeline, in the form of a snoc-list `StartPipeline :>-> shader_1 :>-> ... :>-> shader_n`.


This library will then validate the pipeline at the type-level (i.e. at compile-time) to ensure that:

  * the pipeline sequence is correct (e.g. vertex shader preceding fragment shader),
  * the primitive topology is compatible with the shaders (e.g. in the presence of tessellation shaders, the primitive topology __must__ be `PatchesOfSize (n :: Nat)`),
  * the shader interfaces match (including layout information and decorations).

This is useful as it catches mistakes in shaders and shader interfaces (including some that the official Vulkan validator doesn't catch). Much better to get a compile-time type error that a black screen at runtime!


Convenience functionality to create Vulkan graphics pipelines from such information is also provided, using the [vulkan-api](http://hackage.haskell.org/package/vulkan-api) Haskell bindings (see [`Vulkan.Pipeline.createGraphicsPipeline`](fir-examples/src/Vulkan/Pipeline.hs)).
