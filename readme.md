
![FIR](https://i.imgur.com/Ef5zbt6.png)

---

* [Introduction](#introduction)
* [Getting started](#getting-started)
* [Examples](#examples)

<a name="introduction"></a>
# Introduction
**FIR** is an embedded language for writing GPU shaders in Haskell, compiling to *SPIR-V* for use in *Vulkan* applications.

FIR is intended as an alternative shading language to *GLSL*, providing amenities of modern functional programming such as a strong type-system, user-managed control-flow with monads and *do* notation, applicative/foldable/traversable functors, etc.
The Haskell type system helps in verifying programs at compile time, especially with the use of *indexed monads* to keep track of program state and prevent invalid operations.


<a name="getting-started"></a>
# Getting started

See [getting started](getting_started.md) for help on getting started, including:
  * installation instructions,
  * a simple vertex shader example,
  * how to compile and debug shaders, using built-in functionality as well as SPIR-V tools.

The haddock documentation is also worth perusing for specialised in-depth explanations.

<a name="examples"></a>
# Examples

![FullPipeline](img/full_pipeline_small.png) ![Compute](img/compute_small.png) ![Texture](img/texture_small.png) ![Offscreen](img/offscreen_small.png) ![Bezier](img/bezier_small.png)

Some simple examples are included in the **fir-examples** subdirectory.
See the [examples readme](fir-examples/readme.md) for installation instructions and further information.
