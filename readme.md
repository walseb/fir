
![FIR](img/banner.png)

---

* [Introduction](#introduction)
* [Getting started](#getting-started)
* [Examples](#examples)
* [Acknowledgements](#acknowledgements)

<a name="introduction"></a>
# Introduction
**FIR** is an embedded language for writing GPU shaders in Haskell, compiling to *SPIR-V* for use in *Vulkan* applications.

FIR is intended as an alternative shading language to *GLSL*, providing amenities of modern functional programming such as a strong type-system, user-managed control-flow with monads and *do* notation, applicative/foldable/traversable functors, etc.
The Haskell type system helps in verifying programs at compile time, especially with the use of *indexed monads* to keep track of program state and prevent invalid operations.


<a name="getting-started"></a>
# Getting started

FIR requires GHC >= 8.6. See [getting started](getting_started.md) for help on getting started, including:
  * installation instructions,
  * a simple vertex shader example,
  * how to compile and debug shaders, using built-in functionality as well as SPIR-V tools.

The haddock documentation is also worth perusing for specialised in-depth explanations.

<a name="examples"></a>
# Examples

<div align="center">
![FullPipeline](img/full_pipeline_small.png) ![Compute](img/compute_small.png) ![Texture](img/texture_small.png) ![JuliaSet](img/julia_small.png) ![Offscreen](img/offscreen_small.png) ![Bezier](img/bezier_small.png)
</div>

Some simple examples are included in the **fir-examples** subdirectory.
See the [examples readme](fir-examples/readme.md) for installation instructions and further information.

<a name="acknowledgements"></a>
# Acknowledgements

I extend my thanks to:
* **Baldur Blöndal**, for helping me get started with this project, indulging my silly questions, and pointing me to the paper [*Combining Deep and Shallow Embedding for EDSL*](http://www.cse.chalmers.se/~josefs/publications/TFP12.pdf) by Josef Svenningsson and Emil Axelsson, which was used in the design of the AST.
* **Callan McGill**, for encouraging me, spending long hours helping to debug the library (and getting it working on Linux), and generally keeping me sane.
* **Oliver Charles**, for inspiring me to take on this project by streaming his efforts at [porting Quake 3 to Haskell using Vulkan](https://github.com/ocharles/zero-to-quake-3). This was invaluable in helping me get started with using Vulkan in Haskell.
* **Artem Chirkin**, for his well-designed [vulkan-api](https://github.com/achirkin/vulkan) library.
* **Schell Scivally**, for patiently explaining his [ixshader library](https://hackage.haskell.org/package/ixshader).
* **Conor McBride**, whose paper [*Kleisli arrows of outrageous fortune*](https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf) forms the basis of the implementation of indexed monads which enabled the approach taken in FIR.
* **The GHC developers**, for their tireless work on improving Haskell, tremendously helpful advice, and for continually keeping me excited in the future of programming.
* **Epic Games**, for giving me a huge boost by supporting me through their [MegaGrants program](https://www.unrealengine.com/megagrants).
