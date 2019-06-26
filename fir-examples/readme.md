
# Examples

* [Installation instructions](#installation)
* [Overview of examples](#overview)
  - [Full graphics pipeline](#fullpipeline)
  - [Compute shader: FIR logo](#compute)
  - [Texture sampling](#texture)
  - [Julia set](#julia)
  - [Offscreen rendering](#offscreen)
  - [Bézier curves](#bezier)


<a name="installation"></a>
## Installation instructions
To build the examples, the installation of two external dependencies is necessary: SDL2 and Vulkan.
Once these are installed, one can do:

```
> cd fir-examples
> cabal build
> cabal run Example
```

where *Example* is any one of the [examples](#overview).

<a name="overview"></a>
## Overview of examples

<a name="fullpipeline"></a>
### Full graphics pipeline
<div align="center">
![FullPipeline](../img/full_pipeline.png)
</div>

A graphics pipeline containing all shader stages.

<a name="compute"></a>
### Compute shader: FIR logo
<div align="center">
![Compute](../img/compute.png)
</div>

Rendering the FIR logo using simple ray tracing in a compute shader.

<a name="texture"></a>
### Texture sampling
<div align="center">
![Texture](../img/texture.png)
</div>

Demonstrates how to sample a texture.

<a name="julia"></a>
### Julia set
<div align="center">
![JuliaSet](../img/julia.png)
</div>

Interactive Julia set rendering, computed within a fragment shader.

<a name="offscreen"></a>
### Offscreen rendering
<div align="center">
![Offscreen](../img/offscreen.png)
</div>

Offscreen rendering of a single frame.

<a name="bezier"></a>
### Bézier curves
<div align="center">
![Bezier](../img/bezier.png)
</div>

Work in progress: rendering Bézier curves using tessellation and geometry shaders.
Currently computes signed distance to the outline. Still requires a second pass in a compute shader to fill in the outline.
