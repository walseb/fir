# Examples

* [Installation instructions](#installation)
  - [Windows](#win)
  - [Linux](#linux)
* [Hot reloading](#hotreloading)
* [Overview of examples](#overview)
  - [Full graphics pipeline](#fullpipeline)
  - [FIR logo](#logo)
  - [Hopf fibration](#hopf)
  - [Texture sampling](#texture)
  - [Julia set](#julia)
  - [Offscreen rendering](#offscreen)
  - [Bézier curves](#bezier)


<a name="installation"></a>
## Installation instructions

Start off by making sure the [library itself installs properly](../getting_started.md#installation).

To build the examples, the installation of two external dependencies is necessary: SDL2 (version 2.0.6 or greater), and the Vulkan SDK.
Instructions for building these packages is provided below, for [Windows](#win) and [Linux](#linux).

Once these dependencies are installed, you should be able to run:

```
> cd fir-examples
> cabal build
> cabal run Example
```

where *Example* is any one of the [examples](#overview).

<a name="win"></a>
### Windows

To keep track of external dependencies, we first need to install `pkg-config`.
If this is not already present on your system, download [pkg-config-lite](https://sourceforge.net/projects/pkgconfiglite/)
and add the installed location to your PATH, making `pkg-config` available in the command line.

To install SDL2, download the [SDL2 MinGW development library](https://www.libsdl.org/download-2.0.php) (version 2.0.6 or greater).
After extracting, we need to:
  * Add the `bin` folder to PATH (on 64 bit systems, the folder `SDL2-2.x.y\x86_64-w64-mingw32\bin`).
  * Add the `lib\pkg-config` folder to PKG_CONFIG_PATH to make SDL2 visible to pkg-config.
    If PKG_CONFIG_PATH does not exist (`echo %PKG_CONFIG_PATH%` returns nothing), set it with `setx PKG_CONFIG_PATH path\to\sdl2\lib\pkg-config\`.
You can check that SDL2 is registered with pkg-config using `pkg-config --list-all`.

The Windows Vulkan SDK installer can be downloaded from the [LunarG website](https://vulkan.lunarg.com/sdk/home).
No further setup should be required after installing. The relevant `bin` folder (by default `VulkanSDK\[vulkan-sdk-version]\Bin`) is automatically added to PATH, and the environment variables VULKAN_SDK and VK_SDK_PATH should also have been initialised (pointing to `VulkanSDK\[vulkan-sdk-version]` by default).


<a name="linux"></a>
### Linux
It should be possible to install the SDL2 development kit from your distributions's package repository.

* Ubuntu/Debian: `sudo apt-get install libsdl2-dev`
* ArchLinux: `pacman -S sdl2`

Please ensure that the installed version of SDL2 is at least 2.0.6, as it is this version that adds Vulkan support.
This might require adding newer package repository lists.

For Vulkan, you'll need the Vulkan SDK. What to install will usually depend on your GPU (AMD/NVIDIA/Intel).
The [LunarG website](https://vulkan.lunarg.com/doc/sdk/latest/linux/getting_started.html) provides installation instructions
for the Vulkan SDK on Linux.


<a name="hotreloading"></a>
## Hot reloading

The examples support shader hot reloading, detecting when any of the used SPIR-V files are modified on disk.    

For interactive coding, `ghcid` can be useful, e.g.:

`ghcid -c "cabal repl ShaderFile" -WT "compileAction"`    

This command watches the Haskell module `ShaderFile`, reloading it on changes,
and runs the action `compileAction` each time the module successfully loads.  

To illustrate, consider interactively editing the fragment shader used in the Julia set example, as follows:   

  * Start the executable with `cabal run JuliaSet`.
    The executable will watch for shaders changing on disk.    
    Note: you might want to run the executable in the background:
      - `cabal run JuliaSet &` on Linux/macOS.
      - `start /b cabal run JuliaSet` on Windows.
  * Enable live recompilation of the fragment shader:    
    `ghcid -c "cabal repl FIR.Examples.JuliaSet.Shaders" -WT "compileFragmentShader"`    
    This will save changes to the fragment shader to disk.   

Remarks:

  * The `-WT` flag to `ghcid` is used to specify an action to run upon loading,
    even when warnings were emitted.
  * The shader files used by the examples are, by default, located
    in the directory `fir/fir-examples/shaders`.
    These are the files that will be watched by the application to check when to reload.


<a name="overview"></a>
## Overview of examples

<a name="fullpipeline"></a>
### Full graphics pipeline
<div align="center">
![FullPipeline](../img/full_pipeline.png) <br>
[Application](src/Examples/FullPipeline/Application.hs)   •   [Shaders](src/Examples/FullPipeline/Shaders.hs)
</div>

A graphics pipeline containing all shader stages.

<a name="logo"></a>
### FIR logo
<div align="center">
![Logo](../img/logo.png) <br>
[Application](src/Examples/Logo/Application.hs)   •   [Shaders](src/Examples/Logo/Shaders.hs)
</div>

Rendering the FIR logo using simple ray tracing in a compute shader.

<a name="hopf"></a>
### Hopf fibration
<div align="center">
![Hopf](../img/hopf.png) <br>
[Application](src/Examples/Hopf/Application.hs)   •   [Shaders](src/Examples/Hopf/Shaders.hs)
</div>

Illustration of fibers in the Hopf fibration; each circle is thickened out to a torus using a tessellation shader.

Rendered using multisampling with Vulkan resolve attachments.
Also illustrates how to specify location and component layout information in shaders.

<a name="texture"></a>
### Texture sampling
<div align="center">
![Texture](../img/texture.png) <br>
[Application](src/Examples/Texture/Application.hs)   •   [Shaders](src/Examples/Texture/Shaders.hs)
</div>

Demonstrates how to sample a texture.

<a name="julia"></a>
### Julia set
<div align="center">
![JuliaSet](../img/julia.png) <br>
[Application](src/Examples/Julia/Application.hs)   •   [Shaders](src/Examples/Julia/Shaders.hs)
</div>

Interactive Julia set rendering, computed within a fragment shader.

<a name="offscreen"></a>
### Offscreen rendering
<div align="center">
![Offscreen](../img/offscreen.png) <br>
[Application](src/Examples/Offscreen/Application.hs)   •   [Shaders](src/Examples/Offscreen/Shaders.hs)
</div>

Offscreen rendering of a single frame.

<a name="bezier"></a>
### Bézier curves
<div align="center">
![Bezier](../img/bezier.png) <br>
[Application](src/Examples/Bezier/Application.hs)   •   [Shaders](src/Examples/Bezier/Shaders.hs)
</div>

Work in progress: rendering Bézier curves using tessellation and geometry shaders.
Currently computes signed distance to the outline. Still requires a second pass in a compute shader to fill in the outline.
