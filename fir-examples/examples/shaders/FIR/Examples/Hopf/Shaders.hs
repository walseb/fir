{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.Hopf.Shaders where

-- base
import Data.Foldable
  ( sequence_ )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float, Slot 0 3 ':-> Float -- pos, R
     , Slot 1 0 ':-> V 3 Float, Slot 1 3 ':-> Float -- normal, r
     , Slot 2 0 ':-> V 4 Float                      -- col
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_pos"       ':-> Input  '[ Location 0 ] (V 3 Float)
   , "in_R"         ':-> Input  '[ Location 0, Component 3 ] Float
   , "in_normal"    ':-> Input  '[ Location 1 ] (V 3 Float)
   , "in_r"         ':-> Input  '[ Location 1, Component 3 ] Float
   , "in_colour"    ':-> Input  '[ Location 2 ] (V 4 Float)

   , "out_colour"   ':-> Output '[ Location 0 ] (V 4 Float)
   , "out_R"        ':-> Output '[ Location 1 ] Float
   , "out_normal"   ':-> Output '[ Location 2 ] (V 3 Float)
   , "out_r"        ':-> Output '[ Location 2, Component 3 ] Float

   , "main"         ':-> EntryPoint '[ ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do

    put @"out_colour" =<< get @"in_colour"

    ~(Vec3 px py pz) <- get @"in_pos"
    put @"gl_Position" ( Vec4 px py pz 1 )

    put @"out_normal" =<< get @"in_normal"

    put @"out_R" =<< get @"in_R"
    put @"out_r" =<< get @"in_r"

------------------------------------------------
-- tessellation control

type TessellationControlDefs =
  '[ "in_col"     ':-> Input  '[ Location 0 ] (Array 1 (V 4 Float))
   , "in_R"       ':-> Input  '[ Location 1 ] (Array 1 Float)
   , "in_normal"  ':-> Input  '[ Location 2 ] (Array 1 (V 3 Float))
   , "in_r"       ':-> Input  '[ Location 2, Component 3 ] (Array 1 Float)

   , "out_col"    ':-> Output '[ Location 0 ] (Array 1 (V 4 Float))
   , "out_R"      ':-> Output '[ Location 1 ] (Array 1 Float)
   , "out_normal" ':-> Output '[ Location 2 ] (Array 1 (V 3 Float))
   , "out_r"      ':-> Output '[ Location 2, Component 3 ] (Array 1 Float)
   , "main"       ':-> EntryPoint '[ SpacingEqual, VertexOrderCw, OutputVertices 1 ]
                        TessellationControl
   ]

tessellationControl :: ShaderModule "main" TessellationControlShader TessellationControlDefs _
tessellationControl = shader do

  assign @(Name "gl_TessLevelInner" :.: Index 0) 64
  assign @(Name "gl_TessLevelInner" :.: Index 1) 64
  assign @(Name "gl_TessLevelOuter" :.: Index 0) 64
  assign @(Name "gl_TessLevelOuter" :.: Index 1) 64
  assign @(Name "gl_TessLevelOuter" :.: Index 2) 64
  assign @(Name "gl_TessLevelOuter" :.: Index 3) 64

  i <- get @"gl_InvocationID"

  assign    @(Name "gl_out"     :.: AnIndex Word32 :.: Name "gl_Position") i
    =<< use @(Name "gl_in"      :.: AnIndex Word32 :.: Name "gl_Position") i
  assign    @(Name "out_col"    :.: AnIndex Word32) i
    =<< use @(Name "in_col"     :.: AnIndex Word32) i
  assign    @(Name "out_R"      :.: AnIndex Word32) i
    =<< use @(Name "in_R"       :.: AnIndex Word32) i
  assign    @(Name "out_normal" :.: AnIndex Word32) i
    =<< use @(Name "in_normal"  :.: AnIndex Word32) i
  assign    @(Name "out_r"      :.: AnIndex Word32) i
    =<< use @(Name "in_r"       :.: AnIndex Word32) i

------------------------------------------------
-- tessellation evaluation

type TessellationEvaluationDefs =
  '[ "in_col"     ':-> Input  '[ Location 0 ] (Array 1 (V 4 Float))
   , "in_R"       ':-> Input  '[ Location 1 ] (Array 1 Float)
   , "in_normal"  ':-> Input  '[ Location 2 ] (Array 1 (V 3 Float))
   , "in_r"       ':-> Input  '[ Location 2, Component 3 ] (Array 1 Float)
   , "out_col"    ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"        ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp"    ':-> M 4 4 Float
                                    , "origin" ':-> V 4 Float
                                    ]
                          )
   , "main" ':-> EntryPoint '[ Quads ] TessellationEvaluation
   ]

tessellationEvaluation :: ShaderModule "main" TessellationEvaluationShader TessellationEvaluationDefs _
tessellationEvaluation = shader do

  ~(Vec3 u0 v0 _) <- get @"gl_TessCoord"
  mvp <- use @(Name "ubo" :.: Name "mvp")

  u <- def @"u" @R ( u0 * 6.28318530718 )
  v <- def @"v" @R ( v0 * 6.28318530718 )

  put @"out_col" =<< use @(Name "in_col" :.: Index 0)

  a <- use @(Name "in_r" :.: Index 0)
  c <- use @(Name "in_R" :.: Index 0)
  normal@(~(Vec3 nx ny _)) <- use @(Name "in_normal" :.: Index 0)

  center <- use @(Name "gl_in" :.: Index 0 :.: Name "gl_Position" )

  let
    v1, v2 :: Code (V 3 Float)
    v1 = if nx == 0
         then Vec3 1 0 0
         else normalise $ Vec3 (-ny) nx 0
    v2 = normalise $ normal `cross` v1

    Vec3 px py pz
      =   ( view @(Swizzle "xyz") center )
      ^+^ ( ( cos u * ( c + a * cos v ) ) *^ v1 )
      ^+^ ( ( sin u * ( c + a * cos v ) ) *^ v2 )
      ^+^ ( ( a * sin v ) *^ normal )

  put @"gl_Position" ( mvp !*^ Vec4 px py pz 1 )

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in_colour"  ':-> Input  '[ Location 0 ] (V 4 Float)
   , "out_colour" ':-> Output '[ Location 0 ] (V 4 Float)
   , "main"       ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
    put @"out_colour" =<< get @"in_colour"

------------------------------------------------
-- compiling

vertPath, tescPath, tesePath, fragPath :: FilePath
vertPath = shaderDir </> "hopf_vert.spv"
tescPath = shaderDir </> "hopf_tesc.spv"
tesePath = shaderDir </> "hopf_tese.spv"
fragPath = shaderDir </> "hopf_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compileTo vertPath [] vertex

compileTessellationControlShader :: IO ( Either ShortText ModuleRequirements )
compileTessellationControlShader = compileTo tescPath [] tessellationControl

compileTessellationEvaluationShader :: IO ( Either ShortText ModuleRequirements )
compileTessellationEvaluationShader = compileTo tesePath [] tessellationEvaluation

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compileTo fragPath [] fragment

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileVertexShader
  , compileTessellationControlShader
  , compileTessellationEvaluationShader
  , compileFragmentShader
  ]

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(PatchesOfSize 1)
  :>-> (vertex                , vertPath)
  :>-> (tessellationControl   , tescPath)
  :>-> (tessellationEvaluation, tesePath)
  :>-> (fragment              , fragPath)
