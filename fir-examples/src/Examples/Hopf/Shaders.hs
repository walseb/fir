{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedWildCards         #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Examples.Hopf.Shaders where

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- pipeline input

type RawVertexInput
  = '[ Slot 0 0 ':-> V 3 Float, Slot 0 3 ':-> Float -- pos, R
     , Slot 1 0 ':-> V 3 Float, Slot 1 3 ':-> Float -- normal, r
     , Slot 2 0 ':-> V 4 Float                      -- col
     ]

type VertexInput
  = '[ Slot 0 0 ':-> V 4 Float -- pos & R
     , Slot 1 0 ':-> V 4 Float -- normal &r
     , Slot 2 0 ':-> V 4 Float -- col
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_pos_R"     ':-> Input  '[ Location 0 ] (V 4 Float)
   , "in_normal_r"  ':-> Input  '[ Location 1 ] (V 4 Float)
   , "in_colour"    ':-> Input  '[ Location 2 ] (V 4 Float)
   
   , "out_colour"   ':-> Output '[ Location 0 ] (V 4 Float)
   , "out_R"        ':-> Output '[ Location 1 ] Float
   , "out_normal"   ':-> Output '[ Location 2 ] (V 3 Float)
   , "out_r"        ':-> Output '[ Location 3 ] Float

   , "main"         ':-> EntryPoint '[ ] Vertex
   ]

vertex :: ShaderStage "main" VertexShader VertexDefs _
vertex = shader do

    put @"out_colour" =<< get @"in_colour"

    ~(Vec4 px py pz big_R) <- get @"in_pos_R"
    put @"gl_Position" ( Vec4 px py pz 1 )

    ~(Vec4 nx ny nz small_r) <- get @"in_normal_r"
    put @"out_normal" ( Vec3 nx ny nz )

    put @"out_R" big_R
    put @"out_r" small_r

------------------------------------------------
-- tessellation control

type TessellationControlDefs =
  '[ "in_col"     ':-> Input  '[ Location 0 ] (Array 1 (V 4 Float))
   , "in_R"       ':-> Input  '[ Location 1 ] (Array 1 Float)
   , "in_normal"  ':-> Input  '[ Location 2 ] (Array 1 (V 3 Float))
   , "in_r"       ':-> Input  '[ Location 3 ] (Array 1 Float)
 
   , "out_col"    ':-> Output '[ Location 0 ] (Array 1 (V 4 Float))
   , "out_R"      ':-> Output '[ Location 1 ] (Array 1 Float)
   , "out_normal" ':-> Output '[ Location 2 ] (Array 1 (V 3 Float))
   , "out_r"      ':-> Output '[ Location 3 ] (Array 1 Float)
   , "main"       ':-> EntryPoint '[ SpacingEqual, VertexOrderCw, OutputVertices 1 ]
                        TessellationControl
   ]

tessellationControl :: ShaderStage "main" TessellationControlShader TessellationControlDefs _
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
   , "in_r"       ':-> Input  '[ Location 3 ] (Array 1 Float)
   , "out_col"    ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"        ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp"    ':-> M 4 4 Float
                                    , "origin" ':-> V 4 Float
                                    ]
                          )
   , "main" ':-> EntryPoint '[ Quads ] TessellationEvaluation
   ]

tessellationEvaluation :: ShaderStage "main" TessellationEvaluationShader TessellationEvaluationDefs _
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
    v1, v2 :: AST (V 3 Float)
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

fragment :: ShaderStage "main" FragmentShader FragmentDefs _
fragment = shader do
    put @"out_colour" =<< get @"in_colour"

------------------------------------------------
-- compiling

vertPath, tescPath, tesePath, fragPath :: FilePath
vertPath = "shaders/hopf_vert.spv"
tescPath = "shaders/hopf_tesc.spv"
tesePath = "shaders/hopf_tese.spv"
fragPath = "shaders/hopf_frag.spv"

compileVertexShader :: IO ( Either ShortText () )
compileVertexShader = compile vertPath [] vertex

compileTessellationControlShader :: IO ( Either ShortText () )
compileTessellationControlShader = compile tescPath [] tessellationControl

compileTessellationEvaluationShader :: IO ( Either ShortText () )
compileTessellationEvaluationShader = compile tesePath [] tessellationEvaluation

compileFragmentShader :: IO ( Either ShortText () )
compileFragmentShader = compile fragPath [] fragment

shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(PatchesOfSize 1)
  $    StartPipeline
  :>-> (vertex                , vertPath)
  :>-> (tessellationControl   , tescPath)
  :>-> (tessellationEvaluation, tesePath)
  :>-> (fragment              , fragPath)
