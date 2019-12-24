{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module FIR.Examples.Bezier.Shaders where

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
  = '[ Slot 0 0 ':-> V 3 Float
     , Slot 1 0 ':-> V 3 Float
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position" ':-> Input  '[ Location 0 ] (V 3 Float)
   , "in_colour"   ':-> Input  '[ Location 1 ] (V 3 Float)
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                         ( Struct '[ "mvp"      ':-> M 4 4 Float
                                   , "binormal" ':-> V 4 Float
                                   , "widths"   ':-> V 3 Float
                                   ]
                         )
   , "main"        ':-> EntryPoint '[ ] Vertex
   ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 r g b) <- get @"in_colour"
    ~(Vec3 x y z) <- get @"in_position"
    put @"out_colour"  (Vec4 r g b 1)
    put @"gl_Position" (Vec4 x y z 1)

------------------------------------------------
-- tessellation control

type TessellationControlDefs =
  '[ "in_col"     ':-> Input      '[ Location 0 ] (Array 5 (V 4 Float))
   , "out_col"    ':-> Output     '[ Location 0 ] (Array 5 (V 4 Float))
   , "main"       ':-> EntryPoint '[ SpacingEqual, VertexOrderCw, OutputVertices 5 ]
                        TessellationControl
   ]

tessellationControl :: ShaderModule "main" TessellationControlShader TessellationControlDefs _
tessellationControl = shader do

  assign @(Name "gl_TessLevelOuter" :.: Index 0) 1
  assign @(Name "gl_TessLevelOuter" :.: Index 1) 11

  i <- get @"gl_InvocationID"

  assign @(Name "gl_out" :.: AnIndex Word32 :.: Name "gl_Position") i
    =<< use @(Name "gl_in" :.: AnIndex Word32 :.: Name "gl_Position") i
  assign @(Name "out_col" :.: AnIndex Word32) i
    =<< use @(Name "in_col" :.: AnIndex Word32) i

------------------------------------------------
-- tessellation evaluation

type TessellationEvaluationDefs =
  '[ "in_cols" ':-> Input      '[ Location 0 ] (Array 5 (V 4 Float))
   , "out_col" ':-> Output     '[ Location 0 ] (V 4 Float)
   , "tangent" ':-> Output     '[ Location 1 ] (V 4 Float)
   , "main"    ':-> EntryPoint '[ Isolines ] TessellationEvaluation
   ]

bezier2 :: Code Float -> Code (V 4 Float) -> Code (V 4 Float) -> Code (V 4 Float) -> Code (V 4 Float)
bezier2 t u v w -- could use applicative here
  =   (            (1 - t)**2 ) *^ u
  ^+^ ( 2 * t    * (1 - t)    ) *^ v
  ^+^ (     t**2              ) *^ w

tessellationEvaluation :: ShaderModule "main" TessellationEvaluationShader TessellationEvaluationDefs _
tessellationEvaluation = shader do

  t <- use @(Name "gl_TessCoord" :.: Index 0)

  in_cols <- get @"in_cols"
  put @"out_col" $
      bezier2 t
        ( view @(Index 1) in_cols )
        ( view @(Index 2) in_cols )
        ( view @(Index 3) in_cols )


  pos0 <- use @(Name "gl_in" :.: Index 0 :.: Name "gl_Position")
  pos1 <- use @(Name "gl_in" :.: Index 1 :.: Name "gl_Position")
  pos2 <- use @(Name "gl_in" :.: Index 2 :.: Name "gl_Position")
  pos3 <- use @(Name "gl_in" :.: Index 3 :.: Name "gl_Position")
  pos4 <- use @(Name "gl_in" :.: Index 4 :.: Name "gl_Position")

  put @"gl_Position" ( bezier2 t pos1 pos2 pos3 )

  let
    tangent =
      if t < 0.5
      then (t+0.5) *^ ( pos0 ^+^ pos2 ^-^ 2 *^ pos1 ) ^+^ pos1 ^-^ pos0
      else (t-0.5) *^ ( pos2 ^+^ pos4 ^-^ 2 *^ pos3 ) ^+^ pos3 ^-^ pos2


  put @"tangent" ( normalise tangent )



------------------------------------------------
-- geometry shader

{-
miterConstant :: Code Float
miterConstant = Lit 0.8660255 -- sqrt 3 / 2, rounded up slightly

miter :: Code (V 4 Float) -> Code (V 4 Float) -> Program i i (Code (V 4 Float))
miter u v = do
  #w   #= view @(Swizzle "xyz") u `cross` view @(Swizzle "xyz") v
  #sec #= invSqrt (w `dot` w)
  ...
-}
type GeometryDefs =
  '[ "in_colors" ':-> Input   '[ Location 0 ] ( Array 2 (V 4 Float ) )
   , "tangents"  ':-> Input   '[ Location 1 ] ( Array 2 (V 4 Float ) )
   , "ubo"       ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mvp"      ':-> M 4 4 Float
                                    , "binormal" ':-> V 4 Float
                                    , "widths"   ':-> V 3 Float
                                    ]
                          )
   , "out_color" ':-> Output  '[ Location 0 ] ( V 4 Float )
   , "out_side"  ':-> Output  '[ Location 1 ] Float
   , "main"      ':-> EntryPoint
                         '[ InputLines
                          , OutputTriangleStrip, OutputVertices 4
                          , Invocations 1
                          ]
                          Geometry
   ]


geometry :: ShaderModule "main" GeometryShader GeometryDefs _
geometry = shader do
  p0   <- use @(Name "gl_in" :.: Index 0 :.: Name "gl_Position")
  p1   <- use @(Name "gl_in" :.: Index 1 :.: Name "gl_Position")
  col0 <- use @(Name "in_colors" :.: Index 0)
  col1 <- use @(Name "in_colors" :.: Index 1)

  mvp <- use @(Name "ubo" :.: Name "mvp")
  w <- use @(Name "ubo" :.: Name "widths" :.: Index 0)

  t0 <- use @(Name "tangents" :.: Index 0 :.: Swizzle "xyz")
  t1 <- use @(Name "tangents" :.: Index 1 :.: Swizzle "xyz")
  b  <- use @(Name "ubo" :.: Name "binormal" :.: Swizzle "xyz")

  let
    Vec3 nx0 ny0 nz0 = w *^ ( b  `cross` t0 )
    Vec3 nx1 ny1 nz1 = w *^ ( t1 `cross` b  )

  n0 <- def @"n0" @R ( Vec4 nx0 ny0 nz0 0 )
  n1 <- def @"n1" @R ( Vec4 nx1 ny1 nz1 0 )


  put @"out_color" col0
  put @"out_side" w
  put @"gl_Position" $ mvp !*^ (p0 ^+^ n0)
  emitVertex

  put @"out_color" col0
  put @"out_side" (-w)
  put @"gl_Position" $ mvp !*^ (p0 ^-^ n0)
  emitVertex

  put @"out_color" col1
  put @"out_side" w
  put @"gl_Position" $ mvp !*^ (p1 ^-^ n1)
  emitVertex

  put @"out_color" col1
  put @"out_side" (-w)
  put @"gl_Position" $ mvp !*^ (p1 ^+^ n1)
  emitVertex

  endPrimitive

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in_colour"   ':-> Input  '[ Location 0 ] (V 4 Float)
   , "in_side"     ':-> Input  '[ Location 1 ] Float
   , "out_colour"  ':-> Output '[ Location 0 ] (V 4 Float)
   , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                            ( Struct '[ "mvp"      ':-> M 4 4 Float
                                      , "binormal" ':-> V 4 Float
                                      , "widths"   ':-> V 3 Float
                                      ]
                            )
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

coverage :: Code Float -> Code Float
coverage d =
  if d > 0.649519
  then 0
  else if d < (-0.649519)
       then 1
       else 0.5 + d * ( 0.9123559 * d * d - 1.1547005 )

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
    ~(Vec3 _ w_effective aa_precision) <- use @(Name "ubo" :.: Name "widths")

    side <- get @"in_side"
    -- stroke of effective width w_effective,
    -- with aa radius around the edges of 1 / aa_precision
    overlap <- def @"overlap" @R ( (abs side - w_effective ) * aa_precision )
    covered <- def @"covered" @R ( coverage overlap )

    ( put @"out_colour" . over @(Index 3) ( * covered ) ) =<< get @"in_colour"


------------------------------------------------
-- compiling

vertPath, tescPath, tesePath, geomPath, fragPath :: FilePath
vertPath = shaderDir </> "bezier_vert.spv"
tescPath = shaderDir </> "bezier_tesc.spv"
tesePath = shaderDir </> "bezier_tese.spv"
geomPath = shaderDir </> "bezier_geom.spv"
fragPath = shaderDir </> "bezier_frag.spv"

compileVertexShader :: IO ( Either ShortText ModuleRequirements )
compileVertexShader = compileTo vertPath [] vertex

compileTessellationControlShader :: IO ( Either ShortText ModuleRequirements )
compileTessellationControlShader = compileTo tescPath [] tessellationControl

compileTessellationEvaluationShader :: IO ( Either ShortText ModuleRequirements )
compileTessellationEvaluationShader = compileTo tesePath [] tessellationEvaluation

compileGeometryShader :: IO ( Either ShortText ModuleRequirements )
compileGeometryShader = compileTo geomPath [] geometry

compileFragmentShader :: IO ( Either ShortText ModuleRequirements )
compileFragmentShader = compileTo fragPath [] fragment

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileVertexShader
  , compileTessellationControlShader
  , compileTessellationEvaluationShader
  , compileGeometryShader 
  , compileFragmentShader
  ]

shaderPipeline :: ShaderPipeline FilePath
shaderPipeline
  = ShaderPipeline
  $    StructInput @VertexInput @(PatchesOfSize 5)
  :>-> (vertex                , vertPath)
  :>-> (tessellationControl   , tescPath)
  :>-> (tessellationEvaluation, tesePath)
  :>-> (geometry              , geomPath)
  :>-> (fragment              , fragPath)
