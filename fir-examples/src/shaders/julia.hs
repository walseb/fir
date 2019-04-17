{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE PackageImports         #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}

module Shaders.Julia where

-- base
import GHC.TypeNats
  ( KnownNat )
import qualified Prelude

-- text-utf8
import "text-utf8" Data.Text
  ( Text )

-- vector
import qualified Data.Vector as Array

-- fir
import FIR
import FIR.Labels
import Math.Linear

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   , "main"         ':-> EntryPoint '[            ] Vertex
   ]

vertex :: Program VertexDefs ()
vertex = Program $ entryPoint @"main" @Vertex do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "out_colour"  ':-> Output     '[ Location 0      ] (V 4 Float)
   , "ubo"          ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                          ( Struct '[ "mousePos" ':-> V 2 Float ] )
   , "main"        ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

maxDepth :: AST Word32
maxDepth = 2048

complexSquare :: AST (V 2 Float) -> AST (V 2 Float)
complexSquare (Vec2 x y) = Vec2 ( x * x - y * y ) ( 2 * x * y )

gradient :: forall n. KnownNat n
         => AST Float
         -> AST (Array n (V 4 Float))
         -> AST (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: AST Float
        n = Lit . Prelude.fromIntegral $ knownValue @n
        i :: AST Word32
        i = convert ( (n-1) * t ) -- rounds down
        s :: AST Float
        s = (n-1) * t - convert i
        

sunset :: Array 9 (V 4 Float)
sunset = mkArray . Array.fromList $
       [ V4 0    0    0    1
       , V4 0.28 0.1  0.38 1
       , V4 0.58 0.2  0.38 1
       , V4 0.83 0.3  0.22 1
       , V4 0.98 0.45 0.05 1
       , V4 0.99 0.62 0.2  1
       , V4 1    0.78 0.31 1
       , V4 1    0.91 0.6  1
       , V4 1    1    1    1
       ]

fragment :: Program FragmentDefs ()
fragment = Program $ entryPoint @"main" @Fragment do

    ~(Vec4 x y _ _) <- #gl_FragCoord
    ~(Vec2 mx my) <- use @(Name "ubo" :.: Name "mousePos")



    #pos @(V 2 Float) #= Vec2 ((x-960)/400) ((y-540)/400)
    #continue         #= Lit True
    #depth @Word32    #= 0

    while #continue do
      pos   <- #pos
      depth <- #depth
      if ( pos ^.^ pos > 4 || depth >= maxDepth )
        then ( #continue .= Lit False )
        else do
          #pos   .= complexSquare pos ^+^ Vec2 ((mx-960)/400) ((my-540)/400)
          #depth .= depth + 1

    finalDepth <- #depth
    t <- def @"t" @R
        ( log ( convert finalDepth ) / log ( convert maxDepth - 1 ) :: AST Float )

    let col = gradient t (Lit sunset)

    #out_colour .= col

------------------------------------------------
-- compiling

vertPath, fragPath :: FilePath
vertPath = "src/shaders/julia_vert.spv"
fragPath = "src/shaders/julia_frag.spv"

compileVertexShader :: IO ( Either Text Text )
compileVertexShader = compile vertPath [] vertex

compileFragmentShader :: IO ( Either Text Text )
compileFragmentShader = compile fragPath [] fragment
