{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Tests.VertexInput.EquivalentComponents where

-- fir
import FIR
import Math.Linear

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float, Slot 0 3 ':-> Float -- split1
     , Slot 1 0 ':-> V 3 Float, Slot 1 3 ':-> Float -- split2
     , Slot 2 0 ':-> V 4 Float                      -- nonsplit1
     , Slot 3 0 ':-> V 4 Float                      -- nonsplit2
     ]

------------------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_split1a"    ':-> Input '[ Location 0 ]              ( V 3 Float )
   , "in_split1b"    ':-> Input '[ Location 0, Component 3 ] Float
   , "in_split2"     ':-> Input '[ Location 1 ]              ( V 4 Float )
   , "in_nonsplit1a" ':-> Input '[ Location 2 ]              ( V 3 Float )
   , "in_nonsplit1b" ':-> Input '[ Location 2, Component 3 ] Float
   , "in_nonsplit2"  ':-> Input '[ Location 3 ]              ( V 4 Float )

   , "out_split1a"    ':-> Output '[ Location 0 ]              ( V 3 Float )
   , "out_split1b"    ':-> Output '[ Location 0, Component 3 ] Float
   , "out_split2"     ':-> Output '[ Location 1 ]              ( V 4 Float )
   , "out_nonsplit1a" ':-> Output '[ Location 2 ]              ( V 3 Float )
   , "out_nonsplit1b" ':-> Output '[ Location 2, Component 3 ] Float
   , "out_nonsplit2"  ':-> Output '[ Location 3 ]              ( V 4 Float )

   , "main"           ':-> EntryPoint '[ ] Vertex
   ]

vertex :: ShaderStage "main" VertexShader VertexDefs _
vertex = shader do

  put @"out_split1a"    =<< get @"in_split1a"
  put @"out_split1b"    =<< get @"in_split1b"
  put @"out_split2"     =<< get @"in_split2"
  put @"out_nonsplit1a" =<< get @"in_nonsplit1a"
  put @"out_nonsplit1b" =<< get @"in_nonsplit1b"
  put @"out_nonsplit2"  =<< get @"in_nonsplit2"

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "in_split1a"    ':-> Input  '[ Location 0 ]              ( V 3 Float )
   , "in_split1b"    ':-> Input  '[ Location 0, Component 3 ] Float
   , "in_split2"     ':-> Input  '[ Location 1 ]              ( V 4 Float )
   , "in_nonsplit1a" ':-> Input  '[ Location 2 ]              ( V 3 Float )
   , "in_nonsplit1b" ':-> Input  '[ Location 2, Component 3 ] Float
   , "in_nonsplit2"  ':-> Input  '[ Location 3 ]              ( V 4 Float )
   , "out_colour"    ':-> Output '[ Location 0 ]              ( V 4 Float )
   , "main"          ':-> EntryPoint '[ OriginUpperLeft ] Fragment
   ]

fragment :: ShaderStage "main" FragmentShader FragmentDefs _
fragment = shader do
    put @"out_colour" =<< get @"in_split2"

------------------------------------------------
-- pipeline

shaderPipeline :: ShaderPipeline
shaderPipeline
  = withStructInput @VertexInput @(Triangle List)
  $    StartPipeline
  :>-> (vertex  ,   "vertex.spv")
  :>-> (fragment, "fragment.spv")
