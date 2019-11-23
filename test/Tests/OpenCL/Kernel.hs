{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Tests.OpenCL.Kernel where

-- base
import GHC.TypeNats
  ( Nat, type (*) )

-- fir
import FIR

------------------------------------------------
-- program

type N = ( 32 :: Nat )
n :: Int32
n = fromIntegral ( knownValue @N )

type Defs = '[ "arr"  ':-> CrossWorkgroup '[] ( Array (N * N) Float )
             , "main" ':-> EntryPoint '[ LocalSize N 1 1 ] Kernel
             ]

program :: Module Defs
program = Module $ entryPoint @"main" @Kernel do
  
  ( rowIndex :: AST Int32 )
    <- convert <<$>> get @"cl_GlobalLinearID"

  _ <- def @"colIndex" @RW @Int32 0
  while ( get @"colIndex" < pure (Lit n) ) do
    colIndex <- get @"colIndex"

    _ <- def @"left" @RW @Float 1
    
    when ( colIndex > 0 ) do
      put @"left" =<< ( use @(Name "arr" :.: AnIndex Int32) ( Lit n *   rowIndex       + ( colIndex - 1 ) ) )
    
    _ <- def @"up" @RW @Float 1
    when ( rowIndex > 0 ) do
      put @"up"   =<< ( use @(Name "arr" :.: AnIndex Int32) ( Lit n * ( rowIndex - 1 ) +   colIndex       ) )

    left <- get @"left"
    up   <- get @"up"
    assign @(Name "arr" :.: AnIndex Int32) ( Lit n * rowIndex + colIndex ) ( left + up )

    memoryBarrier Device ( Just $ MemorySemantics (Lock [Acquire]) [ CrossWorkgroupMemory ] )

    modify @"colIndex" (+1)

  pure (Lit ())
