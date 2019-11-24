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
  ( Nat, type (*), type (-) )

-- fir
import FIR

------------------------------------------------
-- program

type N = ( 128 :: Nat )
n :: AST Int32
n = Lit $ fromIntegral ( knownValue @N )

type Defs = '[ "arr"  ':-> CrossWorkgroup '[] ( Array (N * N) Float )
             , "main" ':-> EntryPoint '[ LocalSize (2 * N - 1) 1 1 ] Kernel
             ]

program :: Module Defs
program = Module $ entryPoint @"main" @Kernel do
  
  -- 0 ≤ diag < 2 * n - 1
  ( diag :: AST Int32 )
    <- convert <<$>> get @"cl_GlobalLinearID"

  _ <- def @"entry" @RW @Int32 0
  let
    diagSize :: AST Int32
    diagSize = n - abs ( n - diag - 1 )
  while ( get @"entry" < pure diagSize ) do
    entry <- get @"entry"

    let
      row, col :: AST Int32
      row = min ( n - 1 )   diag           - entry
      col = max   0       ( diag - n + 1 ) + entry

    _ <- def @"left" @RW @Float 1
    when ( col > 0 ) do
      put @"left" =<< ( use @(Name "arr" :.: AnIndex Int32) ( n *   row      + ( col - 1 ) ) )
    
    _ <- def @"up" @RW @Float 1
    when ( row > 0 ) do
      put @"up"   =<< ( use @(Name "arr" :.: AnIndex Int32) ( n * ( row - 1 ) +  col       ) )

    left <- get @"left"
    up   <- get @"up"
    assign @(Name "arr" :.: AnIndex Int32) ( n * row + col ) ( left + up )

    modify @"entry" (+1)

  pure (Lit ())
