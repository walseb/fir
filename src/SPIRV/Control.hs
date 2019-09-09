{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module: SPIRV.Control

This module enumerates SPIR-V control parameters:

  * selection control parameters (SPIR-V specification §3.22 __Selection Control__),
  * loop control parameters (SPIR-V specification §3.23 __Loop Control__),
  * function control parameters (SPIR-V specification §3.24 __Function Control__).

These are used both at the type-level and value-level.

-}

module SPIRV.Control
  ( Inlineability(..)
  , SideEffects(..)
  , FunctionControl
  , type NoFunctionControl, pattern NoFunctionControl
  , SelectionControl(..)
  , LoopUnrolling(..)
  , LoopDependency(..)
  , type NoLoopControl, pattern NoLoopControl
  )
  where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Nat, KnownNat )

-- binary
import qualified Data.Binary.Put as Binary

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )

------------------------------------------------------------------------------------------------
-- function control

data Inlineability = Inline | DontInline
  deriving ( Eq, Show )
data SideEffects = OnlyReads | NoSideEffects
  deriving ( Eq, Show )

type FunctionControl = ( Maybe Inlineability, Maybe SideEffects )

type NoFunctionControl = ( '( 'Nothing, 'Nothing ) :: FunctionControl )

pattern NoFunctionControl :: FunctionControl
pattern NoFunctionControl = ( Nothing, Nothing )

instance Put FunctionControl where
  wordCount _ = 1
  put ( i, s ) = put w
    where w :: Word32
          w = wi + ws
          wi = case i of
                  Nothing         -> 0
                  Just Inline     -> 1
                  Just DontInline -> 2
          ws = case s of
                  Nothing            -> 0
                  Just OnlyReads     -> 4
                  Just NoSideEffects -> 8

instance Demotable Inlineability where
  type Demote Inlineability = Inlineability
instance Known Inlineability 'Inline where
  known = Inline
instance Known Inlineability 'DontInline where
  known = DontInline

instance Demotable SideEffects where
  type Demote SideEffects = SideEffects
instance Known SideEffects 'OnlyReads where
  known = OnlyReads
instance Known SideEffects 'NoSideEffects where
  known = NoSideEffects


------------------------------------------------------------------------------------------------
-- selection control

data SelectionControl
  = Flatten
  | DontFlatten
  deriving ( Eq, Show )

instance Put SelectionControl where
  wordCount _ = 1
  put Flatten     = put @Word32 1
  put DontFlatten = put @Word32 2

instance Put (Maybe SelectionControl) where
  wordCount _ = 1
  put Nothing            = put @Word32 0
  put (Just Flatten    ) = put @Word32 1
  put (Just DontFlatten) = put @Word32 2

instance Demotable SelectionControl where
  type Demote SelectionControl = SelectionControl
instance Known SelectionControl Flatten where
  known = Flatten
instance Known SelectionControl DontFlatten where
  known = DontFlatten

------------------------------------------------------------------------------------------------
-- loop control

data LoopUnrolling
  = Unroll
  | DontUnroll
  deriving ( Eq, Show )

data LoopDependency n
  = DependencyInfinite
  | DependencyLength n

type LoopControl n = ( Maybe LoopUnrolling, Maybe (LoopDependency n) )
type NoLoopControl = ( '( Nothing, Nothing ) :: LoopControl Nat )
pattern NoLoopControl :: LoopControl Word32
pattern NoLoopControl = ( Nothing, Nothing )

instance Demotable LoopUnrolling where
  type Demote LoopUnrolling = LoopUnrolling
instance Known LoopUnrolling Unroll where
  known = Unroll
instance Known LoopUnrolling DontUnroll where
  known = DontUnroll

instance Demotable (LoopDependency Nat) where
  type Demote (LoopDependency Nat) = LoopDependency Word32
instance Known (LoopDependency Nat) DependencyInfinite where
  known = DependencyInfinite
instance KnownNat l => Known (LoopDependency Nat) (DependencyLength l) where
  known = DependencyLength ( knownValue @l )


instance Put (LoopControl Word32) where
  wordCount (_, Just (DependencyLength _)) = 2
  wordCount _                              = 1
  put ( u, d ) = put w *> mbPutDependencyLength
    where w :: Word32
          w = wu + wd
          wu = case u of
                  Nothing         -> 0
                  Just Unroll     -> 1
                  Just DontUnroll -> 2
          wd = case d of
                  Nothing                   -> 0
                  Just DependencyInfinite   -> 4
                  Just (DependencyLength _) -> 8
          mbPutDependencyLength :: Binary.PutM ()
          mbPutDependencyLength = case d of
            Just (DependencyLength l) -> put l
            _                         -> pure ()
