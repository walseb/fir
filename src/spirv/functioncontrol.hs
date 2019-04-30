{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module SPIRV.FunctionControl where

-- base
import Data.Word
  ( Word32 )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known) )

------------------------------------------------------------------------------------------------

data Inlineability = Inline | DontInline
  deriving ( Eq, Show )
data SideEffects = OnlyReads| NoSideEffects
  deriving ( Eq, Show )

type FunctionControl = ( Maybe Inlineability, Maybe SideEffects )

noFunctionControl :: FunctionControl
noFunctionControl = ( Nothing, Nothing )

type NoFunctionControl = ( '( 'Nothing, 'Nothing ) :: FunctionControl )

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
