{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}

module SPIRV.FunctionControl where

-- base
import Data.Word(Word32)

-- fir
import Data.Binary.Class.Put(Put(..))

------------------------------------------------------------------------------------------------

data Inlineability = Inline | DontInline
  deriving ( Eq, Show )
data SideEffects = OnlyReads| NoSideEffects
  deriving ( Eq, Show )

type FunctionControl = ( Maybe Inlineability, Maybe SideEffects )

noFunctionControl :: FunctionControl
noFunctionControl = ( Nothing, Nothing )

instance Put FunctionControl where
  sizeOf _ = 1
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

-- doing it by hand, don't feel like introducing auxiliary typeclases
class KnownFunctionControl (control :: FunctionControl) where
  functionControl :: FunctionControl

instance KnownFunctionControl '(Nothing        , Nothing           ) where
  functionControl = (Nothing        , Nothing           )

instance KnownFunctionControl '(Just Inline    , Nothing           ) where
  functionControl = (Just Inline    , Nothing           )

instance KnownFunctionControl '(Just DontInline, Nothing           ) where
  functionControl = (Just DontInline, Nothing           )

instance KnownFunctionControl '(Nothing        , Just OnlyReads    ) where
  functionControl = (Nothing        , Just OnlyReads    )

instance KnownFunctionControl '(Just Inline    , Just OnlyReads    ) where
  functionControl = (Just Inline    , Just OnlyReads    )

instance KnownFunctionControl '(Just DontInline, Just OnlyReads    ) where
  functionControl = (Just DontInline, Just OnlyReads    )

instance KnownFunctionControl '(Nothing        , Just NoSideEffects) where
  functionControl = (Nothing        , Just NoSideEffects)

instance KnownFunctionControl '(Just Inline    , Just NoSideEffects) where
  functionControl = (Just Inline    , Just NoSideEffects)

instance KnownFunctionControl '(Just DontInline, Just NoSideEffects) where
  functionControl = (Just DontInline, Just NoSideEffects)
