{-# LANGUAGE DerivingStrategies #-}

{-|
Module: SPIRV.Version

Handles SPIR-V version numbers.

-}

module SPIRV.Version
  ( Version(..), magicNo )
  where

-- base
import Data.Bits
  ( Bits(shiftL) )
import Data.Word
  ( Word8, Word32 )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )

----------------------------------------------------------------------------

data Version = Version { major :: Word8, minor :: Word8 }
  deriving stock ( Eq, Ord )

instance Show Version where
  show (Version majorVer minorVer)
    = show majorVer ++ "." ++ show minorVer

instance Put Version where
  put (Version majorVer minorVer) = put ver
    where
      ver :: Word32
      ver = (fromIntegral majorVer) `shiftL` 16
          + (fromIntegral minorVer) `shiftL` 8
  wordCount _ = 1

magicNo :: Word32
magicNo = 0x07230203
