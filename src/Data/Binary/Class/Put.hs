{-# LANGUAGE TypeApplications #-}

{-|
Module: Data.Binary.Class.Put

One half of the 'Data.Binary.Binary' type class: putting.

This type class is used to provide arguments to @SPIR-V@
instructions; everything comes in multiples of 'Word32'.

The instances provided in this module are /little endian/,
in agreement with the default endianness for @SPIR-V@ assembly.

Note: this type class is __not__ concerned with memory layout on the GPU.
GPU memory layout is managed using @SPIR-V@ decorations, in conjunction with @Vulkan@.

-}

module Data.Binary.Class.Put
  (
    -- * Put type class
    Put(..)
    -- ** Instances
    -- $instances
  , PutWord32Enum(..)
  ) where

-- base
import Data.Int
  ( Int8,Int16,Int32,Int64 )
import Data.Semigroup
  ( stimes )
import Data.Word
  ( Word8,Word16,Word32,Word64 )

-- binary
import qualified Data.Binary     as Binary
  ( put )
import qualified Data.Binary.Put as Binary
  ( PutM
  , putWord8
  , putWord32le, putWord64le
  , putInt32le, putInt64le
  , putShortByteString
  )

-- data-binary-ieee754
import qualified Data.Binary.IEEE754 as Binary.IEEE754
  ( putFloat32le, putFloat64le )

-- half
import Numeric.Half
  ( Half )
import qualified Numeric.Half as Half
  ( fromHalf )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( length, toShortByteString )

----------------------------------------------------------------------------
-- Put type class

-- | One half of 'Data.Binary.Binary'.
class Put a where
  put    :: a -> Binary.PutM ()
  -- | Memory footprint, as a multiple of __32 bits__
  -- (/not/ the usual 8 bits as in 'Foreign.Storable.sizeOf').
  wordCount :: a -> Word32

---------------------------------------------------------------------------
-- $instances
--
-- Little-endian instances are provided for 'Put',
-- as these are best suited to @SPIR-V@.
--
-- Note that types smaller than 32 bits are immediately padded:
-- this type class is purely used for specifying arguments
-- to @SPIR-V@ instructions, and the smallest allowed size is 32 bits;
-- it is not possible to tightly pack arguments of smaller sizes.

instance Put Word8 where
  put = Binary.putWord32le . fromIntegral
  wordCount _ = 1

instance Put Word16 where
  put = Binary.putWord32le . fromIntegral
  wordCount _ = 1

instance Put Word32 where
  put = Binary.putWord32le
  wordCount _ = 1

instance Put Word64 where
  put = Binary.putWord64le
  wordCount _ = 2

instance Put Int8 where
  put = Binary.putInt32le . fromIntegral
  wordCount _ = 1

instance Put Int16 where
  put = Binary.putInt32le . fromIntegral
  wordCount _ = 1

instance Put Int32 where
  put = Binary.putInt32le
  wordCount _ = 1

instance Put Int64 where
  put = Binary.putInt64le
  wordCount _ = 2

instance Put Half where
  put = Binary.put . Half.fromHalf
  wordCount _ = 1

instance Put Float where
  put = Binary.IEEE754.putFloat32le
  wordCount _ = 1

instance Put Double where
  put = Binary.IEEE754.putFloat64le
  wordCount _ = 2

-- | @C@-style string
instance Put ShortText where
  put lit =
    let n = ShortText.length lit
        pad = 4 - (n `mod` 4)
    in Binary.putShortByteString (ShortText.toShortByteString lit)
    <> (pad `stimes` Binary.putWord8 0)
  wordCount lit
    = let n = fromIntegral $ ShortText.length lit
      in 1 + (n `div` 4)

----------------------------------------------------------------------------
-- | Newtype for 'Put' instance using 'Enum'.
--
-- Useful with deriving via:
-- @ data ... deriving Put via (PutWord32Enum ...) @.
newtype PutWord32Enum a = PutWord32Enum { runPutWord32Enum :: a }

instance Enum a => Put (PutWord32Enum a) where
  put = put @Word32 . fromIntegral . fromEnum . runPutWord32Enum
  wordCount _ = 1
