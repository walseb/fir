module Data.Binary.Class.Put where

-- base
import Data.Int(Int8,Int16,Int32,Int64)
import Data.Semigroup(stimes)
import Data.Word(Word8,Word16,Word32,Word64)

-- binary
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary

-- bytestring
import qualified Data.ByteString as ByteString

-- data-binary-ieee754
import qualified Data.Binary.IEEE754 as Binary.IEEE754

-- half
import Numeric.Half(Half)
import qualified Numeric.Half as Half

-- text-utf8
import Data.Text(Text)
import qualified Data.Text.Encoding as Text

----------------------------------------------------------------------------
-- Put typeclass

class Put a where
  put    :: a -> Binary.PutM ()
  sizeOf :: a -> Word32 -- size in bytes

-- little endian instances as suitable for SPIR-V

instance Put Word8 where
  put = Binary.putWord32le . fromIntegral
  sizeOf _ = 1

instance Put Word16 where
  put = Binary.putWord32le . fromIntegral
  sizeOf _ = 1

instance Put Word32 where
  put = Binary.putWord32le
  sizeOf _ = 1

instance Put Word64 where
  put = Binary.putWord64le
  sizeOf _ = 2

instance Put Word where
  put = Binary.put
  sizeOf _ = 2

instance Put Int8 where
  put = Binary.putInt32le . fromIntegral
  sizeOf _ = 1

instance Put Int16 where
  put = Binary.putInt32le . fromIntegral
  sizeOf _ = 1

instance Put Int32 where
  put = Binary.putInt32le
  sizeOf _ = 1

instance Put Int64 where
  put = Binary.putInt64le
  sizeOf _ = 2

instance Put Int where
  put = Binary.put
  sizeOf _ = 2

instance Put Half where
  put = Binary.put . Half.fromHalf
  sizeOf _ = 1

instance Put Float where
  put = Binary.IEEE754.putFloat32le
  sizeOf _ = 1

instance Put Double where
  put = Binary.IEEE754.putFloat64le
  sizeOf _ = 2

-- C-style string
instance Put Text where
  put lit =
    let bs = Text.encodeUtf8 lit
        n = ByteString.length bs
        pad = 4 - (n `mod` 4)
    in Binary.putByteString bs
    <> (pad `stimes` Binary.putWord8 0)
  sizeOf lit
    = let n = fromIntegral $ ByteString.length ( Text.encodeUtf8 lit )
      in 1 + (n `div` 4)