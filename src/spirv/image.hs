{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module SPIRV.Image where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Binary.Class.Put
  ( Put(..), PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known) )
import SPIRV.ScalarTy
  ( ScalarTy, Signedness(..) )

--------------------------------------------------

data ImageTy
  = ImageTy
      { component      :: ScalarTy -- called the 'Sampled type' in SPIR-V
      , dimensionality :: Dimensionality
      , depth          :: Maybe Depth
      , arrayness      :: Arrayness
      , multiSampling  :: MultiSampling
      , imageUsage     :: Maybe ImageUsage
      , imageFormat    :: ImageFormat Word32
      }
  deriving ( Eq, Show, Ord )

data Dimensionality
  = OneD
  | TwoD
  | ThreeD
  | Cube
  | Rect
  | Buffer
  | SubpassData
  deriving ( Eq, Show, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Dimensionality)

instance Demotable Dimensionality where
  type Demote Dimensionality = Dimensionality
instance Known Dimensionality 'OneD where
  known = OneD
instance Known Dimensionality 'TwoD where
  known = TwoD
instance Known Dimensionality 'ThreeD where
  known = ThreeD
instance Known Dimensionality 'Cube where
  known = Cube
instance Known Dimensionality 'Rect where
  known = Rect
instance Known Dimensionality 'Buffer where
  known = Buffer
instance Known Dimensionality 'SubpassData where
  known = SubpassData

data Depth
  = NoDepth
  | Depth
  deriving ( Eq, Show, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Depth)

instance Demotable Depth where
  type Demote Depth = Depth
instance Known Depth 'NoDepth where
  known = NoDepth
instance Known Depth 'Depth where
  known = Depth

instance Put (Maybe Depth) where
  put (Just d) = put         d
  put Nothing  = put @Word32 2
  sizeOf _ = 1

data Arrayness
  = NonArrayed
  | Arrayed
  deriving ( Eq, Show, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Arrayness)

instance Demotable Arrayness where
  type Demote Arrayness = Arrayness
instance Known Arrayness 'NonArrayed where
  known = NonArrayed
instance Known Arrayness 'Arrayed where
  known = Arrayed

data MultiSampling
  = SingleSampled
  | MultiSampled
  deriving ( Eq, Show, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum MultiSampling)

instance Demotable MultiSampling where
  type Demote MultiSampling = MultiSampling
instance Known MultiSampling 'SingleSampled where
  known = SingleSampled
instance Known MultiSampling 'MultiSampled where
  known = MultiSampled

data ImageUsage
  = Sampled
  | Storage
  deriving ( Eq, Show, Ord, Enum, Bounded )

instance Put (Maybe ImageUsage) where
  put Nothing        = put @Word32 0
  put (Just Sampled) = put @Word32 1
  put (Just Storage) = put @Word32 2
  sizeOf _ = 1

instance Demotable ImageUsage where
  type Demote ImageUsage = ImageUsage
instance Known ImageUsage 'Sampled where
  known = Sampled
instance Known ImageUsage 'Storage where
  known = Storage

data Normalisation
  = Unnormalised
  | Normalised
  deriving ( Eq, Show, Ord )

data Component
  = Integer Normalisation Signedness
  | Floating
  deriving ( Eq, Show, Ord )

pattern SNorm :: Component
pattern SNorm = Integer Normalised Signed
type SNorm = ( 'Integer 'Normalised 'Signed :: Component )

pattern UNorm :: Component
pattern UNorm = Integer Normalised Unsigned
type UNorm = ( 'Integer 'Normalised 'Unsigned :: Component )

pattern F :: Component
pattern F = Floating
type F = ( 'Floating :: Component )

pattern I :: Component
pattern I = Integer Unnormalised Signed
type I = ( 'Integer 'Unnormalised 'Signed :: Component )

pattern UI :: Component
pattern UI = Integer Unnormalised Unsigned
type UI = ( 'Integer 'Unnormalised 'Unsigned :: Component )

data ImageFormat a
  = ImageFormat Component [a]
  deriving ( Eq, Show, Ord )



pattern RGBA32 :: Component -> ImageFormat Word32
pattern RGBA32 component = ImageFormat component [32,32,32,32]
type RGBA32 (component :: Component) = ( 'ImageFormat component '[32,32,32,32] :: ImageFormat Nat )

pattern RGBA16 :: Component -> ImageFormat Word32
pattern RGBA16 component = ImageFormat component [16,16,16,16]
type RGBA16 (component :: Component) = ( 'ImageFormat component '[16,16,16,16] :: ImageFormat Nat )

pattern RGBA8 :: Component -> ImageFormat Word32
pattern RGBA8 component = ImageFormat component [8,8,8,8]
type RGBA8 (component :: Component) = ( 'ImageFormat component '[8,8,8,8] :: ImageFormat Nat )

pattern RG32 :: Component -> ImageFormat Word32
pattern RG32 component = ImageFormat component [32,32]
type RG32 (component :: Component) = ( 'ImageFormat component '[32,32] :: ImageFormat Nat )

pattern RG16 :: Component -> ImageFormat Word32
pattern RG16 component = ImageFormat component [16,16]
type RG16 (component :: Component) = ( 'ImageFormat component '[16,16] :: ImageFormat Nat )

pattern RG8 :: Component -> ImageFormat Word32
pattern RG8 component = ImageFormat component [8,8]
type RG8 (component :: Component) = ( 'ImageFormat component '[8,8] :: ImageFormat Nat )

pattern R32 :: Component -> ImageFormat Word32
pattern R32 component = ImageFormat component [32]
type R32 (component :: Component) = ( 'ImageFormat component '[32] :: ImageFormat Nat )

pattern R16 :: Component -> ImageFormat Word32
pattern R16 component = ImageFormat component [16]
type R16 (component :: Component) = ( 'ImageFormat component '[16] :: ImageFormat Nat )

pattern R8 :: Component -> ImageFormat Word32
pattern R8 component = ImageFormat component [8]
type R8 (component :: Component) = ( 'ImageFormat component '[8] :: ImageFormat Nat )

pattern R11G11B10 :: Component -> ImageFormat Word32
pattern R11G11B10 component = ImageFormat component [11,11,10]
type R11G11B10 (component :: Component) = ( 'ImageFormat component '[11,11,10] :: ImageFormat Nat )

pattern RGB10A2 :: Component -> ImageFormat Word32
pattern RGB10A2 component = ImageFormat component [10,10,10,2]
type RGB10A2 (component :: Component) = ( 'ImageFormat component '[10,10,10,2] :: ImageFormat Nat )

instance Demotable (ImageFormat Nat) where
  type Demote (ImageFormat Nat) = ImageFormat Word32

instance Known (ImageFormat Nat) (RGBA32    F     ) where
  known = RGBA32    F
instance Known (ImageFormat Nat) (RGBA16    F     ) where
  known = RGBA16    F
instance Known (ImageFormat Nat) (R32       F     ) where
  known = R32       F
instance Known (ImageFormat Nat) (RGBA8     UNorm ) where
  known = RGBA8     UNorm
instance Known (ImageFormat Nat) (RGBA8     SNorm ) where
  known = RGBA8     SNorm
instance Known (ImageFormat Nat) (RG32      F     ) where
  known = RG32      F
instance Known (ImageFormat Nat) (RG16      F     ) where
  known = RG16      F
instance Known (ImageFormat Nat) (R11G11B10 F     ) where
  known = R11G11B10 F
instance Known (ImageFormat Nat) (R16       F     ) where
  known = R16       F
instance Known (ImageFormat Nat) (RGBA16    UNorm ) where
  known = RGBA16    UNorm
instance Known (ImageFormat Nat) (RGB10A2   UNorm ) where
  known = RGB10A2   UNorm
instance Known (ImageFormat Nat) (RG16      UNorm ) where
  known = RG16      UNorm
instance Known (ImageFormat Nat) (RG8       UNorm ) where
  known = RG8       UNorm
instance Known (ImageFormat Nat) (R16       UNorm ) where
  known = R16       UNorm
instance Known (ImageFormat Nat) (R8        UNorm ) where
  known = R8        UNorm
instance Known (ImageFormat Nat) (RGBA16    SNorm ) where
  known = RGBA16    SNorm
instance Known (ImageFormat Nat) (RG16      SNorm ) where
  known = RG16      SNorm
instance Known (ImageFormat Nat) (RG8       SNorm ) where
  known = RG8       SNorm
instance Known (ImageFormat Nat) (R16       SNorm ) where
  known = R16       SNorm
instance Known (ImageFormat Nat) (R8        SNorm ) where
  known = R8        SNorm
instance Known (ImageFormat Nat) (RGBA32    I     ) where
  known = RGBA32    I
instance Known (ImageFormat Nat) (RGBA16    I     ) where
  known = RGBA16    I
instance Known (ImageFormat Nat) (RGBA8     I     ) where
  known = RGBA8     I
instance Known (ImageFormat Nat) (R32       I     ) where
  known = R32       I
instance Known (ImageFormat Nat) (RG32      I     ) where
  known = RG32      I
instance Known (ImageFormat Nat) (RG16      I     ) where
  known = RG16      I
instance Known (ImageFormat Nat) (RG8       I     ) where
  known = RG8       I
instance Known (ImageFormat Nat) (R16       I     ) where
  known = R16       I
instance Known (ImageFormat Nat) (R8        I     ) where
  known = R8        I
instance Known (ImageFormat Nat) (RGBA32    UI    ) where
  known = RGBA32    UI
instance Known (ImageFormat Nat) (RGBA16    UI    ) where
  known = RGBA16    UI
instance Known (ImageFormat Nat) (RGBA8     UI    ) where
  known = RGBA8     UI
instance Known (ImageFormat Nat) (R32       UI    ) where
  known = R32       UI
instance Known (ImageFormat Nat) (RGB10A2   UI    ) where
  known = RGB10A2   UI
instance Known (ImageFormat Nat) (RG32      UI    ) where
  known = RG32      UI
instance Known (ImageFormat Nat) (RG16      UI    ) where
  known = RG16      UI
instance Known (ImageFormat Nat) (RG8       UI    ) where
  known = RG8       UI
instance Known (ImageFormat Nat) (R16       UI    ) where
  known = R16       UI
instance Known (ImageFormat Nat) (R8        UI    ) where
  known = R8        UI


fromFormat :: ImageFormat Word32 -> Maybe Word32
fromFormat ( RGBA32    F     ) = Just  1
fromFormat ( RGBA16    F     ) = Just  2
fromFormat ( R32       F     ) = Just  3
fromFormat ( RGBA8     UNorm ) = Just  4
fromFormat ( RGBA8     SNorm ) = Just  5
fromFormat ( RG32      F     ) = Just  6
fromFormat ( RG16      F     ) = Just  7
fromFormat ( R11G11B10 F     ) = Just  8
fromFormat ( R16       F     ) = Just  9
fromFormat ( RGBA16    UNorm ) = Just 10
fromFormat ( RGB10A2   UNorm ) = Just 11
fromFormat ( RG16      UNorm ) = Just 12
fromFormat ( RG8       UNorm ) = Just 13
fromFormat ( R16       UNorm ) = Just 14
fromFormat ( R8        UNorm ) = Just 15
fromFormat ( RGBA16    SNorm ) = Just 16
fromFormat ( RG16      SNorm ) = Just 17
fromFormat ( RG8       SNorm ) = Just 18
fromFormat ( R16       SNorm ) = Just 19
fromFormat ( R8        SNorm ) = Just 20
fromFormat ( RGBA32    I     ) = Just 21
fromFormat ( RGBA16    I     ) = Just 22
fromFormat ( RGBA8     I     ) = Just 23
fromFormat ( R32       I     ) = Just 24
fromFormat ( RG32      I     ) = Just 25
fromFormat ( RG16      I     ) = Just 26
fromFormat ( RG8       I     ) = Just 27
fromFormat ( R16       I     ) = Just 28
fromFormat ( R8        I     ) = Just 29
fromFormat ( RGBA32    UI    ) = Just 30
fromFormat ( RGBA16    UI    ) = Just 31
fromFormat ( RGBA8     UI    ) = Just 32
fromFormat ( R32       UI    ) = Just 33
fromFormat ( RGB10A2   UI    ) = Just 34
fromFormat ( RG32      UI    ) = Just 35
fromFormat ( RG16      UI    ) = Just 36
fromFormat ( RG8       UI    ) = Just 37
fromFormat ( R16       UI    ) = Just 38
fromFormat ( R8        UI    ) = Just 39
fromFormat _                   = Nothing

instance Put (ImageFormat Word32) where
  put format = case fromFormat format of
    Just i  -> put         i
    Nothing -> put @Word32 0
  sizeOf _ = 1

instance Put (Maybe (ImageFormat Word32)) where
  put (Just format) = put         format
  put Nothing       = put @Word32 0
  sizeOf _ = 1
