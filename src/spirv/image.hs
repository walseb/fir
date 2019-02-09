{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module SPIRV.Image where

-- base
import Prelude hiding
  ( Integral, Floating )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat )

-- fir
import Data.Binary.Class.Put
  ( Put(..), PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import SPIRV.ScalarTy
  ( ScalarTy, Signedness(..) )

--------------------------------------------------

data Image
  = Image
      { component      :: ScalarTy -- called the 'Sampled type' in SPIR-V
      , dimensionality :: Dimensionality
      , hasDepth       :: Maybe HasDepth
      , arrayness      :: Arrayness
      , multiSampling  :: MultiSampling
      , imageUsage     :: Maybe ImageUsage
      , imageFormat    :: Maybe (ImageFormat Word32)
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

data HasDepth
  = NotDepthImage
  | DepthImage
  deriving ( Eq, Show, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum HasDepth)

instance Demotable HasDepth where
  type Demote HasDepth = HasDepth
instance Known HasDepth NotDepthImage where
  known = NotDepthImage
instance Known HasDepth DepthImage where
  known = DepthImage

instance Put (Maybe HasDepth) where
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
  deriving ( Eq, Show, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum Normalisation)

instance Demotable Normalisation where
  type Demote Normalisation = Normalisation
instance Known Normalisation Unnormalised where
  known = Unnormalised
instance Known Normalisation Normalised where
  known = Normalised

data Component
  = Integer Normalisation Signedness
  | Floating
  deriving ( Eq, Show, Ord )

instance Demotable Component where
  type Demote Component = Component
instance ( Known Normalisation norm
         , Known Signedness    sign
         ) 
      => Known Component ('Integer norm sign)
      where
  known = Integer (knownValue @norm) (knownValue @sign)
instance Known Component 'Floating where
  known = Floating

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
type family RGBA32 (component :: Component) :: ImageFormat Nat where
  RGBA32 SNorm = TypeError ( Text "RGBA32 format cannot use normalised integers." )
  RGBA32 UNorm = TypeError ( Text "RGBA32 format cannot use normalised integers." )
  RGBA32 comp  = 'ImageFormat comp '[32,32,32,32] 

pattern RGBA16 :: Component -> ImageFormat Word32
pattern RGBA16 component = ImageFormat component [16,16,16,16]
type RGBA16 (component :: Component) = ( 'ImageFormat component '[16,16,16,16] :: ImageFormat Nat )

pattern RGBA8 :: Component -> ImageFormat Word32
pattern RGBA8 component = ImageFormat component [8,8,8,8]
type family RGBA8 (component :: Component) :: ImageFormat Nat where
  RGBA8 F    = TypeError ( Text "RGBA8 format cannot use floating-point numbers." )
  RGBA8 comp = 'ImageFormat comp '[8,8,8,8]

pattern RG32 :: Component -> ImageFormat Word32
pattern RG32 component = ImageFormat component [32,32]
type family RG32 (component :: Component) :: ImageFormat Nat where
  RG32 SNorm = TypeError ( Text "RG32 format cannot use normalised integers." )
  RG32 UNorm = TypeError ( Text "RG32 format cannot use normalised integers." )
  RG32 comp  = 'ImageFormat comp '[32,32] 

pattern RG16 :: Component -> ImageFormat Word32
pattern RG16 component = ImageFormat component [16,16]
type RG16 (component :: Component) = ( 'ImageFormat component '[16,16] :: ImageFormat Nat )

pattern RG8 :: Component -> ImageFormat Word32
pattern RG8 component = ImageFormat component [8,8,8,8]
type family RG8 (component :: Component) :: ImageFormat Nat where
  RG8 F    = TypeError ( Text "RG8 format cannot use floating-point numbers." )
  RG8 comp = 'ImageFormat comp '[8,8]

pattern R32 :: Component -> ImageFormat Word32
pattern R32 component = ImageFormat component [32]
type family R32 (component :: Component) :: ImageFormat Nat where
  R32 SNorm = TypeError ( Text "R32 format cannot use normalised integers." )
  R32 UNorm = TypeError ( Text "R32 format cannot use normalised integers." )
  R32 comp  = 'ImageFormat comp '[32] 

pattern R16 :: Component -> ImageFormat Word32
pattern R16 component = ImageFormat component [16]
type R16 (component :: Component) = ( 'ImageFormat component '[16] :: ImageFormat Nat )

pattern R8 :: Component -> ImageFormat Word32
pattern R8 component = ImageFormat component [8]
type family R8 (component :: Component) :: ImageFormat Nat where
  R8 F    = TypeError ( Text "R8 format cannot use floating-point numbers." )
  R8 comp = 'ImageFormat comp '[8]

pattern R11G11B10 :: Component -> ImageFormat Word32
pattern R11G11B10 component = ImageFormat component [11,11,10]
type family R11G11B10 (component :: Component) :: ImageFormat Nat where
  R11G11B10 F    = 'ImageFormat F '[11,11,10]
  R11G11B10 comp = TypeError ( Text "R11G11B10 format must use floating-point numbers." )

pattern RGB10A2 :: Component -> ImageFormat Word32
pattern RGB10A2 component = ImageFormat component [10,10,10,2]
type family RGB10A2 (component :: Component) :: ImageFormat Nat where
  RGB10A2 UI    = 'ImageFormat UI    '[10,10,10,2]
  RGB10A2 UNorm = 'ImageFormat UNorm '[10,10,10,2]
  RGB10A2 comp  = TypeError ( Text "RGB10A2 format must use unsigned integers." )

instance Demotable (ImageFormat Nat) where
  type Demote (ImageFormat Nat) = ImageFormat Word32
instance ( Known Component comp
         , Known [Nat] sizes
         )
       => Known (ImageFormat Nat) ('ImageFormat comp sizes)
     where
  known = ImageFormat (knownValue @comp) (knownValue @sizes)


type family RequiredFormatUsage ( fmt :: ImageFormat Nat ) :: Maybe ImageUsage where
  RequiredFormatUsage ('ImageFormat _                       '[8,8,8,8]) = Nothing
  RequiredFormatUsage ('ImageFormat ('Integer Normalised _) '[i,i,i,i]) = Just Storage
  RequiredFormatUsage ('ImageFormat _                       '[i,i,i,i]) = Nothing
  RequiredFormatUsage ('ImageFormat _                       '[32]     ) = Just Storage
  RequiredFormatUsage _                                                 = Just Storage

requiredFormatUsage :: ImageFormat Word32 -> Maybe ImageUsage
requiredFormatUsage (ImageFormat _                      [8,8,8,8]) = Nothing
requiredFormatUsage (ImageFormat (Integer Normalised _) [_,_,_,_]) = Just Storage
requiredFormatUsage (ImageFormat _                      [i,_,_,l]) = if i == l then Nothing else Just Storage
requiredFormatUsage (ImageFormat _                      [32]     ) = Just Storage
requiredFormatUsage _                                              = Just Storage


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

data SamplerAddressing
  = ClampToEdge
  | Clamp
  | Repeat
  | RepeatMirrored
  deriving ( Show, Eq, Ord, Enum, Bounded )

instance Put SamplerAddressing where
  put = put @Word32 . fromIntegral . succ . fromEnum
  sizeOf _ = 1

instance Put (Maybe SamplerAddressing) where
  put (Just addr) = put         addr
  put Nothing     = put @Word32 0
  sizeOf _ = 1

data FilterMode
  = Nearest
  | Linear
  deriving ( Show, Eq, Ord, Enum, Bounded )
  deriving Put via (PutWord32Enum FilterMode)

instance Demotable SamplerAddressing where
  type Demote SamplerAddressing = SamplerAddressing
instance Known SamplerAddressing ClampToEdge where
  known = ClampToEdge
instance Known SamplerAddressing Clamp where
  known = Clamp
instance Known SamplerAddressing Repeat where
  known = Repeat
instance Known SamplerAddressing RepeatMirrored   where
  known = RepeatMirrored

instance Demotable FilterMode where
  type Demote FilterMode = FilterMode
instance Known FilterMode Nearest where
  known = Nearest
instance Known FilterMode Linear where
  known = Linear

data DepthTesting
  = NoDepthTest
  | DepthTest
  deriving ( Show, Eq, Ord, Enum, Bounded )

instance Demotable DepthTesting where
  type Demote DepthTesting = DepthTesting
instance Known DepthTesting NoDepthTest where
  known = NoDepthTest
instance Known DepthTesting DepthTest where
  known = DepthTest

data Projection
  = Affine
  | Projective
  deriving ( Show, Eq, Ord, Enum, Bounded )

instance Demotable Projection where
  type Demote Projection = Projection
instance Known Projection Affine where
  known = Affine
instance Known Projection Projective where
  known = Projective

data LODOperand
  = Bias
  | LOD
  | Grad
  | MinLOD
  deriving ( Show, Eq, Ord )

data Operand
  = LODOperand LODOperand
  | ConstOffset
  | Offset
  | ConstOffsets
  | Sample
  deriving ( Show, Eq, Ord )

operandBit :: Operand -> Word32
operandBit (LODOperand Bias )   = 0x01
operandBit (LODOperand LOD  )   = 0x02
operandBit (LODOperand Grad )   = 0x04
operandBit ConstOffset          = 0x08
operandBit Offset               = 0x10
operandBit ConstOffsets         = 0x20
operandBit Sample               = 0x40
operandBit (LODOperand MinLOD ) = 0x80
