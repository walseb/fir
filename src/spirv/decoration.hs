
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module SPIRV.Decoration where

-- base
import Data.Proxy(Proxy(Proxy))
import Data.Word(Word32)
import GHC.TypeLits(KnownNat, natVal)

-- fir
import Data.Binary.Class.Put(Put(..))
import qualified SPIRV.Builtin as Builtin

--------------------------------------------------

data Decoration a
  = RelaxedPrecision
  | SpecId a
  | Block
  | BufferBlock
  | RowMajor
  | ColMajor
  | ArrayStride a
  | MatrixStride a
  | GLSLShared
  | GLSLPacked
  | CPacked
  | Builtin Builtin.Builtin
  -- no 12
  | NoPerspective
  | Flat
  | Patch
  | Centroid
  | Sample
  | Invariant
  | Restrict
  | Aliased
  | Volatile
  | Constant
  | Coherent
  | NonWritable
  | NonReadable
  | DynamicallyUniform -- simply called 'Uniform' in the SPIR-V spec
  -- no 27
  | SaturatedConversion
  | Stream a
  | Location a
  | Component a
  | Index a
  | Binding a
  | DescriptorSet a
  | Offset a
  | XfbBuffer a
  | XfbStride a
  -- | FuncParamAttr Attribute
  -- | FPRoundingMode RoundingMode
  -- | FPFastMathMode FastMathMode
  -- | LinkageAttributes Word32 LinkageType
  | NoContraction
  | InputAttachmentIndex a
  | Alignment a
  deriving ( Show, Eq, Ord )

instance Put (Decoration Word32) where
  put RelaxedPrecision         = put @Word32  0
  put (SpecId i)               = put @Word32  1 *> put i
  put Block                    = put @Word32  2
  put BufferBlock              = put @Word32  3
  put RowMajor                 = put @Word32  4
  put ColMajor                 = put @Word32  5
  put (ArrayStride i)          = put @Word32  6 *> put i
  put (MatrixStride i)         = put @Word32  7 *> put i
  put GLSLShared               = put @Word32  8
  put GLSLPacked               = put @Word32  9
  put CPacked                  = put @Word32 10
  put (Builtin builtin)        = put @Word32 11 *> put builtin
  --
  put NoPerspective            = put @Word32 13
  put Flat                     = put @Word32 14
  put Patch                    = put @Word32 15
  put Centroid                 = put @Word32 16
  put Sample                   = put @Word32 17
  put Invariant                = put @Word32 18
  put Restrict                 = put @Word32 19
  put Aliased                  = put @Word32 20
  put Volatile                 = put @Word32 21
  put Constant                 = put @Word32 22
  put Coherent                 = put @Word32 23
  put NonWritable              = put @Word32 24
  put NonReadable              = put @Word32 25
  put DynamicallyUniform       = put @Word32 26
  --
  put SaturatedConversion      = put @Word32 28
  put (Stream               i) = put @Word32 29 *> put i
  put (Location             i) = put @Word32 30 *> put i
  put (Component            i) = put @Word32 31 *> put i
  put (Index                i) = put @Word32 32 *> put i
  put (Binding              i) = put @Word32 33 *> put i
  put (DescriptorSet        i) = put @Word32 34 *> put i
  put (Offset               i) = put @Word32 35 *> put i
  put (XfbBuffer            i) = put @Word32 36 *> put i
  put (XfbStride            i) = put @Word32 37 *> put i
  --
  --
  --
  --
  put NoContraction            = put @Word32 42
  put (InputAttachmentIndex i) = put @Word32 43 *> put i
  put (Alignment            i) = put @Word32 44 *> put i

  sizeOf (SpecId               _) = 2
  sizeOf (ArrayStride          _) = 2
  sizeOf (MatrixStride         _) = 2
  sizeOf (Builtin              _) = 2
  sizeOf (Stream               _) = 2
  sizeOf (Location             _) = 2
  sizeOf (Component            _) = 2
  sizeOf (Index                _) = 2
  sizeOf (Binding              _) = 2
  sizeOf (DescriptorSet        _) = 2
  sizeOf (Offset               _) = 2
  sizeOf (XfbBuffer            _) = 2
  sizeOf (XfbStride            _) = 2
  sizeOf (InputAttachmentIndex _) = 2
  sizeOf (Alignment            _) = 2
  sizeOf _ = 1

class KnownDecoration (decoration :: Decoration a) where
  decoration :: Decoration Word32

instance KnownDecoration RelaxedPrecision where
  decoration = RelaxedPrecision
instance KnownNat i => KnownDecoration (SpecId i) where
  decoration = SpecId ( fromIntegral . natVal $ Proxy @i )
instance KnownDecoration Block where
  decoration = Block
instance KnownDecoration BufferBlock where
  decoration = BufferBlock
instance KnownDecoration RowMajor where
  decoration = RowMajor
instance KnownDecoration ColMajor where
  decoration = ColMajor
instance KnownNat i => KnownDecoration (ArrayStride i) where
  decoration = ArrayStride ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (MatrixStride i) where
  decoration = MatrixStride ( fromIntegral . natVal $ Proxy @i )
instance KnownDecoration GLSLShared where
  decoration = GLSLShared
instance KnownDecoration GLSLPacked where
  decoration = GLSLPacked
instance KnownDecoration CPacked where
  decoration = CPacked
instance Builtin.KnownBuiltin builtin => KnownDecoration (Builtin builtin) where
  decoration = Builtin ( Builtin.builtin @builtin )
instance KnownDecoration NoPerspective where
  decoration = NoPerspective
instance KnownDecoration Flat where
  decoration = Flat
instance KnownDecoration Patch where
  decoration = Patch
instance KnownDecoration Centroid where
  decoration = Centroid
instance KnownDecoration Sample where
  decoration = Sample
instance KnownDecoration Invariant where
  decoration = Invariant
instance KnownDecoration Restrict where
  decoration = Restrict
instance KnownDecoration Aliased where
  decoration = Aliased
instance KnownDecoration Volatile where
  decoration = Volatile
instance KnownDecoration Constant where
  decoration = Constant
instance KnownDecoration Coherent where
  decoration = Coherent
instance KnownDecoration NonWritable where
  decoration = NonWritable
instance KnownDecoration NonReadable where
  decoration = NonReadable
instance KnownDecoration DynamicallyUniform where
  decoration = DynamicallyUniform
instance KnownDecoration SaturatedConversion where
  decoration = SaturatedConversion
instance KnownNat i => KnownDecoration (Stream i) where
  decoration = Stream ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (Location i) where
  decoration = Location ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (Component i) where
  decoration = Component ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (Index i) where
  decoration = Index ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (Binding i) where
  decoration = Binding ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (DescriptorSet i) where
  decoration = DescriptorSet ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (Offset i) where
  decoration = Offset ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (XfbBuffer i) where
  decoration = XfbBuffer ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (XfbStride i) where
  decoration = XfbStride ( fromIntegral . natVal $ Proxy @i )
instance KnownDecoration NoContraction where
  decoration = NoContraction
instance KnownNat i => KnownDecoration (InputAttachmentIndex i) where
  decoration = InputAttachmentIndex ( fromIntegral . natVal $ Proxy @i )
instance KnownNat i => KnownDecoration (Alignment i) where
  decoration = Alignment ( fromIntegral . natVal $ Proxy @i )


class KnownDecorations (modes :: [Decoration a]) where
  decorations :: [Decoration Word32]

instance KnownDecorations '[] where
  decorations = []

instance (KnownDecoration a, KnownDecorations as)
      => KnownDecorations (a ': as) where
  decorations = decoration @_ @a : decorations @_ @as
