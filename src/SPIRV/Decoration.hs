
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module: SPIRV.Decoration

This module enumerates SPIR-V decorations. Used both at the type-level and term-level.

See SPIR-V specification §3.20 __Decoration__.

In this library, decorations are usually provided as part of a top-level definition specifying an interface
for a SPIR-V module. See "FIR.Definition".

-}

module SPIRV.Decoration
  ( Decoration(..)
  , Decorations
  , isLayoutDecoration
  )
  where

-- base
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( Nat )

-- containers
import Data.Set
  ( Set )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known), knownValue )
import qualified SPIRV.Builtin as Builtin

--------------------------------------------------

data Decoration a
  = RelaxedPrecision
  | SpecId a
  | Block
  --   | BufferBlock -- deprecated
  | RowMajor
  | ColMajor
  | ArrayStride  a
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
  | Stream        a
  | Location      a
  | Component     a
  | Index         a
  | Binding       a
  | DescriptorSet a
  | Offset        a
  | XfbBuffer     a
  | XfbStride     a
  -- | FuncParamAttr Attribute
  -- | FPRoundingMode RoundingMode
  -- | FPFastMathMode FastMathMode
  -- | LinkageAttributes Word32 LinkageType
  | NoContraction
  | InputAttachmentIndex a
  | Alignment a
  deriving stock ( Show, Eq, Ord )

isLayoutDecoration :: Decoration a -> Bool
isLayoutDecoration dec = case dec of
  Block          -> True
  --BufferBlock    -> True
  RowMajor       -> True
  ColMajor       -> True
  ArrayStride  _ -> True
  MatrixStride _ -> True
  GLSLShared     -> True
  GLSLPacked     -> True
  CPacked        -> True
  _              -> False

type Decorations = Set (Decoration Word32)

instance Put (Decoration Word32) where
  put RelaxedPrecision         = put @Word32  0
  put (SpecId i)               = put @Word32  1 *> put i
  put Block                    = put @Word32  2
  --put BufferBlock              = put @Word32  3
  put RowMajor                 = put @Word32  4
  put ColMajor                 = put @Word32  5
  put (ArrayStride  i)         = put @Word32  6 *> put i
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

  wordCount (SpecId               _) = 2
  wordCount (ArrayStride          _) = 2
  wordCount (MatrixStride         _) = 2
  wordCount (Builtin              _) = 2
  wordCount (Stream               _) = 2
  wordCount (Location             _) = 2
  wordCount (Component            _) = 2
  wordCount (Index                _) = 2
  wordCount (Binding              _) = 2
  wordCount (DescriptorSet        _) = 2
  wordCount (Offset               _) = 2
  wordCount (XfbBuffer            _) = 2
  wordCount (XfbStride            _) = 2
  wordCount (InputAttachmentIndex _) = 2
  wordCount (Alignment            _) = 2
  wordCount _ = 1

instance Demotable (Decoration Nat) where
  type Demote (Decoration Nat) = Decoration Word32

instance Known (Decoration Nat) RelaxedPrecision where
  known = RelaxedPrecision
instance Known Nat i => Known (Decoration Nat) (SpecId i) where
  known = SpecId ( knownValue @i )
instance Known (Decoration Nat) Block where
  known = Block
--instance Known (Decoration Nat) BufferBlock where
--  known = BufferBlock
instance Known (Decoration Nat) RowMajor where
  known = RowMajor
instance Known (Decoration Nat) ColMajor where
  known = ColMajor
instance Known Nat i => Known (Decoration Nat) (ArrayStride i) where
  known = ArrayStride ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (MatrixStride i) where
  known = MatrixStride ( knownValue @i )
instance Known (Decoration Nat) GLSLShared where
  known = GLSLShared
instance Known (Decoration Nat) GLSLPacked where
  known = GLSLPacked
instance Known (Decoration Nat) CPacked where
  known = CPacked
instance Known Builtin.Builtin builtin => Known (Decoration Nat) (Builtin builtin) where
  known = Builtin ( knownValue @builtin )
instance Known (Decoration Nat) NoPerspective where
  known = NoPerspective
instance Known (Decoration Nat) Flat where
  known = Flat
instance Known (Decoration Nat) Patch where
  known = Patch
instance Known (Decoration Nat) Centroid where
  known = Centroid
instance Known (Decoration Nat) Sample where
  known = Sample
instance Known (Decoration Nat) Invariant where
  known = Invariant
instance Known (Decoration Nat) Restrict where
  known = Restrict
instance Known (Decoration Nat) Aliased where
  known = Aliased
instance Known (Decoration Nat) Volatile where
  known = Volatile
instance Known (Decoration Nat) Constant where
  known = Constant
instance Known (Decoration Nat) Coherent where
  known = Coherent
instance Known (Decoration Nat) NonWritable where
  known = NonWritable
instance Known (Decoration Nat) NonReadable where
  known = NonReadable
instance Known (Decoration Nat) DynamicallyUniform where
  known = DynamicallyUniform
instance Known (Decoration Nat) SaturatedConversion where
  known = SaturatedConversion
instance Known Nat i => Known (Decoration Nat) (Stream i) where
  known = Stream ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (Location i) where
  known = Location ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (Component i) where
  known = Component ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (Index i) where
  known = Index ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (Binding i) where
  known = Binding ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (DescriptorSet i) where
  known = DescriptorSet ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (Offset i) where
  known = Offset ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (XfbBuffer i) where
  known = XfbBuffer ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (XfbStride i) where
  known = XfbStride ( knownValue @i )
instance Known (Decoration Nat) NoContraction where
  known = NoContraction
instance Known Nat i => Known (Decoration Nat) (InputAttachmentIndex i) where
  known = InputAttachmentIndex ( knownValue @i )
instance Known Nat i => Known (Decoration Nat) (Alignment i) where
  known = Alignment ( knownValue @i )
