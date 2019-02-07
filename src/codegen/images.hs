{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.Images
  ( sample, imageRead, imageWrite )
  where

-- base
import Control.Arrow
  ( (&&&) )
import Data.Bits
  ( Bits((.&.), shiftR, testBit) )
import Data.Word
  ( Word32 )

-- containers
import qualified Data.Map as Map

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-utf8
import qualified Data.Text as Text

-- fir
import CodeGen.Binary
  ( putInstruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
import CodeGen.IDs
  ( typeID, constID )
import CodeGen.Instruction
  ( ID, Instruction(..)
  , Args(Arg,EndArgs), toArgs
  )
import CodeGen.Monad
  ( CGMonad, liftPut
  , MonadFresh(fresh)
  )
import Data.Type.Known
  ( Known, knownValue )
import FIR.Prim.Image
  ( ImageProperties
  , ImageOperand(..), ImageOperands(..), ImageOperandList(..)
  , LODMethod(..)
  , knownImageReturnComponent
  )
import SPIRV.Image
  ( DepthTesting(..), Projection(..) )
import qualified SPIRV.Image     as SPIRV
import qualified SPIRV.Operation as SPIRV
  ( Operation )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import qualified SPIRV.PrimTy
  ( PrimTy(Image, SampledImage) )

--------------------------------------------------------------------------

sample :: forall meth props.
          ( Known (Maybe SPIRV.SamplingMethod) meth
          , Known ImageProperties props
          )
       => (ID, SPIRV.PrimTy)
       -> ImageOperands meth props
       -> ID
       -> CGMonad (ID, SPIRV.PrimTy)
sample (imgID, imgTy) (Operands ops) coords
  = do
      ( bm, operandsArgs ) <- operandsToArgs ops
      let lod    = lodMethod    bm
          mbMeth = knownValue @meth
          proj = case mbMeth of
            Nothing                  -> Affine
            Just (SPIRV.Method _ pr) -> pr
          dtest  = depthTesting bm
          imgResCompTy = knownImageReturnComponent @props
          imgResTy = case dtest of
              DepthTest -> imgResCompTy
              _         -> SPIRV.Vector 4 imgResCompTy
      -- create sampled image
      sampledImgID <-
        case imgTy of
          SPIRV.PrimTy.SampledImage _
            -> pure imgID
          SPIRV.PrimTy.Image _
            -> throwError
                  ( "codeGen: trying to sample storage image." )
          _ -> throwError
                  ( "codeGen: image sampling provided non-image of type " <> Text.pack ( show imgTy ) )
      --
      resTyID <- typeID imgResTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = sampleOperation lod dtest proj
          , resTy     = Just resTyID
          , resID     = Just v
          , args      = Arg sampledImgID
                      $ Arg coords
                      $ operandsArgs
          }
      pure (v, imgResTy)

imageRead :: forall props.
            ( Known ImageProperties props )
          => (ID, SPIRV.PrimTy)
          -> ImageOperands Nothing props
          -> ID
          -> CGMonad (ID, SPIRV.PrimTy)
imageRead (imgID, _) (Operands ops) coords
  = do
      ( _, operandsArgs ) <- operandsToArgs ops
      let imgResTy = SPIRV.Vector 4 (knownImageReturnComponent @props)
      resTyID <- typeID imgResTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.ImageRead
          , resTy     = Just resTyID
          , resID     = Just v
          , args      = Arg imgID
                      $ Arg coords
                      $ operandsArgs
          }
      pure (v, imgResTy)

imageWrite :: ( Known ImageProperties props )
           => (ID, SPIRV.PrimTy)
           -> ImageOperands Nothing props
           -> ID
           -> ID
           -> CGMonad ()
imageWrite (imgID, _) (Operands ops) coords texelToWrite
  = do
      ( _, operandsArgs ) <- operandsToArgs ops
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.ImageRead
          , resTy     = Nothing
          , resID     = Nothing
          , args      = Arg imgID
                      $ Arg coords
                      $ Arg texelToWrite
                      $ operandsArgs
          }
      pure ()

{-

--------------------------------------------------------------------------
-- add/remove sampler

addSampler :: (ID, SPIRV.Image.Image) -> ID -> CGMonad (ID, SPIRV.PrimTy)
addSampler (imgID, imgTy) samplerID
  = do
      let sampledImgTy = SPIRV.PrimTy.SampledImage imgTy
      sampledImgTyID <- typeID sampledImgTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.SampledImage
          , resTy     = Just sampledImgTyID
          , resID     = Just v
          , args      = Arg imgID
                      $ Arg samplerID EndArgs
          }
      pure (v, sampledImgTy)

removeSampler :: (ID, SPIRV.Image.Image) -> CGMonad (ID, SPIRV.PrimTy)
removeSampler (imgID, imgTy)
  = do
      let plainImgTy = SPIRV.PrimTy.Image imgTy
      plainImgTyID <- typeID plainImgTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.Image
          , resTy     = Just plainImgTyID
          , resID     = Just v
          , args      = Arg imgID EndArgs
          }
      pure (v, plainImgTy)
-}

--------------------------------------------------------------------------
-- sampling: find which sampling operation to use

lodMethod :: Word32 -> LODMethod
lodMethod bm
  | testBit bm 9 || testBit bm 10 = ExplicitLOD --'LOD' or 'Grad' operand provided
  | otherwise                     = ImplicitLOD

depthTesting :: Word32 -> DepthTesting
depthTesting bm
  | testBit bm 0 = DepthTest
  | otherwise    = NoDepthTest

sampleOperation :: LODMethod -> DepthTesting -> Projection -> SPIRV.Operation
sampleOperation ImplicitLOD NoDepthTest Affine     = SPIRV.Op.ImageSampleImplicitLod
sampleOperation ExplicitLOD NoDepthTest Affine     = SPIRV.Op.ImageSampleExplicitLod
sampleOperation ImplicitLOD DepthTest   Affine     = SPIRV.Op.ImageSampleDrefImplicitLod
sampleOperation ExplicitLOD DepthTest   Affine     = SPIRV.Op.ImageSampleDrefExplicitLod
sampleOperation ImplicitLOD NoDepthTest Projective = SPIRV.Op.ImageSampleProjImplicitLod
sampleOperation ExplicitLOD NoDepthTest Projective = SPIRV.Op.ImageSampleProjExplicitLod
sampleOperation ImplicitLOD DepthTest   Projective = SPIRV.Op.ImageSampleProjDrefImplicitLod
sampleOperation ExplicitLOD DepthTest   Projective = SPIRV.Op.ImageSampleProjDrefExplicitLod

--------------------------------------------------------------------------
-- dealing with operands

-- sorting operands as SPIR-V needs
-- bottom byte reserved for depth
operandBit :: ImageOperand a dim proj ms op -> Word32
operandBit ( Dref           _ ) = 0x0001
operandBit ( Bias           _ ) = 0x0100
operandBit ( LOD            _ ) = 0x0200
operandBit ( Grad          {} ) = 0x0400
operandBit ( ConstOffsetBy  _ ) = 0x0800
operandBit ( OffsetBy       _ ) = 0x1000
operandBit ( ConstOffsetsBy _ ) = 0x2000
operandBit ( SampleNo       _ ) = 0x4000
operandBit ( MinLOD         _ ) = 0x8000

operand :: ImageOperand a dim proj ms op -> CGMonad [ID]
operand (Dref    dref) = (:[]) . fst <$> codeGen dref
operand (Bias    b   ) = (:[]) . fst <$> codeGen b
operand (LOD     lod ) = (:[]) . fst <$> codeGen lod
operand (MinLOD  lod ) = (:[]) . fst <$> codeGen lod
operand (SampleNo nb ) = (:[]) . fst <$> codeGen nb
operand (Grad gx gy) = traverse ( (fst <$>) . codeGen) [gx, gy]
operand (ConstOffsetBy  off ) = (:[])       <$> constID off
operand (OffsetBy       off ) = (:[]) . fst <$> codeGen off
operand (ConstOffsetsBy offs) = (:[])       <$> constID offs

operands :: ImageOperandList a dim proj ms ops -> CGMonad (Word32, [(ID, Word32)])
operands Done = pure (0, [])
operands (op `Op` ops)
  = do
      opIDs        <- operand op
      (bm, opsIDs) <- operands ops
      let opBit = operandBit op
      pure $ ( bm .&. opBit
             , stableSplice ( opIDs, opBit ) opsIDs
             )

opArgs :: Word32 -> [(ID, Word32)] -> Args
opArgs _  [] = EndArgs
opArgs bm ((dref, 0x0001) : ops)  -- special case for depth argument
  = Arg dref (opArgs bm ops) -- must go before everything else (including bitmask)
opArgs bm ops
  = Arg
      ( shiftR bm 8 ) -- bitmask first, with bottom byte removed
      ( toArgs (map fst ops) ) -- then the IDs in order

operandsToArgs :: ImageOperandList a dim proj ms ops -> CGMonad (Word32, Args)
operandsToArgs ops = (fst &&& uncurry opArgs) <$> operands ops

stableSplice :: Ord b => ( [a], b ) -> [ (a,b) ] -> [ (a,b) ]
stableSplice    ( as, bref ) []
  = map ( , bref ) as
stableSplice l1@( as, bref ) l2@( (a, b) : nxt )
  | b > bref  = map ( , bref ) as ++ l2
  | otherwise = ( a, b ) : stableSplice l1 nxt
