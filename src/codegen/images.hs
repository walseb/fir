{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module CodeGen.Images
  ( imageTexel, writeTexel )
  where

-- base
import Control.Arrow
  ( (&&&) )
import Data.Bits
  ( Bits((.|.), shiftR, testBit) )
import Data.List
  ( insertBy )
import Data.Ord
  ( comparing )
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
  ( Known )
import FIR.Prim.Image
  ( ImageProperties
  , ImageOperands(..)
  , knownImage
  , knownImageReturnComponent
  )
import SPIRV.Image
  ( DepthTesting(..), Projection(..) )
import qualified SPIRV.Image     as SPIRV
  ( Image(component) )
import qualified SPIRV.Operation as SPIRV
  ( Operation )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV
import qualified SPIRV.PrimTy
  ( PrimTy(Image, SampledImage) )
import qualified SPIRV.ScalarTy  as SPIRV
  ( ScalarTy(Integer, Floating) )

--------------------------------------------------------------------------


imageTexel
      :: forall props ops.
         ( Known ImageProperties props )
      => (ID, SPIRV.PrimTy)
      -> ImageOperands props ops
      -> ID
      -> CGMonad (ID, SPIRV.PrimTy)
imageTexel (imgID, imgTy) ops coords
  = do
      ( bm, operandsArgs ) <- operandsToArgs ops
      let lod    = lodMethod    bm
          proj   = projection   bm
          dtest  = depthTesting bm
          imgResCompTy = knownImageReturnComponent @props
          imgResTy = case dtest of
              DepthTest -> imgResCompTy
              _         -> SPIRV.Vector 4 imgResCompTy
      resTyID <- typeID imgResTy
      v <- fresh
      
      case SPIRV.component (knownImage @props) of
        -- must be using a sampler (accessing with floating-point coordinates)
        SPIRV.Floating _
          -> do sampledImgID
                  <- case imgTy of
                        SPIRV.PrimTy.SampledImage _
                          -> pure imgID
                        SPIRV.PrimTy.Image _
                          -> throwError
                                ( "codeGen: cannot sample image with floating-point coordinates: \
                                  \no sampler provided."
                                )
                        _ -> throwError
                                ( "codeGen: image sampling operation provided non-image \
                                  \of type " <> Text.pack ( show imgTy )
                                )
                liftPut $ putInstruction Map.empty
                  Instruction
                    { operation = sampleOperation lod dtest proj
                    , resTy     = Just resTyID
                    , resID     = Just v
                    , args      = Arg sampledImgID
                                $ Arg coords
                                $ operandsArgs
                    }
        -- reading from image directly (using integral coordinates)
        SPIRV.Integer _ _
          -> do plainImgID
                  <- case imgTy of
                        SPIRV.PrimTy.Image _
                          -> pure imgID
                        SPIRV.PrimTy.SampledImage plainImg
                          -> fst <$> removeSampler (imgID, plainImg)
                        _ -> throwError
                               ( "codeGen: image read operation provided non-image \
                                 \of type " <> Text.pack ( show imgTy )
                               )
                liftPut $ putInstruction Map.empty
                  Instruction
                    { operation = SPIRV.Op.ImageRead
                    , resTy     = Just resTyID
                    , resID     = Just v
                    , args      = Arg plainImgID
                                $ Arg coords
                                $ operandsArgs
                    }
      -- return the result ID
      pure (v, imgResTy)


writeTexel :: ( Known ImageProperties props )
           => (ID, SPIRV.PrimTy)
           -> ImageOperands props ops
           -> ID
           -> ID
           -> CGMonad ()
writeTexel (imgID, imgTy) ops coords texel
  = do
      ( _, operandsArgs ) <- operandsToArgs ops
      plainImgID
        <- case imgTy of
              SPIRV.PrimTy.Image _
                -> pure imgID
              SPIRV.PrimTy.SampledImage plainImg
                -> fst <$> removeSampler (imgID, plainImg)
              _ -> throwError
                     ( "codeGen: image write operation provided non-image \
                       \of type " <> Text.pack ( show imgTy )
                     )
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.ImageRead
          , resTy     = Nothing
          , resID     = Nothing
          , args      = Arg plainImgID
                      $ Arg coords
                      $ Arg texel
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
-}

removeSampler :: (ID, SPIRV.Image) -> CGMonad (ID, SPIRV.PrimTy)
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

--------------------------------------------------------------------------
-- sampling: find which sampling operation to use

data LODMethod
  = ImplicitLOD
  | ExplicitLOD
  deriving ( Show, Eq, Ord, Enum, Bounded )

lodMethod :: Word32 -> LODMethod
lodMethod bm
  | testBit bm 9 || testBit bm 10 = ExplicitLOD --'LOD' or 'Grad' operand provided
  | otherwise                     = ImplicitLOD

depthTesting :: Word32 -> DepthTesting
depthTesting bm
  | testBit bm 1 = DepthTest
  | otherwise    = NoDepthTest

projection :: Word32 -> Projection
projection bm
  | testBit bm 0 = Projective
  | otherwise    = Affine

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

operands :: ImageOperands props ops -> CGMonad (Word32, [(ID, Word32)])
operands Done = pure ( 0x0000, [] )
operands Proj = pure ( 0x0001, [] )
operands (Dref dref ops)
  = do
      i <- fst <$> codeGen dref
      (bm, nxt) <- operands ops
      let opBit = 0x0002
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (Bias bias ops)
  = do
      i <- fst <$> codeGen bias
      (bm, nxt) <- operands ops
      let opBit = 0x0100
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (LOD lod ops)
  = do
      i <- fst <$> codeGen lod
      (bm, nxt) <- operands ops
      let opBit = 0x0200
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (Grad gx gy ops)
  = do
      i1 <- fst <$> codeGen gx
      i2 <- fst <$> codeGen gy
      (bm, nxt) <- operands ops
      let opBit = 0x0400
      pure ( opBit .|. bm
           , stableSplice ([i1,i2], opBit) nxt )
operands (ConstOffsetBy off ops)
  = do
      i <- constID off
      (bm, nxt) <- operands ops
      let opBit = 0x0800
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (OffsetBy off ops)
  = do
      i <- fst <$> codeGen off
      (bm, nxt) <- operands ops
      let opBit = 0x1000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (ConstOffsetsBy offs ops)
  = do
      i <- constID offs
      (bm, nxt) <- operands ops
      let opBit = 0x2000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (SampleNo no ops)
  = do
      i <- fst <$> codeGen no
      (bm, nxt) <- operands ops
      let opBit = 0x0400
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (MinLOD lod ops)
  = do
      i <- fst <$> codeGen lod
      (bm, nxt) <- operands ops
      let opBit = 0x0800
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )


{-
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
      pure $ ( bm .|. opBit
             , stableSplice ( opIDs, opBit ) opsIDs
             )
-}

opArgs :: Word32 -> [(ID, Word32)] -> Args
opArgs _  [] = EndArgs
opArgs bm ((dref, 0x0002) : ops)  -- special case for depth argument
  = Arg dref (opArgs bm ops) -- must go before everything else (including bitmask)
opArgs bm ops
  = Arg
      ( shiftR bm 8 ) -- bitmask first, with bottom byte removed
      ( toArgs (map fst ops) ) -- then the IDs in order

operandsToArgs :: ImageOperands props ops -> CGMonad (Word32, Args)
operandsToArgs ops = (fst &&& uncurry opArgs) <$> operands ops

stableSplice :: Ord b => ( [a], b ) -> [ (a,b) ] -> [ (a,b) ]
stableSplice    ( as, bref ) []
  = map ( , bref ) as
stableSplice l1@( as, bref ) l2@( (a, b) : nxt )
  | b > bref  = map ( , bref ) as ++ l2
  | otherwise = ( a, b ) : stableSplice l1 nxt
