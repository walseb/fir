{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-|
Module: CodeGen.Images

Code generation for image sampling, reading and writing.
-}

module CodeGen.Images
  ( imageTexel, writeTexel )
  where

-- base
import Control.Arrow
  ( first, (&&&) )
import Data.Bits
  ( Bits((.|.), shiftR, testBit) )
import Data.Foldable
  ( traverse_ )
import Data.List
  ( insertBy )
import Data.Ord
  ( comparing )
import Data.Word
  ( Word32 )

-- lens
import Control.Lens
  ( view )

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-short
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.IDs
  ( typeID, constID )
import CodeGen.Instruction
  ( ID, TyID, Instruction(..)
  , Args(Arg,EndArgs), toArgs
  )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh) )
import CodeGen.State
  ( _backend, requireCapabilities )
import Data.Type.Known
  ( Known, knownValue )
import FIR.AST
  ( AST, Code
  , pattern NilOps, pattern Proj, pattern Dref, pattern Bias
  , pattern LOD, pattern MinLOD, pattern Grad
  , pattern ConstOffsetBy, pattern OffsetBy
  , pattern Gather, pattern SampleNo
  , ImgOpsF
  )
import FIR.Prim.Image
  ( ImageProperties, ImageAndCoordinate(..)
  , ImageCoordinateKind(..)
  , ImageOperands, OperandName
  , GatherInfo(..)
  , knownImage
  )
import SPIRV.Image
  ( DepthTesting(..), Projection(..) )
import qualified SPIRV.Image        as SPIRV
import qualified SPIRV.Image
  ( Image(Image) )
import qualified SPIRV.Operation    as SPIRV
  ( Operation )
import qualified SPIRV.Operation    as SPIRV.Op
import qualified SPIRV.PrimTy       as SPIRV
import qualified SPIRV.PrimTy
  ( PrimTy(Image, SampledImage) )
import qualified SPIRV.Requirements as SPIRV
  ( formatCapabilities
  , dimCapabilities
  , msCapabilities
  , lodCapabilities
  , gatherCapabilities
  )

--------------------------------------------------------------------------
-- image sample/read/write

imageTexel
  :: forall
        ( props :: ImageProperties )
        ( ops   :: [OperandName]   )
  .  ( CodeGen AST, Known ImageProperties props )
  => (ID, SPIRV.PrimTy)
  -> Code (ImageOperands props ops)
  -> ID
  -> (TyID, SPIRV.PrimTy)
  -> CGMonad (ID, SPIRV.PrimTy)
imageTexel (imgID, imgTy) ops coords (imgTexelTyID, imgTexelTy)
  = do
      ( bm, operandsArgs ) <- operandsToArgs ops
      let lod    = lodMethod    bm
          proj   = projection   bm
          dtest  = depthTesting bm
          gathering = gather    bm
          ImageAndCoordinate
            ( SPIRV.Image.Image
                { SPIRV.dimensionality = dim
                , SPIRV.arrayness      = arrayness
                , SPIRV.multiSampling  = ms
                , SPIRV.imageFormat    = mbFmt
                }
            , coordCompKind
            ) = knownValue @props

      v <- fresh

      sampling <- case coordCompKind of
        -- must be using a sampler (accessing with floating-point coordinates)
        FloatingPointCoordinates
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
                                  \of type " <> ShortText.pack ( show imgTy )
                                )
                if gathering
                then do
                  instruction
                    Instruction
                      { operation = gatherOperation dtest
                      , resTy     = Just imgTexelTyID
                      , resID     = Just v
                      , args      = Arg sampledImgID
                                  $ Arg coords
                                  $ operandsArgs
                      }
                  pure False
                else do
                  instruction
                    Instruction
                      { operation = sampleOperation lod dtest proj
                      , resTy     = Just imgTexelTyID
                      , resID     = Just v
                      , args      = Arg sampledImgID
                                  $ Arg coords
                                  $ operandsArgs
                      }
                  pure True
        -- reading from image directly (using integral coordinates)
        IntegralCoordinates
          -> do plainImgID
                  <- case imgTy of
                        SPIRV.PrimTy.Image _
                          -> pure imgID
                        SPIRV.PrimTy.SampledImage plainImg
                          -> fst <$> removeSampler (imgID, plainImg)
                        _ -> throwError
                               ( "codeGen: image read operation provided non-image \
                                 \of type " <> ShortText.pack ( show imgTy )
                               )
                instruction
                  Instruction
                    { operation = SPIRV.Op.ImageRead
                    , resTy     = Just imgTexelTyID
                    , resID     = Just v
                    , args      = Arg plainImgID
                                $ Arg coords
                                $ operandsArgs
                    }
                pure False

      -- declare capabilities
      requireImageCapabilities sampling bm dim arrayness ms mbFmt
      
      -- return the result ID
      pure (v, imgTexelTy)


writeTexel
  :: forall props ops
  .  ( CodeGen AST, Known ImageProperties props )
  => (ID, SPIRV.PrimTy)
  -> Code (ImageOperands props ops)
  -> ID
  -> ID
  -> CGMonad ()
writeTexel (imgID, imgTy) ops coords texel
  = do
      let SPIRV.Image.Image
            { SPIRV.dimensionality = dim
            , SPIRV.arrayness      = arrayness
            , SPIRV.multiSampling  = ms
            , SPIRV.imageFormat    = mbFmt
            } = knownImage @props

      ( _, operandsArgs ) <- operandsToArgs ops
      plainImgID
        <- case imgTy of
              SPIRV.PrimTy.Image _
                -> pure imgID
              SPIRV.PrimTy.SampledImage plainImg
                -> fst <$> removeSampler (imgID, plainImg)
              _ -> throwError
                     ( "codeGen: image write operation provided non-image \
                       \of type " <> ShortText.pack ( show imgTy )
                     )
      instruction
        Instruction
          { operation = SPIRV.Op.ImageWrite
          , resTy     = Nothing
          , resID     = Nothing
          , args      = Arg plainImgID
                      $ Arg coords
                      $ Arg texel
                      $ operandsArgs
          }

      -- declare capabilities
      requireImageCapabilities False 0 dim arrayness ms mbFmt
      
      -- return the result ID
      pure ()

--------------------------------------------------------------------------
-- add/remove sampler

{-
addSampler :: (ID, SPIRV.Image.Image) -> ID -> CGMonad (ID, SPIRV.PrimTy)
addSampler (imgID, imgTy) samplerID
  = do
      let sampledImgTy = SPIRV.PrimTy.SampledImage imgTy
      sampledImgTyID <- typeID sampledImgTy
      v <- fresh
      instruction
        Instruction
          { operation = SPIRV.Op.SampledImage
          , resTy     = Just sampledImgTyID
          , resID     = Just v
          , args      = Arg imgID
                      $ Arg samplerID EndArgs
          }
      pure (v, sampledImgTy)
-}

removeSampler :: CodeGen AST => (ID, SPIRV.Image) -> CGMonad (ID, SPIRV.PrimTy)
removeSampler (imgID, imgTy)
  = do
      let plainImgTy = SPIRV.PrimTy.Image imgTy
      plainImgTyID <- typeID plainImgTy
      v <- fresh
      instruction
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
  deriving stock ( Show, Eq, Ord, Enum, Bounded )

lodMethod :: Word32 -> LODMethod
lodMethod bm
  | bm `testBit` 9 || bm `testBit` 10 = ExplicitLOD --'LOD' or 'Grad' operand provided
  | otherwise                         = ImplicitLOD

depthTesting :: Word32 -> DepthTesting
depthTesting bm
  | bm `testBit` 0 = DepthTest
  | otherwise      = NoDepthTest

projection :: Word32 -> Projection
projection bm
  | bm `testBit` 1 = Projective
  | otherwise      = Affine

gather :: Word32 -> Bool
gather = (`testBit` 13)

sampleOperation :: LODMethod -> DepthTesting -> Projection -> SPIRV.Operation
sampleOperation ImplicitLOD NoDepthTest Affine     = SPIRV.Op.ImageSampleImplicitLod
sampleOperation ExplicitLOD NoDepthTest Affine     = SPIRV.Op.ImageSampleExplicitLod
sampleOperation ImplicitLOD DepthTest   Affine     = SPIRV.Op.ImageSampleDrefImplicitLod
sampleOperation ExplicitLOD DepthTest   Affine     = SPIRV.Op.ImageSampleDrefExplicitLod
sampleOperation ImplicitLOD NoDepthTest Projective = SPIRV.Op.ImageSampleProjImplicitLod
sampleOperation ExplicitLOD NoDepthTest Projective = SPIRV.Op.ImageSampleProjExplicitLod
sampleOperation ImplicitLOD DepthTest   Projective = SPIRV.Op.ImageSampleProjDrefImplicitLod
sampleOperation ExplicitLOD DepthTest   Projective = SPIRV.Op.ImageSampleProjDrefExplicitLod

gatherOperation :: DepthTesting -> SPIRV.Operation
gatherOperation NoDepthTest = SPIRV.Op.ImageGather
gatherOperation DepthTest   = SPIRV.Op.ImageDrefGather

--------------------------------------------------------------------------
-- capabilities

requireImageCapabilities
  :: CodeGen AST
  => Bool
  -> Word32
  -> SPIRV.Dimensionality
  -> SPIRV.Arrayness
  -> SPIRV.MultiSampling
  -> Maybe (SPIRV.ImageFormat Word32)
  -> CGMonad ()
requireImageCapabilities sampling bm dim arrayness ms mbFmt
  = do
      bk <- view _backend
      traverse_ requireCapabilities
        ( SPIRV.formatCapabilities <$> mbFmt )
      requireCapabilities ( SPIRV.msCapabilities        ms )
      requireCapabilities ( SPIRV.lodCapabilities    bk bm )
      requireCapabilities ( SPIRV.gatherCapabilities    bm )
      requireCapabilities ( SPIRV.dimCapabilities sampling dim arrayness )

--------------------------------------------------------------------------
-- dealing with operands

instance CodeGen (ImgOpsF ast) where
  codeGenArgs _ = throwError "Unexpected image operand."

operands :: CodeGen AST => Code (ImageOperands props ops) -> CGMonad (Word32, [(ID, Word32)])
operands NilOps     = pure  ( 0x0000, [] )
operands (Proj ops) = first ( 0x0002 .|. ) <$> operands ops
operands (Dref dref ops)
  = do
      i <- fst <$> codeGen dref
      (bm, nxt) <- operands ops
      let opBit = 0x0001
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
operands (Grad (gx,gy) ops)
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
      offID <- fst <$> codeGen off
      (bm, nxt) <- operands ops
      let opBit = 0x1000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (offID, opBit) nxt
           )
operands (Gather (ComponentWithOffsets comp offs) ops)
  = do
      compID <- fst <$> codeGen comp
      offsID <- constID offs
      (bm, nxt) <- operands ops
      let opBit = 0x2000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (compID, 0x0001) -- says "put me first", see 'opArgs'
           . insertBy (comparing snd) (offsID, opBit )
           $ nxt
           )
operands (Gather (DepthWithOffsets offs) ops)
  = do
      offsID <- constID offs
      (bm, nxt) <- operands ops
      let opBit = 0x2000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (offsID, opBit) nxt
           )
operands (SampleNo no ops)
  = do
      i <- fst <$> codeGen no
      (bm, nxt) <- operands ops
      let opBit = 0x4000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands (MinLOD lod ops)
  = do
      i <- fst <$> codeGen lod
      (bm, nxt) <- operands ops
      let opBit = 0x8000
      pure ( opBit .|. bm
           , insertBy (comparing snd) (i, opBit) nxt
           )
operands ops
  = throwError 
  $ "'operands': image operands not of the expected form\n"
  <> ShortText.pack (show ops)

opArgs :: Word32 -> [(ID, Word32)] -> Args
opArgs _  [] = EndArgs
opArgs bm ((putMeFirst, 0x0001) : ops)
  -- special case for an argument that needs to be put first
  -- must go before everything else (including bitmask)
  = Arg putMeFirst (opArgs bm ops) 
opArgs bm ops
  = Arg
      ( shiftR bm 8 ) -- bitmask first, with bottom byte removed
      ( toArgs (map fst ops) ) -- then the IDs in order

operandsToArgs :: CodeGen AST => Code (ImageOperands props ops) -> CGMonad (Word32, Args)
operandsToArgs ops = (fst &&& uncurry opArgs) <$> operands ops

stableSplice :: Ord b => ( [a], b ) -> [ (a,b) ] -> [ (a,b) ]
stableSplice    ( as, bref ) []
  = map ( , bref ) as
stableSplice l1@( as, bref ) l2@( (a, b) : nxt )
  | b > bref  = map ( , bref ) as ++ l2
  | otherwise = ( a, b ) : stableSplice l1 nxt
