{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns    #-}

{-|
Module: SPIRV.Requrements

Functions computing capability/extension requirements
for various circumstances.

-}

module SPIRV.Requirements where

-- base
import Data.Bits
  ( Bits(testBit) )
import Data.Word
  ( Word32 )

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( insert, unions )

-- fir
import SPIRV.Capability
import SPIRV.Extension
import qualified SPIRV.Image    as Image
import           SPIRV.Operation
  hiding ( Capability )
import           SPIRV.PrimOp
  ( PrimOp(MatOp), op )
import           SPIRV.PrimTy
  ( PrimTy, scalars )
import qualified SPIRV.PrimTy   as PrimTy
import           SPIRV.ScalarTy
  ( ScalarTy, Width(..) )
import qualified SPIRV.ScalarTy as ScalarTy
import           SPIRV.Stage
  ( ExecutionModel(Stage), Stage(..), Shader )
import qualified SPIRV.Stage    as Stage
  ( ExecutionModel(..), Shader(..) )

--------------------------------------------------------------------------
-- primitive operations

primOpCapabilities :: PrimOp -> Set Capability
primOpCapabilities ( op -> SatConvertSToU ) = [ Kernel ]
primOpCapabilities ( op -> SatConvertUToS ) = [ Kernel ]
primOpCapabilities ( MatOp {}             ) = [ Matrix ]
primOpCapabilities _                        = [ ]

--------------------------------------------------------------------------
-- types

scalarCapabilities :: ScalarTy -> Set Capability
scalarCapabilities (ScalarTy.Integer _ W8 ) = [ Int8  ]
scalarCapabilities (ScalarTy.Integer _ W16) = [ Int16 ]
scalarCapabilities (ScalarTy.Integer _ W64) = [ Int64 ]
scalarCapabilities (ScalarTy.Integer _ _  ) = [ ]
scalarCapabilities (ScalarTy.Floating  W16) = [ Float16 ]
scalarCapabilities (ScalarTy.Floating  W64) = [ Float64 ]
scalarCapabilities (ScalarTy.Floating  _  ) = [ ]

primTyCapabilities :: PrimTy -> Set Capability
primTyCapabilities ( PrimTy.Matrix _ _ ty )
  = Set.insert Matrix    $ scalarCapabilities ty
primTyCapabilities ( PrimTy.Vector n   ty )
  | n > 4     = Set.insert Vector16 $ primTyCapabilities ty
  | otherwise =                       primTyCapabilities ty
primTyCapabilities ty
  = Set.unions ( map scalarCapabilities ( PrimTy.scalars ty) )

-- types when present in an interface
globalCapabilities :: PrimTy -> Set Capability
globalCapabilities = primTyCapabilities

globalScalarExtensions :: ScalarTy -> Set Extension
globalScalarExtensions (ScalarTy.Integer _ W8 ) = [ SPV_KHR_8bit_storage  ]
globalScalarExtensions (ScalarTy.Integer _ W16) = [ SPV_KHR_16bit_storage ]
globalScalarExtensions (ScalarTy.Integer _ _  ) = [ ]
globalScalarExtensions (ScalarTy.Floating  W16) = [ SPV_KHR_16bit_storage ]
globalScalarExtensions (ScalarTy.Floating  _  ) = [ ]

globalExtensions :: PrimTy -> Set Extension
globalExtensions ty = Set.unions ( map globalScalarExtensions ( scalars ty ) )

--------------------------------------------------------------------------
-- stages / execution models

shaderCapabilities :: Shader -> Set Capability
shaderCapabilities Stage.VertexShader                 = [ Shader       ]
shaderCapabilities Stage.TessellationControlShader    = [ Tessellation ] --- |
shaderCapabilities Stage.TessellationEvaluationShader = [ Tessellation ] --- |-- shader capability implied
shaderCapabilities Stage.GeometryShader               = [ Geometry     ] --- |
shaderCapabilities Stage.FragmentShader               = [ Shader       ]
shaderCapabilities Stage.ComputeShader                = [ Shader       ]

stageCapabilities :: Stage -> Set Capability
stageCapabilities (ShaderStage s) = shaderCapabilities s
stageCapabilities (MeshStage   _) = [ MeshShadingNV ]
stageCapabilities (RayStage    _) = [ RayTracingNV  ]

executionModelCapabilities :: ExecutionModel -> Set Capability
executionModelCapabilities (Stage s)    = stageCapabilities s
executionModelCapabilities Stage.Kernel = [ Kernel ]

stageExtensions :: Stage -> Set Extension
stageExtensions (ShaderStage _) = [ ]
stageExtensions (MeshStage   _) = [ SPV_NV_mesh_shader ]
stageExtensions (RayStage    _) = [ SPV_NV_ray_tracing ]

executionModelExtensions :: ExecutionModel -> Set Extension
executionModelExtensions (Stage s)    = stageExtensions s
executionModelExtensions Stage.Kernel = [ ]

--------------------------------------------------------------------------
-- images, image formats, image properties

formatCapabilities :: Image.ImageFormat Word32 -> Set Capability
formatCapabilities format
  = case Image.requiredFormatUsage format of
      Just Image.Storage   -> [ Shader, StorageImageExtendedFormats ]
      _                    -> [ Shader ]

dimCapabilities :: Bool -> Image.Dimensionality -> Image.Arrayness -> Set Capability
dimCapabilities True  Image.Rect        _             = [ Shader, SampledRect ]
dimCapabilities False Image.Rect        _             = [ Shader, SampledRect, ImageRect ]
dimCapabilities _     Image.SubpassData _             = [ Shader, InputAttachment ]
dimCapabilities False Image.OneD        _             = [ Sampled1D ]
dimCapabilities True  Image.OneD        _             = [ Sampled1D, Image1D ]
dimCapabilities True  Image.Cube        Image.Arrayed = [ SampledCubeArray ]
dimCapabilities True  Image.Buffer      _             = [ SampledBuffer ]
dimCapabilities False Image.Buffer      _             = [ SampledBuffer, ImageBuffer ]
dimCapabilities _     _                 _             = [ ]

msCapabilities :: Image.MultiSampling -> Set Capability
msCapabilities Image.MultiSampled = [ ImageMSArray ]
msCapabilities _                  = [ ]

-- lod capability from image operand bitmask
-- (note that the bitmask is shifted up one byte from what SPIR-V uses)
lodCapabilities :: Word32 -> Set Capability
lodCapabilities bm
  -- uses the LOD operand
  | bm `testBit` 9  = [ Kernel, ImageBasic, ImageMipmap ]
  -- uses the MinLOD opeand
  | bm `testBit` 15 = [ Shader, MinLod ]
  | otherwise       = [ ]

-- whether the ImageGatherExtended capability is required
gatherCapabilities :: Word32 -> Set Capability
gatherCapabilities bm
  |  bm `testBit` 12 -- Offset operand
  || bm `testBit` 13 -- ConstOffsets operand
  = [ ImageGatherExtended ]
  | otherwise
  = [ ]
