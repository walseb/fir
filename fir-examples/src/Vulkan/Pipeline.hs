{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Vulkan.Pipeline where

-- base
import Control.Arrow
  ( second )
import Data.Bits
  ( (.|.) )
import Data.Functor
  ( (<&>) )
import Data.Maybe
  ( fromMaybe )
import Data.Word
  ( Word32 )
import GHC.TypeNats
  ( Nat )

-- bytestring
import qualified Data.ByteString

-- logging-effect
import Control.Monad.Log
  ( logDebug )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )

-- vector
import qualified Data.Vector as Boxed.Vector
  ( empty, fromList, head, singleton )

-- vulkan
import qualified Vulkan
import qualified Vulkan.CStruct.Extends as Vulkan
  ( SomeStruct(SomeStruct) )

-- fir
import FIR
  ( Shader(..)
  , PipelineInfo
  , PipelineStages, ShaderPipeline(..)
  , pipelineStages
  , PrimitiveConnectedness(..)
  , PrimitiveTopology(..)
  , (:->)((:->))
  , Known, knownValue
  , GetVertexInputInfo
  , BindingStrides, VertexLocationDescriptions
  , ImageFormat
  )

-- fir-examples
import Vulkan.Formats
  ( simpleFormat )
import Vulkan.Monad

-------------------------------------------------------------------------

topology :: PrimitiveTopology n -> Vulkan.PrimitiveTopology
topology Points                    = Vulkan.PRIMITIVE_TOPOLOGY_POINT_LIST
topology (Line List              ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_LIST
topology (Line Strip             ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_STRIP
topology (Line Fan               ) = error "Invalid topology: fan of lines."
topology (Triangle List          ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
topology (Triangle Strip         ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
topology (Triangle Fan           ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
topology (Line AdjacencyList     ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
topology (Line AdjacencyStrip    ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
topology (Triangle AdjacencyList ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
topology (Triangle AdjacencyStrip) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
topology (PatchesOfSize         _) = Vulkan.PRIMITIVE_TOPOLOGY_PATCH_LIST

assemblyInfo
  :: PrimitiveTopology n -> Vulkan.PipelineInputAssemblyStateCreateInfo
assemblyInfo primTop =
  Vulkan.PipelineInputAssemblyStateCreateInfo
    { Vulkan.flags                  = Vulkan.zero
    , Vulkan.topology               = topology primTop
    , Vulkan.primitiveRestartEnable = False
    }

tessellationInfo
  :: PrimitiveTopology Word32 -> Maybe ( Vulkan.PipelineTessellationStateCreateInfo '[] )
tessellationInfo (PatchesOfSize pts) = Just $
  Vulkan.PipelineTessellationStateCreateInfo
    { Vulkan.next               = ()
    , Vulkan.flags              = Vulkan.zero
    , Vulkan.patchControlPoints = fromIntegral pts
    }
tessellationInfo _ = Nothing

topologyAndVertexInputStateInfo
  :: forall
      ( info    :: PipelineInfo               )
      ( top     :: PrimitiveTopology Nat      )
      ( descs   :: VertexLocationDescriptions )
      ( strides :: BindingStrides             )
  . ( '(top, descs, strides) ~ GetVertexInputInfo info
    , Known (PrimitiveTopology Nat)    top
    , Known VertexLocationDescriptions descs
    , Known BindingStrides             strides
    )
  => ( PrimitiveTopology Word32, Vulkan.PipelineVertexInputStateCreateInfo '[] )
topologyAndVertexInputStateInfo =
  let
    primTop :: PrimitiveTopology Word32
    primTop = knownValue @top

    bindingStrides :: [ Word32 :-> Word32 ]
    bindingStrides = knownValue @strides

    attributes :: [ Word32 :-> (Word32, Word32, ImageFormat Word32) ]
    attributes = knownValue @descs

    computeVulkanFormat :: ImageFormat Word32 -> Vulkan.Format
    computeVulkanFormat fmt
      = fromMaybe
          ( error $ "Unsupported format " ++ show fmt ++ " used as a vertex input attribute." )
          ( simpleFormat fmt )

    vertexBindingDescriptions :: [ Vulkan.VertexInputBindingDescription ]
    vertexBindingDescriptions =
      bindingStrides <&> \ ( binding :-> stride ) ->
          Vulkan.VertexInputBindingDescription
            { Vulkan.binding   = binding
            , Vulkan.stride    = stride
            , Vulkan.inputRate = Vulkan.VERTEX_INPUT_RATE_VERTEX
            }

    vertexAttributeDescriptions :: [ Vulkan.VertexInputAttributeDescription ]
    vertexAttributeDescriptions =
      attributes <&> \ ( location :-> ( binding, offset, format ) ) ->
        Vulkan.VertexInputAttributeDescription 
          { Vulkan.location = location
          , Vulkan.binding  = binding
          , Vulkan.format   = computeVulkanFormat format
          , Vulkan.offset   = offset
          }

    vertexInputStateInfo :: Vulkan.PipelineVertexInputStateCreateInfo '[]
    vertexInputStateInfo =
      Vulkan.PipelineVertexInputStateCreateInfo
        { Vulkan.next                        = ()
        , Vulkan.flags                       = Vulkan.zero
        , Vulkan.vertexBindingDescriptions   = Boxed.Vector.fromList vertexBindingDescriptions
        , Vulkan.vertexAttributeDescriptions = Boxed.Vector.fromList vertexAttributeDescriptions
        }

  in ( primTop, vertexInputStateInfo )


stageFlag :: Shader -> Vulkan.ShaderStageFlagBits
stageFlag VertexShader                 = Vulkan.SHADER_STAGE_VERTEX_BIT
stageFlag TessellationControlShader    = Vulkan.SHADER_STAGE_TESSELLATION_CONTROL_BIT
stageFlag TessellationEvaluationShader = Vulkan.SHADER_STAGE_TESSELLATION_EVALUATION_BIT
stageFlag GeometryShader               = Vulkan.SHADER_STAGE_GEOMETRY_BIT
stageFlag FragmentShader               = Vulkan.SHADER_STAGE_FRAGMENT_BIT
stageFlag ComputeShader                = Vulkan.SHADER_STAGE_COMPUTE_BIT

shaderExtension :: Shader -> String
shaderExtension VertexShader                 = "vert"
shaderExtension TessellationControlShader    = "tesc"
shaderExtension TessellationEvaluationShader = "tese"
shaderExtension GeometryShader               = "geom"
shaderExtension FragmentShader               = "frag"
shaderExtension ComputeShader                = "comp"

loadShader :: MonadVulkan m => Vulkan.Device -> FilePath -> m ( ReleaseKey, Vulkan.ShaderModule )
loadShader device shaderPath = do

  bytes <- liftIO ( Data.ByteString.readFile shaderPath )

  let
    shaderCreateInfo :: Vulkan.ShaderModuleCreateInfo '[]
    shaderCreateInfo =
      Vulkan.ShaderModuleCreateInfo
        { Vulkan.next  = ()
        , Vulkan.flags = Vulkan.zero
        , Vulkan.code  = bytes
        }

  Vulkan.withShaderModule device shaderCreateInfo Nothing allocate

shaderInfo
  :: Shader
  -> Vulkan.ShaderModule
  -> Vulkan.PipelineShaderStageCreateInfo '[]
shaderInfo shaderStage shaderModule =
  Vulkan.PipelineShaderStageCreateInfo
    { Vulkan.next               = ()
    , Vulkan.flags              = Vulkan.zero
    , Vulkan.name               = "main"
    , Vulkan.module'            = shaderModule
    , Vulkan.stage              = stageFlag shaderStage
    , Vulkan.specializationInfo = Nothing
    }

createPipelineLayout
  :: MonadVulkan m
  => Vulkan.Device
  -> [ Vulkan.DescriptorSetLayout ]
  -> m Vulkan.PipelineLayout
createPipelineLayout device layouts = snd <$> Vulkan.withPipelineLayout device pipelineLayoutCreateInfo Nothing allocate

    where
      pipelineLayoutCreateInfo :: Vulkan.PipelineLayoutCreateInfo
      pipelineLayoutCreateInfo =
        Vulkan.PipelineLayoutCreateInfo
          { Vulkan.flags              = Vulkan.zero
          , Vulkan.setLayouts         = Boxed.Vector.fromList layouts
          , Vulkan.pushConstantRanges = Boxed.Vector.empty
          }

data VkPipelineInfo
  = VkPipelineInfo
  { pipelineExtent2D    :: Vulkan.Extent2D
  , pipelineSampleCount :: Vulkan.SampleCountFlagBits
  , vkPipelineLayout    :: Vulkan.PipelineLayout
  }


data VkPipeline
  = GraphicsPipeline   { vkPipeline :: Vulkan.Pipeline }
  | ComputePipeline    { vkPipeline :: Vulkan.Pipeline }
  | RayTracingPipeline { vkPipeline :: Vulkan.Pipeline }

createGraphicsPipeline
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.RenderPass
  -> VkPipelineInfo
  -> ShaderPipeline Vulkan.ShaderModule
  -> m ( ReleaseKey, VkPipeline )
createGraphicsPipeline device renderPass
  ( VkPipelineInfo extent sampleCount pipelineLayout )
  ( ShaderPipeline
      ( shaderModules :: PipelineStages info Vulkan.ShaderModule )
  ) = do
    logDebug "Creating graphics pipeline"

    second ( GraphicsPipeline . Boxed.Vector.head . snd ) <$>
      Vulkan.withGraphicsPipelines
        device
        ( Vulkan.NULL_HANDLE :: Vulkan.PipelineCache )
        ( Boxed.Vector.singleton $ Vulkan.SomeStruct pipelineCreateInfo )
        Nothing
        allocate

  where

      shaderInfos :: [ Vulkan.PipelineShaderStageCreateInfo '[] ]
      shaderInfos = map ( uncurry shaderInfo ) ( pipelineStages shaderModules )

      rasterizationCreateInfo :: Vulkan.PipelineRasterizationStateCreateInfo '[]
      rasterizationCreateInfo =
        Vulkan.PipelineRasterizationStateCreateInfo
          { Vulkan.next                     = ()
          , Vulkan.flags                       = Vulkan.zero
          , Vulkan.depthClampEnable         = False
          , Vulkan.rasterizerDiscardEnable  = False
          , Vulkan.polygonMode              = Vulkan.POLYGON_MODE_FILL
          , Vulkan.lineWidth                = 1
          , Vulkan.depthBiasEnable          = False
          , Vulkan.depthBiasSlopeFactor     = 0
          , Vulkan.depthBiasClamp           = 0
          , Vulkan.depthBiasConstantFactor  = 0
          , Vulkan.frontFace                = Vulkan.FRONT_FACE_COUNTER_CLOCKWISE
          , Vulkan.cullMode                 = Vulkan.CULL_MODE_BACK_BIT
          }

      primTop :: PrimitiveTopology Word32
      vertexInputState :: Vulkan.PipelineVertexInputStateCreateInfo '[]
      (primTop, vertexInputState) = topologyAndVertexInputStateInfo @info

      assemblyState       = assemblyInfo     primTop
      mbTessellationState = tessellationInfo primTop

      viewport :: Vulkan.Viewport
      viewport =
        Vulkan.Viewport
          { Vulkan.x        = 0
          , Vulkan.y        = 0
          , Vulkan.width    = fromIntegral $ ( Vulkan.width  :: Vulkan.Extent2D -> Word32 ) extent
          , Vulkan.height   = fromIntegral $ ( Vulkan.height :: Vulkan.Extent2D -> Word32 ) extent
          , Vulkan.minDepth = 0
          , Vulkan.maxDepth = 1
          }

      offset :: Vulkan.Offset2D
      offset =
        Vulkan.Offset2D
          { Vulkan.x = 0
          , Vulkan.y = 0
          }

      scissor :: Vulkan.Rect2D
      scissor =
        Vulkan.Rect2D
          { Vulkan.offset = offset
          , Vulkan.extent = extent
          }

      viewportState :: Vulkan.PipelineViewportStateCreateInfo '[]
      viewportState =
        Vulkan.PipelineViewportStateCreateInfo
          { Vulkan.next          = ()
          , Vulkan.flags         = Vulkan.zero
          , Vulkan.viewports     = Boxed.Vector.singleton viewport
          , Vulkan.viewportCount = 1
          , Vulkan.scissors      = Boxed.Vector.singleton scissor
          , Vulkan.scissorCount  = 1
          }

      multisampleState :: Vulkan.PipelineMultisampleStateCreateInfo '[]
      multisampleState =
        Vulkan.PipelineMultisampleStateCreateInfo
          { Vulkan.next                  = ()
          , Vulkan.flags                 = Vulkan.zero
          , Vulkan.sampleShadingEnable   = False
          , Vulkan.sampleMask            = Boxed.Vector.empty
          , Vulkan.minSampleShading      = 1
          , Vulkan.rasterizationSamples  = sampleCount
          , Vulkan.alphaToCoverageEnable = False
          , Vulkan.alphaToOneEnable      = False
          }

      attachmentState :: Vulkan.PipelineColorBlendAttachmentState
      attachmentState =
        Vulkan.PipelineColorBlendAttachmentState
          { Vulkan.blendEnable         = True
          , Vulkan.alphaBlendOp        = Vulkan.BLEND_OP_ADD
          , Vulkan.srcColorBlendFactor = Vulkan.BLEND_FACTOR_SRC_ALPHA
          , Vulkan.dstColorBlendFactor = Vulkan.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          , Vulkan.colorBlendOp        = Vulkan.BLEND_OP_ADD
          , Vulkan.srcAlphaBlendFactor = Vulkan.BLEND_FACTOR_ONE
          , Vulkan.dstAlphaBlendFactor = Vulkan.BLEND_FACTOR_ZERO
          , Vulkan.colorWriteMask      =
               (     Vulkan.COLOR_COMPONENT_R_BIT
                 .|. Vulkan.COLOR_COMPONENT_G_BIT
                 .|. Vulkan.COLOR_COMPONENT_B_BIT
                 .|. Vulkan.COLOR_COMPONENT_A_BIT
               )
          }


      colorBlendState :: Vulkan.PipelineColorBlendStateCreateInfo '[]
      colorBlendState =
        Vulkan.PipelineColorBlendStateCreateInfo
          { Vulkan.next           = ()
          , Vulkan.flags          = Vulkan.zero
          , Vulkan.logicOpEnable  = False
          , Vulkan.logicOp        = Vulkan.LOGIC_OP_COPY
          , Vulkan.blendConstants = ( 0, 0, 0, 0 )
          , Vulkan.attachments    = Boxed.Vector.singleton attachmentState
          }

      nullStencilOp :: Vulkan.StencilOpState
      nullStencilOp =
        Vulkan.StencilOpState
          { Vulkan.reference   = 0
          , Vulkan.writeMask   = 0
          , Vulkan.compareMask = 0
          , Vulkan.compareOp   = Vulkan.COMPARE_OP_EQUAL
          , Vulkan.depthFailOp = Vulkan.STENCIL_OP_KEEP
          , Vulkan.passOp      = Vulkan.STENCIL_OP_KEEP
          , Vulkan.failOp      = Vulkan.STENCIL_OP_KEEP
          }

      depthStencilState :: Vulkan.PipelineDepthStencilStateCreateInfo
      depthStencilState =
        Vulkan.PipelineDepthStencilStateCreateInfo
          { Vulkan.flags                 = Vulkan.zero
          , Vulkan.depthTestEnable       = True
          , Vulkan.depthWriteEnable      = True
          , Vulkan.depthCompareOp        = Vulkan.COMPARE_OP_LESS_OR_EQUAL
          , Vulkan.depthBoundsTestEnable = False
          , Vulkan.maxDepthBounds        = 1
          , Vulkan.minDepthBounds        = 0
          , Vulkan.stencilTestEnable     = False
          , Vulkan.front                 = nullStencilOp
          , Vulkan.back                  = nullStencilOp
          }

      pipelineCreateInfo :: Vulkan.GraphicsPipelineCreateInfo '[]
      pipelineCreateInfo =
        Vulkan.GraphicsPipelineCreateInfo
          { Vulkan.next               = ()
          , Vulkan.flags              = Vulkan.zero
          , Vulkan.stages             = Boxed.Vector.fromList ( map Vulkan.SomeStruct shaderInfos )
          , Vulkan.vertexInputState   = Just ( Vulkan.SomeStruct vertexInputState )
          , Vulkan.tessellationState  = fmap Vulkan.SomeStruct mbTessellationState
          , Vulkan.basePipelineIndex  = 0
          , Vulkan.basePipelineHandle = Vulkan.NULL_HANDLE
          , Vulkan.subpass            = 0
          , Vulkan.renderPass         = renderPass
          , Vulkan.layout             = pipelineLayout
          , Vulkan.rasterizationState = Vulkan.SomeStruct rasterizationCreateInfo
          , Vulkan.inputAssemblyState = Just assemblyState
          , Vulkan.viewportState      = Just ( Vulkan.SomeStruct viewportState )
          , Vulkan.multisampleState   = Just ( Vulkan.SomeStruct multisampleState )
          , Vulkan.colorBlendState    = Just ( Vulkan.SomeStruct colorBlendState )
          , Vulkan.depthStencilState  = Just depthStencilState
          , Vulkan.dynamicState       = Nothing
          }

createComputePipeline
  :: MonadVulkan m
  => Vulkan.Device
  -> Vulkan.PipelineLayout
  -> Vulkan.ShaderModule
  -> m ( ReleaseKey, VkPipeline )
createComputePipeline device pipelineLayout shaderModule = do
  logDebug "Creating compute pipeline"

  second ( ComputePipeline . Boxed.Vector.head . snd ) <$>
    Vulkan.withComputePipelines
      device
      ( Vulkan.NULL_HANDLE :: Vulkan.PipelineCache )
      ( Boxed.Vector.singleton $ Vulkan.SomeStruct pipelineCreateInfo )
      Nothing
      allocate

  where
    computeShaderInfo :: Vulkan.PipelineShaderStageCreateInfo '[]
    computeShaderInfo = shaderInfo ComputeShader shaderModule

    pipelineCreateInfo :: Vulkan.ComputePipelineCreateInfo '[]
    pipelineCreateInfo =
      Vulkan.ComputePipelineCreateInfo
        { Vulkan.next               = ()
        , Vulkan.flags              = Vulkan.zero
        , Vulkan.stage              = Vulkan.SomeStruct computeShaderInfo
        , Vulkan.layout             = pipelineLayout
        , Vulkan.basePipelineIndex  = 0
        , Vulkan.basePipelineHandle = Vulkan.NULL_HANDLE
        }
