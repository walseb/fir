{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Vulkan.Pipeline where

-- base
import Control.Monad.IO.Class
  ( liftIO )
import Data.Bits
  ( (.|.) )
import Data.Foldable
  ( toList )
import Data.Functor
  ( (<&>) )
import Data.Maybe
  ( fromMaybe )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import qualified Foreign
import GHC.TypeNats
  ( Nat )

-- bytestring
import qualified Data.ByteString

-- managed
import Control.Monad.Managed
  ( MonadManaged )

-- vulkan-api
import Graphics.Vulkan.Marshal.Create
  ( (&*) )
import qualified Graphics.Vulkan as Vulkan
import qualified Graphics.Vulkan.Core_1_0 as Vulkan
import qualified Graphics.Vulkan.Marshal.Create as Vulkan

-- fir
import FIR
  ( Shader(..), ShaderPipeline(..)
  , pipelineInfoShaders
  , PrimitiveConnectedness(..)
  , PrimitiveTopology(..)
  , (:->)((:->))
  , Known, knownValue
  , BindingStrides, VertexLocationDescriptions
  , ImageFormat
  )

-- fir-examples
import Vulkan.Formats
  ( simpleFormat )
import Vulkan.Monad

-------------------------------------------------------------------------

topology :: PrimitiveTopology n -> Vulkan.VkPrimitiveTopology
topology Points                    = Vulkan.VK_PRIMITIVE_TOPOLOGY_POINT_LIST
topology (Line List              ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_LINE_LIST
topology (Line Strip             ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
topology (Line Fan               ) = error "Invalid topology: fan of lines."
topology (Triangle List          ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
topology (Triangle Strip         ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
topology (Triangle Fan           ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
topology (Line AdjacencyList     ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
topology (Line AdjacencyStrip    ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
topology (Triangle AdjacencyList ) = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
topology (Triangle AdjacencyStrip) = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
topology (PatchesOfSize         _) = Vulkan.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST

assemblyInfo
  :: PrimitiveTopology n -> Vulkan.VkPipelineInputAssemblyStateCreateInfo
assemblyInfo primTop =
  Vulkan.createVk
    (  Vulkan.set @"sType"    Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
    &* Vulkan.set @"pNext"    Vulkan.VK_NULL
    &* Vulkan.set @"topology" ( topology primTop )
    &* Vulkan.set @"primitiveRestartEnable" Vulkan.VK_FALSE
    )

tessellationInfo
  :: PrimitiveTopology Word32 -> Maybe (Vulkan.VkPipelineTessellationStateCreateInfo)
tessellationInfo (PatchesOfSize pts) =
  (Just . Vulkan.createVk )
     (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
     &* Vulkan.set @"pNext" Vulkan.VK_NULL
     &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
     &* Vulkan.set @"patchControlPoints" (fromIntegral pts)
     )
tessellationInfo _ = Nothing

vertexInputStateInfo
  :: forall (bds :: [Nat :-> Nat]) (descs :: VertexLocationDescriptions)
  . ( Known BindingStrides bds
    , Known VertexLocationDescriptions descs
    )
  => Vulkan.VkPipelineVertexInputStateCreateInfo
vertexInputStateInfo =
  let
    bindings :: [ Word32 :-> Word32 ]
    bindings = knownValue @bds

    attributes :: [ Word32 :-> (Word32, Word32, ImageFormat Word32) ]
    attributes = knownValue @descs

    computeVulkanFormat :: ImageFormat Word32 -> Vulkan.VkFormat
    computeVulkanFormat fmt
      = fromMaybe
          ( error $ "Unsupported format " ++ show fmt ++ " used as a vertex input attribute." )
          ( simpleFormat fmt )

    vertexBindingDescriptions :: [ Vulkan.VkVertexInputBindingDescription ]
    vertexBindingDescriptions =
      bindings <&> \ ( binding :-> stride ) ->
          Vulkan.createVk
            (  Vulkan.set @"binding"   binding
            &* Vulkan.set @"stride"    stride
            &* Vulkan.set @"inputRate" Vulkan.VK_VERTEX_INPUT_RATE_VERTEX
            )

    vertexAttributeDescriptions :: [ Vulkan.VkVertexInputAttributeDescription ]
    vertexAttributeDescriptions =
      attributes <&> \ ( location :-> (binding, offset, format) ) ->
        Vulkan.createVk @Vulkan.VkVertexInputAttributeDescription
          (  Vulkan.set @"location" location
          &* Vulkan.set @"binding"  binding
          &* Vulkan.set @"format"   (computeVulkanFormat format)
          &* Vulkan.set @"offset"   offset
          )

  in
    Vulkan.createVk
      (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.setListCountAndRef
              @"vertexBindingDescriptionCount"
              @"pVertexBindingDescriptions"
              vertexBindingDescriptions
      &* Vulkan.setListCountAndRef
              @"vertexAttributeDescriptionCount"
              @"pVertexAttributeDescriptions"
              vertexAttributeDescriptions
      )


stageFlag :: Shader -> Vulkan.VkShaderStageFlagBits
stageFlag VertexShader                 = Vulkan.VK_SHADER_STAGE_VERTEX_BIT
stageFlag TessellationControlShader    = Vulkan.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
stageFlag TessellationEvaluationShader = Vulkan.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
stageFlag GeometryShader               = Vulkan.VK_SHADER_STAGE_GEOMETRY_BIT
stageFlag FragmentShader               = Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
stageFlag ComputeShader                = Vulkan.VK_SHADER_STAGE_COMPUTE_BIT

shaderExtension :: Shader -> String
shaderExtension VertexShader                 = "vert"
shaderExtension TessellationControlShader    = "tesc"
shaderExtension TessellationEvaluationShader = "tese"
shaderExtension GeometryShader               = "geom"
shaderExtension FragmentShader               = "frag"
shaderExtension ComputeShader                = "comp"

loadShader :: MonadManaged m => Vulkan.VkDevice -> FilePath -> m Vulkan.VkShaderModule
loadShader device shaderPath = do

  bytes <-
    liftIO ( Data.ByteString.readFile shaderPath )

  managedVulkanResource
    ( \a b ->
        Data.ByteString.useAsCStringLen bytes $ \( bytesPtr, len ) ->
          let
            createInfo =
              Vulkan.createVk
                (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
                &* Vulkan.set @"pNext" Vulkan.VK_NULL
                &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
                &* Vulkan.set @"pCode"    ( Foreign.castPtr bytesPtr )
                &* Vulkan.set @"codeSize" ( fromIntegral len )
                )

          in
          Vulkan.vkCreateShaderModule device ( Vulkan.unsafePtr createInfo ) a b
    )
    ( Vulkan.vkDestroyShaderModule device )

createShaderInfo
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Shader 
  -> FilePath
  -> m Vulkan.VkPipelineShaderStageCreateInfo
createShaderInfo device shaderStage shaderPath = do
  shader <- loadShader device shaderPath
  let shaderModule :: Vulkan.VkPipelineShaderStageCreateInfo
      shaderModule =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
          &* Vulkan.set @"pNext"        Vulkan.VK_NULL
          &* Vulkan.set @"flags"        Vulkan.VK_ZERO_FLAGS
          &* Vulkan.setStrRef @"pName"  "main"
          &* Vulkan.set @"module"       shader
          &* Vulkan.set @"stage"        (stageFlag shaderStage)
          )
  pure shaderModule

createPipelineLayout
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorSetLayout
  -> m Vulkan.VkPipelineLayout
createPipelineLayout device layout0 =
  managedVulkanResource
    ( Vulkan.vkCreatePipelineLayout  device ( Vulkan.unsafePtr pipelineLayoutCreateInfo ) )
    ( Vulkan.vkDestroyPipelineLayout device )

    where
      pipelineLayoutCreateInfo :: Vulkan.VkPipelineLayoutCreateInfo
      pipelineLayoutCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.setListCountAndRef
                @"setLayoutCount"
                @"pSetLayouts"
                [ layout0 ]
          &* Vulkan.setListCountAndRef
                @"pushConstantRangeCount"
                @"pPushConstantRanges"
                [ ]
          )

createGraphicsPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkDescriptorSetLayout
  -> ShaderPipeline
  -> m ( Vulkan.VkPipeline, Vulkan.VkPipelineLayout )
createGraphicsPipeline device renderPass extent layout0
  ( WithVertexInput
      (_ :: Proxy bds)
      (_ :: Proxy descs)
      (_ :: Proxy top)
      pipelineWithInfo
  ) = do
    pipelineLayout <-
      createPipelineLayout device layout0

    let
      shaderPaths :: [(Shader, String)]
      shaderPaths = pipelineInfoShaders pipelineWithInfo

    shaders :: [ Vulkan.VkPipelineShaderStageCreateInfo ]
       <- traverse ( uncurry (createShaderInfo device) ) shaderPaths

    let
      vertexInputState :: Vulkan.VkPipelineVertexInputStateCreateInfo
      vertexInputState = vertexInputStateInfo @bds @descs

      rasterizationCreateInfo :: Vulkan.VkPipelineRasterizationStateCreateInfo
      rasterizationCreateInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
          &* Vulkan.set @"pNext"                   Vulkan.VK_NULL
          &* Vulkan.set @"depthClampEnable"        Vulkan.VK_FALSE
          &* Vulkan.set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
          &* Vulkan.set @"polygonMode"             Vulkan.VK_POLYGON_MODE_FILL
          &* Vulkan.set @"lineWidth"               1
          &* Vulkan.set @"depthBiasEnable"         Vulkan.VK_FALSE
          &* Vulkan.set @"depthBiasSlopeFactor"    0
          &* Vulkan.set @"depthBiasClamp"          0
          &* Vulkan.set @"depthBiasConstantFactor" 0
          &* Vulkan.set @"frontFace"               Vulkan.VK_FRONT_FACE_COUNTER_CLOCKWISE
          &* Vulkan.set @"cullMode"                Vulkan.VK_CULL_MODE_BACK_BIT
          )

      primTop :: PrimitiveTopology Word32
      primTop = knownValue @top

      assemblyState       = assemblyInfo     primTop
      mbTessellationState = tessellationInfo primTop

      viewport :: Vulkan.VkViewport
      viewport =
        Vulkan.createVk
          (  Vulkan.set @"x"        0
          &* Vulkan.set @"y"        0
          &* Vulkan.set @"width"    ( fromIntegral $ Vulkan.getField @"width"  extent )
          &* Vulkan.set @"height"   ( fromIntegral $ Vulkan.getField @"height" extent )
          &* Vulkan.set @"minDepth" 0
          &* Vulkan.set @"maxDepth" 1
          )

      offset :: Vulkan.VkOffset2D
      offset =
        Vulkan.createVk
          (  Vulkan.set @"x" 0
          &* Vulkan.set @"y" 0
          )

      scissor :: Vulkan.VkRect2D
      scissor =
        Vulkan.createVk
          (  Vulkan.set @"offset" offset
          &* Vulkan.set @"extent" extent
          )

      viewportState :: Vulkan.VkPipelineViewportStateCreateInfo
      viewportState =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.setListCountAndRef @"viewportCount" @"pViewports" [ viewport ]
          &* Vulkan.setListCountAndRef @"scissorCount"  @"pScissors"  [ scissor  ]
          )

      multisampleState :: Vulkan.VkPipelineMultisampleStateCreateInfo
      multisampleState =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
          &* Vulkan.set @"minSampleShading"     1
          &* Vulkan.set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
          &* Vulkan.set @"pNext"                Vulkan.VK_NULL
          )

      attachmentState :: Vulkan.VkPipelineColorBlendAttachmentState
      attachmentState =
        Vulkan.createVk
          (  Vulkan.set @"blendEnable"         Vulkan.VK_TRUE
          &* Vulkan.set @"alphaBlendOp"        Vulkan.VK_BLEND_OP_ADD
          &* Vulkan.set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_SRC_ALPHA
          &* Vulkan.set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
          &* Vulkan.set @"colorBlendOp"        Vulkan.VK_BLEND_OP_ADD
          &* Vulkan.set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
          &* Vulkan.set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
          &* Vulkan.set @"colorWriteMask"
               (     Vulkan.VK_COLOR_COMPONENT_R_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
                 .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
               )
          )


      colorBlendState :: Vulkan.VkPipelineColorBlendStateCreateInfo
      colorBlendState =
        Vulkan.createVk
          (  Vulkan.set @"sType"   Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
          &* Vulkan.set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
          &* Vulkan.set @"pNext"   Vulkan.VK_NULL
          &* Vulkan.setAt @"blendConstants" @0 0
          &* Vulkan.setAt @"blendConstants" @1 0
          &* Vulkan.setAt @"blendConstants" @2 0
          &* Vulkan.setAt @"blendConstants" @3 0
          &* Vulkan.setListCountAndRef @"attachmentCount" @"pAttachments" [ attachmentState ]
          )

      nullStencilOp :: Vulkan.VkStencilOpState
      nullStencilOp =
        Vulkan.createVk
          (  Vulkan.set @"reference"   0
          &* Vulkan.set @"writeMask"   0
          &* Vulkan.set @"compareMask" 0
          &* Vulkan.set @"compareOp"   Vulkan.VK_COMPARE_OP_EQUAL
          &* Vulkan.set @"depthFailOp" Vulkan.VK_STENCIL_OP_KEEP
          &* Vulkan.set @"passOp"      Vulkan.VK_STENCIL_OP_KEEP
          &* Vulkan.set @"failOp"      Vulkan.VK_STENCIL_OP_KEEP
          )

      depthStencilState :: Vulkan.VkPipelineDepthStencilStateCreateInfo
      depthStencilState =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.set @"depthTestEnable"   Vulkan.VK_TRUE
          &* Vulkan.set @"depthWriteEnable"  Vulkan.VK_TRUE
          &* Vulkan.set @"depthCompareOp"    Vulkan.VK_COMPARE_OP_LESS_OR_EQUAL
          &* Vulkan.set @"depthBoundsTestEnable" Vulkan.VK_FALSE
          &* Vulkan.set @"maxDepthBounds"    1
          &* Vulkan.set @"minDepthBounds"    0
          &* Vulkan.set @"stencilTestEnable" Vulkan.VK_FALSE
          &* Vulkan.set @"front" nullStencilOp
          &* Vulkan.set @"back"  nullStencilOp
          )

      createInfo =
        Vulkan.createVk
          (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.setListCountAndRef @"stageCount" @"pStages" shaders
          &* Vulkan.setVkRef   @"pVertexInputState"   vertexInputState
          &* Vulkan.setListRef @"pTessellationState"  (toList mbTessellationState)
          &* Vulkan.set        @"basePipelineIndex"   0
          &* Vulkan.set        @"subpass"             0
          &* Vulkan.set        @"renderPass"          renderPass
          &* Vulkan.set        @"layout"              pipelineLayout
          &* Vulkan.setVkRef   @"pRasterizationState" rasterizationCreateInfo
          &* Vulkan.setVkRef   @"pInputAssemblyState" assemblyState
          &* Vulkan.setVkRef   @"pViewportState"      viewportState
          &* Vulkan.setVkRef   @"pMultisampleState"   multisampleState
          &* Vulkan.setVkRef   @"pColorBlendState"    colorBlendState
          &* Vulkan.setVkRef   @"pDepthStencilState"  depthStencilState
          )

    pipeline <-
      managedVulkanResource
        ( Vulkan.vkCreateGraphicsPipelines
            device
            Vulkan.vkNullPtr
            1
            ( Vulkan.unsafePtr createInfo )
        )
        ( Vulkan.vkDestroyPipeline device )

    return ( pipeline, pipelineLayout )

createComputePipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorSetLayout
  -> FilePath
  -> m ( Vulkan.VkPipeline, Vulkan.VkPipelineLayout )
createComputePipeline device layout0 shaderPath = do

  pipelineLayout <-
    createPipelineLayout device layout0

  computeShader <- createShaderInfo device ComputeShader shaderPath

  let
    createInfo :: Vulkan.VkComputePipelineCreateInfo
    createInfo = Vulkan.createVk
      (  Vulkan.set @"sType"  Vulkan.VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
      &* Vulkan.set @"pNext"  Vulkan.VK_NULL
      &* Vulkan.set @"flags"  Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"stage"  computeShader
      &* Vulkan.set @"layout" pipelineLayout
      &* Vulkan.set @"basePipelineIndex" 0
      )

  pipeline <-
    managedVulkanResource
      ( Vulkan.vkCreateComputePipelines
          device
          Vulkan.vkNullPtr
          1
          ( Vulkan.unsafePtr createInfo )

      )
      ( Vulkan.vkDestroyPipeline device )

  return ( pipeline, pipelineLayout )
