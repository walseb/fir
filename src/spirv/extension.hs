{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}

module SPIRV.Extension where

-- base
import Data.Word
  ( Word32 )

-- text-utf8
import Data.Text
  ( Text )

-- fir
import Data.Binary.Class.Put
  ( Put )

--------------------------------------------------
-- extended instruction sets

data ExtInst
  = OpenCL
  | GLSL
  deriving ( Show, Eq, Ord, Enum, Bounded )

extInstName :: ExtInst -> Text
extInstName OpenCL = "OpenCL.std.100"
extInstName GLSL   = "GLSL.std.450"

--------------------------------------------------
-- extensions

newtype Extension = Extension Word32
  deriving ( Eq, Ord, Put )

instance Show Extension where
  show extension = "Extension " ++ showExtension extension

pattern SPV_AMD_shader_explicit_vertex_parameter :: Extension
pattern SPV_AMD_shader_explicit_vertex_parameter = Extension 1
pattern SPV_AMD_shader_trinary_minmax :: Extension
pattern SPV_AMD_shader_trinary_minmax = Extension 2
pattern SPV_AMD_gcn_shader :: Extension
pattern SPV_AMD_gcn_shader = Extension 3
pattern SPV_KHR_shader_ballot :: Extension
pattern SPV_KHR_shader_ballot = Extension 4
pattern SPV_AMD_shader_ballot :: Extension
pattern SPV_AMD_shader_ballot = Extension 5
pattern SPV_AMD_gpu_shader_half_float :: Extension
pattern SPV_AMD_gpu_shader_half_float = Extension 6
pattern SPV_KHR_shader_draw_parameters :: Extension
pattern SPV_KHR_shader_draw_parameters = Extension 7
pattern SPV_KHR_subgroup_vote :: Extension
pattern SPV_KHR_subgroup_vote = Extension 8
pattern SPV_KHR_16bit_storage :: Extension
pattern SPV_KHR_16bit_storage = Extension 9
pattern SPV_KHR_device_group :: Extension
pattern SPV_KHR_device_group = Extension 10
pattern SPV_KHR_multiview :: Extension
pattern SPV_KHR_multiview = Extension 11
pattern SPV_NVX_multiview_per_view_attributes :: Extension
pattern SPV_NVX_multiview_per_view_attributes = Extension 12
pattern SPV_NV_viewport_array2 :: Extension
pattern SPV_NV_viewport_array2 = Extension 13
pattern SPV_NV_stereo_view_rendering :: Extension
pattern SPV_NV_stereo_view_rendering = Extension 14
pattern SPV_NV_sample_mask_override_coverage :: Extension
pattern SPV_NV_sample_mask_override_coverage = Extension 15
pattern SPV_NV_geometry_shader_passthrough :: Extension
pattern SPV_NV_geometry_shader_passthrough = Extension 16
pattern SPV_AMD_texture_gather_bias_lod :: Extension
pattern SPV_AMD_texture_gather_bias_lod = Extension 17
pattern SPV_KHR_storage_buffer_storage_class :: Extension
pattern SPV_KHR_storage_buffer_storage_class = Extension 18
pattern SPV_KHR_variable_pointers :: Extension
pattern SPV_KHR_variable_pointers = Extension 19
pattern SPV_AMD_gpu_shader_int16 :: Extension
pattern SPV_AMD_gpu_shader_int16 = Extension 20
pattern SPV_KHR_post_depth_coverage :: Extension
pattern SPV_KHR_post_depth_coverage = Extension 21
pattern SPV_KHR_shader_atomic_counter_ops :: Extension
pattern SPV_KHR_shader_atomic_counter_ops = Extension 22
pattern SPV_EXT_shader_stencil_export :: Extension
pattern SPV_EXT_shader_stencil_export = Extension 23
pattern SPV_EXT_shader_viewport_index_layer :: Extension
pattern SPV_EXT_shader_viewport_index_layer = Extension 24
pattern SPV_AMD_shader_image_load_store_lod :: Extension
pattern SPV_AMD_shader_image_load_store_lod = Extension 25
pattern SPV_AMD_shader_fragment_mask :: Extension
pattern SPV_AMD_shader_fragment_mask = Extension 26
pattern SPV_EXT_fragment_fully_covered :: Extension
pattern SPV_EXT_fragment_fully_covered = Extension 27
pattern SPV_AMD_gpu_shader_half_float_fetch :: Extension
pattern SPV_AMD_gpu_shader_half_float_fetch = Extension 28
pattern SPV_GOOGLE_decorate_string :: Extension
pattern SPV_GOOGLE_decorate_string = Extension 29
pattern SPV_GOOGLE_hlsl_functionality1 :: Extension
pattern SPV_GOOGLE_hlsl_functionality1 = Extension 30
pattern SPV_NV_shader_subgroup_partitioned :: Extension
pattern SPV_NV_shader_subgroup_partitioned = Extension 31
pattern SPV_EXT_descriptor_indexing :: Extension
pattern SPV_EXT_descriptor_indexing = Extension 32
pattern SPV_KHR_8bit_storage :: Extension
pattern SPV_KHR_8bit_storage = Extension 33
pattern SPV_KHR_vulkan_memory_model :: Extension
pattern SPV_KHR_vulkan_memory_model = Extension 34
pattern SPV_NVX_raytracing :: Extension
pattern SPV_NVX_raytracing = Extension 35
pattern SPV_NV_compute_shader_derivatives :: Extension
pattern SPV_NV_compute_shader_derivatives = Extension 36
pattern SPV_NV_fragment_shader_barycentric :: Extension
pattern SPV_NV_fragment_shader_barycentric = Extension 37
pattern SPV_NV_mesh_shader :: Extension
pattern SPV_NV_mesh_shader = Extension 38
pattern SPV_NV_shader_image_footprint :: Extension
pattern SPV_NV_shader_image_footprint = Extension 39
pattern SPV_NV_shading_rate :: Extension
pattern SPV_NV_shading_rate = Extension 40


showExtension :: Extension -> String
showExtension SPV_AMD_shader_explicit_vertex_parameter = "SPV_AMD_shader_explicit_vertex_parameter"
showExtension SPV_AMD_shader_trinary_minmax = "SPV_AMD_shader_trinary_minmax"
showExtension SPV_AMD_gcn_shader = "SPV_AMD_gcn_shader"
showExtension SPV_KHR_shader_ballot = "SPV_KHR_shader_ballot"
showExtension SPV_AMD_shader_ballot = "SPV_AMD_shader_ballot"
showExtension SPV_AMD_gpu_shader_half_float = "SPV_AMD_gpu_shader_half_float"
showExtension SPV_KHR_shader_draw_parameters = "SPV_KHR_shader_draw_parameters"
showExtension SPV_KHR_subgroup_vote = "SPV_KHR_subgroup_vote"
showExtension SPV_KHR_16bit_storage = "SPV_KHR_16bit_storage"
showExtension SPV_KHR_device_group = "SPV_KHR_device_group"
showExtension SPV_KHR_multiview = "SPV_KHR_multiview"
showExtension SPV_NVX_multiview_per_view_attributes = "SPV_NVX_multiview_per_view_attributes"
showExtension SPV_NV_viewport_array2 = "SPV_NV_viewport_array2"
showExtension SPV_NV_stereo_view_rendering = "SPV_NV_stereo_view_rendering"
showExtension SPV_NV_sample_mask_override_coverage = "SPV_NV_sample_mask_override_coverage"
showExtension SPV_NV_geometry_shader_passthrough = "SPV_NV_geometry_shader_passthrough"
showExtension SPV_AMD_texture_gather_bias_lod = "SPV_AMD_texture_gather_bias_lod"
showExtension SPV_KHR_storage_buffer_storage_class = "SPV_KHR_storage_buffer_storage_class"
showExtension SPV_KHR_variable_pointers = "SPV_KHR_variable_pointers"
showExtension SPV_AMD_gpu_shader_int16 = "SPV_AMD_gpu_shader_int16"
showExtension SPV_KHR_post_depth_coverage = "SPV_KHR_post_depth_coverage"
showExtension SPV_KHR_shader_atomic_counter_ops = "SPV_KHR_shader_atomic_counter_ops"
showExtension SPV_EXT_shader_stencil_export = "SPV_EXT_shader_stencil_export"
showExtension SPV_EXT_shader_viewport_index_layer = "SPV_EXT_shader_viewport_index_layer"
showExtension SPV_AMD_shader_image_load_store_lod = "SPV_AMD_shader_image_load_store_lod"
showExtension SPV_AMD_shader_fragment_mask = "SPV_AMD_shader_fragment_mask"
showExtension SPV_EXT_fragment_fully_covered = "SPV_EXT_fragment_fully_covered"
showExtension SPV_AMD_gpu_shader_half_float_fetch = "SPV_AMD_gpu_shader_half_float_fetch"
showExtension SPV_GOOGLE_decorate_string = "SPV_GOOGLE_decorate_string"
showExtension SPV_GOOGLE_hlsl_functionality1 = "SPV_GOOGLE_hlsl_functionality1"
showExtension SPV_NV_shader_subgroup_partitioned = "SPV_NV_shader_subgroup_partitioned"
showExtension SPV_EXT_descriptor_indexing = "SPV_EXT_descriptor_indexing"
showExtension SPV_KHR_8bit_storage = "SPV_KHR_8bit_storage"
showExtension SPV_KHR_vulkan_memory_model = "SPV_KHR_vulkan_memory_model"
showExtension SPV_NVX_raytracing = "SPV_NVX_raytracing"
showExtension SPV_NV_compute_shader_derivatives = "SPV_NV_compute_shader_derivatives"
showExtension SPV_NV_fragment_shader_barycentric = "SPV_NV_fragment_shader_barycentric"
showExtension SPV_NV_mesh_shader = "SPV_NV_mesh_shader"
showExtension SPV_NV_shader_image_footprint = "SPV_NV_shader_image_footprint"
showExtension SPV_NV_shading_rate = "SPV_NV_shading_rate"
showExtension (Extension i) = show i