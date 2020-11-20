{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}

{-|
Module: SPIRV.Extension

This module enumerates SPIR-V extensions as well as extended instruction sets.
These are used only at the value level.

See SPIR-V specification §2.10 __Extended Instruction Sets__,
as well as the SPIR-V registry for a complete list of SPIR-V extensions.

-}

module SPIRV.Extension where

-- template-haskell
import Language.Haskell.TH.Syntax
  ( Lift )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import Instances.TH.Lift
  ( ) -- Lift instance for ShortText
import Data.Binary.Class.Put
  ( Put )

--------------------------------------------------
-- extended instruction sets

data ExtInst
  = OpenCL
  | GLSL
  | DebugPrintf
  deriving stock ( Show, Eq, Ord, Enum, Bounded )

extInstName :: ExtInst -> ShortText
extInstName OpenCL      = "OpenCL.std"
extInstName GLSL        = "GLSL.std.450"
extInstName DebugPrintf = "NonSemantic.DebugPrintf"

--------------------------------------------------
-- extensions

newtype Extension = Extension ShortText
  deriving newtype ( Eq, Ord, Put )
  deriving stock   ( Show, Lift )

pattern SPV_AMD_shader_explicit_vertex_parameter :: Extension
pattern SPV_AMD_shader_explicit_vertex_parameter = Extension "SPV_AMD_shader_explicit_vertex_parameter"
pattern SPV_AMD_shader_trinary_minmax :: Extension
pattern SPV_AMD_shader_trinary_minmax = Extension "SPV_AMD_shader_trinary_minmax"
pattern SPV_AMD_gcn_shader :: Extension
pattern SPV_AMD_gcn_shader = Extension "SPV_AMD_gcn_shader"
pattern SPV_KHR_shader_ballot :: Extension
pattern SPV_KHR_shader_ballot = Extension "SPV_KHR_shader_ballot"
pattern SPV_AMD_shader_ballot :: Extension
pattern SPV_AMD_shader_ballot = Extension "SPV_AMD_shader_ballot"
pattern SPV_AMD_gpu_shader_half_float :: Extension
pattern SPV_AMD_gpu_shader_half_float = Extension "SPV_AMD_gpu_shader_half_float"
pattern SPV_KHR_shader_draw_parameters :: Extension
pattern SPV_KHR_shader_draw_parameters = Extension "SPV_KHR_shader_draw_parameters"
pattern SPV_KHR_subgroup_vote :: Extension
pattern SPV_KHR_subgroup_vote = Extension "SPV_KHR_subgroup_vote"
pattern SPV_KHR_16bit_storage :: Extension
pattern SPV_KHR_16bit_storage = Extension "SPV_KHR_16bit_storage"
pattern SPV_KHR_device_group :: Extension
pattern SPV_KHR_device_group = Extension "SPV_KHR_device_group"
pattern SPV_KHR_multiview :: Extension
pattern SPV_KHR_multiview = Extension "SPV_KHR_multiview"
pattern SPV_NVX_multiview_per_view_attributes :: Extension
pattern SPV_NVX_multiview_per_view_attributes = Extension "SPV_NVX_multiview_per_view_attributes"
pattern SPV_NV_viewport_array2 :: Extension
pattern SPV_NV_viewport_array2 = Extension "SPV_NV_viewport_array2"
pattern SPV_NV_stereo_view_rendering :: Extension
pattern SPV_NV_stereo_view_rendering = Extension "SPV_NV_stereo_view_rendering"
pattern SPV_NV_sample_mask_override_coverage :: Extension
pattern SPV_NV_sample_mask_override_coverage = Extension "SPV_NV_sample_mask_override_coverage"
pattern SPV_NV_geometry_shader_passthrough :: Extension
pattern SPV_NV_geometry_shader_passthrough = Extension "SPV_NV_geometry_shader_passthrough"
pattern SPV_AMD_texture_gather_bias_lod :: Extension
pattern SPV_AMD_texture_gather_bias_lod = Extension "SPV_AMD_texture_gather_bias_lod"
pattern SPV_KHR_storage_buffer_storage_class :: Extension
pattern SPV_KHR_storage_buffer_storage_class = Extension "SPV_KHR_storage_buffer_storage_class"
pattern SPV_KHR_variable_pointers :: Extension
pattern SPV_KHR_variable_pointers = Extension "SPV_KHR_variable_pointers"
pattern SPV_AMD_gpu_shader_int16 :: Extension
pattern SPV_AMD_gpu_shader_int16 = Extension "SPV_AMD_gpu_shader_int16"
pattern SPV_KHR_post_depth_coverage :: Extension
pattern SPV_KHR_post_depth_coverage = Extension "SPV_KHR_post_depth_coverage"
pattern SPV_KHR_shader_atomic_counter_ops :: Extension
pattern SPV_KHR_shader_atomic_counter_ops = Extension "SPV_KHR_shader_atomic_counter_ops"
pattern SPV_EXT_shader_stencil_export :: Extension
pattern SPV_EXT_shader_stencil_export = Extension "SPV_EXT_shader_stencil_export"
pattern SPV_EXT_shader_viewport_index_layer :: Extension
pattern SPV_EXT_shader_viewport_index_layer = Extension "SPV_EXT_shader_viewport_index_layer"
pattern SPV_AMD_shader_image_load_store_lod :: Extension
pattern SPV_AMD_shader_image_load_store_lod = Extension "SPV_AMD_shader_image_load_store_lod"
pattern SPV_AMD_shader_fragment_mask :: Extension
pattern SPV_AMD_shader_fragment_mask = Extension "SPV_AMD_shader_fragment_mask"
pattern SPV_EXT_fragment_fully_covered :: Extension
pattern SPV_EXT_fragment_fully_covered = Extension "SPV_EXT_fragment_fully_covered"
pattern SPV_AMD_gpu_shader_half_float_fetch :: Extension
pattern SPV_AMD_gpu_shader_half_float_fetch = Extension "SPV_AMD_gpu_shader_half_float_fetch"
pattern SPV_GOOGLE_decorate_string :: Extension
pattern SPV_GOOGLE_decorate_string = Extension "SPV_GOOGLE_decorate_string"
pattern SPV_GOOGLE_hlsl_functionality1 :: Extension
pattern SPV_GOOGLE_hlsl_functionality1 = Extension "SPV_GOOGLE_hlsl_functionality1"
pattern SPV_NV_shader_subgroup_partitioned :: Extension
pattern SPV_NV_shader_subgroup_partitioned = Extension "SPV_NV_shader_subgroup_partitioned"
pattern SPV_EXT_descriptor_indexing :: Extension
pattern SPV_EXT_descriptor_indexing = Extension "SPV_EXT_descriptor_indexing"
pattern SPV_KHR_8bit_storage :: Extension
pattern SPV_KHR_8bit_storage = Extension "SPV_KHR_8bit_storage"
pattern SPV_KHR_vulkan_memory_model :: Extension
pattern SPV_KHR_vulkan_memory_model = Extension "SPV_KHR_vulkan_memory_model"
pattern SPV_NV_ray_tracing :: Extension
pattern SPV_NV_ray_tracing = Extension "SPV_NV_ray_tracing"
pattern SPV_NV_compute_shader_derivatives :: Extension
pattern SPV_NV_compute_shader_derivatives = Extension "SPV_NV_compute_shader_derivatives"
pattern SPV_NV_fragment_shader_barycentric :: Extension
pattern SPV_NV_fragment_shader_barycentric = Extension "SPV_NV_fragment_shader_barycentric"
pattern SPV_NV_mesh_shader :: Extension
pattern SPV_NV_mesh_shader = Extension "SPV_NV_mesh_shader"
pattern SPV_NV_shader_image_footprint :: Extension
pattern SPV_NV_shader_image_footprint = Extension "SPV_NV_shader_image_footprint"
pattern SPV_NV_shading_rate :: Extension
pattern SPV_NV_shading_rate = Extension "SPV_NV_shading_rate"
pattern SPV_INTEL_subgroups :: Extension
pattern SPV_INTEL_subgroups = Extension "SPV_INTEL_subgroups"
pattern SPV_INTEL_media_block_io :: Extension
pattern SPV_INTEL_media_block_io = Extension "SPV_INTEL_media_block_io"
pattern SPV_INTEL_device_side_avc_motion_estimation :: Extension
pattern SPV_INTEL_device_side_avc_motion_estimation = Extension "SPV_INTEL_device_side_avc_motion_estimation"
pattern SPV_EXT_fragment_invocation_density :: Extension
pattern SPV_EXT_fragment_invocation_density = Extension "SPV_EXT_fragment_invocation_density"
pattern SPV_KHR_no_integer_wrap_decoration :: Extension
pattern SPV_KHR_no_integer_wrap_decoration = Extension "SPV_KHR_no_integer_wrap_decoration"
pattern SPV_KHR_float_controls :: Extension
pattern SPV_KHR_float_controls = Extension "SPV_KHR_float_controls"
pattern SPV_EXT_physical_storage_buffer :: Extension
pattern SPV_EXT_physical_storage_buffer = Extension "SPV_EXT_physical_storage_buffer"
pattern SPV_INTEL_fpga_memory_attributes :: Extension
pattern SPV_INTEL_fpga_memory_attributes = Extension "SPV_INTEL_fpga_memory_attributes"
pattern SPV_NV_cooperative_matrix :: Extension
pattern SPV_NV_cooperative_matrix = Extension "SPV_NV_cooperative_matrix"
pattern SPV_INTEL_shader_integer_functions2 :: Extension
pattern SPV_INTEL_shader_integer_functions2 = Extension "SPV_INTEL_shader_integer_functions2"
pattern SPV_INTEL_fpga_loop_controls :: Extension
pattern SPV_INTEL_fpga_loop_controls = Extension "SPV_INTEL_fpga_loop_controls"
pattern SPV_EXT_fragment_shader_interlock :: Extension
pattern SPV_EXT_fragment_shader_interlock = Extension "SPV_EXT_fragment_shader_interlock"
pattern SPV_NV_shader_sm_builtins :: Extension
pattern SPV_NV_shader_sm_builtins = Extension "SPV_NV_shader_sm_builtins"
pattern SPV_KHR_shader_clock :: Extension
pattern SPV_KHR_shader_clock = Extension "SPV_KHR_shader_clock"
pattern SPV_INTEL_unstructured_loop_controls :: Extension
pattern SPV_INTEL_unstructured_loop_controls = Extension "SPV_INTEL_unstructured_loop_controls"
pattern SPV_EXT_demote_to_helper_invocation :: Extension
pattern SPV_EXT_demote_to_helper_invocation = Extension "SPV_EXT_demote_to_helper_invocation"
pattern SPV_INTEL_fpga_reg :: Extension
pattern SPV_INTEL_fpga_reg = Extension "SPV_INTEL_fpga_reg"
pattern SPV_INTEL_blocking_pipes :: Extension
pattern SPV_INTEL_blocking_pipes = Extension "SPV_INTEL_blocking_pipes"
pattern SPV_KHR_physical_storage_buffer :: Extension
pattern SPV_KHR_physical_storage_buffer = Extension "SPV_KHR_physical_storage_buffer"
pattern SPV_KHR_non_semantic_info :: Extension
pattern SPV_KHR_non_semantic_info = Extension "SPV_KHR_non_semantic_info"
