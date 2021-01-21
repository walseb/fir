{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module: SPIRV.Groups

This module defines @SPIR-V@ group operations.
-}

module SPIRV.Groups
  ( GroupOperation(..)
  , groupOperationToWord32
  ) where

-- base
import Data.Word
  ( Word32 )

-- fir
import Data.Binary.Class.Put
  ( Put(..), PutWord32Enum(..) )
import Data.Type.Known
  ( Demotable(Demote), Known(known) )  

-------------------------------------------------------------------------------

data GroupOperation
  = Reduce
  | InclusiveScan
  | ExclusiveScan
  | ClusteredReduce
  deriving stock ( Show, Eq, Ord, Enum )
  deriving Put via ( PutWord32Enum GroupOperation )

groupOperationToWord32 :: GroupOperation -> Word32
groupOperationToWord32 = fromIntegral . fromEnum

instance Demotable GroupOperation where
  type Demote GroupOperation = GroupOperation

instance Known GroupOperation 'Reduce where
  known = Reduce
instance Known GroupOperation 'InclusiveScan where
  known = InclusiveScan
instance Known GroupOperation 'ExclusiveScan where
  known = ExclusiveScan
instance Known GroupOperation 'ClusteredReduce where
  known = ClusteredReduce
