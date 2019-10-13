{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CodeGen.Instruction
  ( Args(..), foldrArgs, arity, toArgs, Pairs(..)
  , ID(..), TyID(MkTyID, TyID, tyID)
  , Instruction(..)
  , prependArg, setArgs
  ) where

-- base
import Control.Category
  ( (>>>) )
import Data.Word
  ( Word32 )

-- fir
import Data.Binary.Class.Put
  ( Put(..) )
import qualified SPIRV.Operation as SPIRV

----------------------------------------------------------------------------
-- args

data Args where
  EndArgs :: Args
  Arg     :: ( Show a, Put a ) => a -> Args -> Args

deriving stock instance Show Args

-- mono-foldable business
foldrArgs :: (forall a. (Show a, Put a) => a -> b -> b) -> b -> Args -> b
foldrArgs _ b0 EndArgs    = b0
foldrArgs f b0 (Arg a as) = f a ( foldrArgs f b0 as )

instance Put Args where
  put = foldrArgs (put >>> (>>)) (pure ())
  wordCount = foldrArgs ( (+) . wordCount ) 0

arity :: Num a => Args -> a
arity = foldrArgs (const (+1)) 0

toArgs :: ( Show a, Put a, Foldable t ) => t a -> Args
toArgs = foldr Arg EndArgs

newtype Pairs a = Pairs [(a,a)]
 deriving stock ( Functor, Foldable, Traversable )

----------------------------------------------------------------------------
-- instructions

newtype ID = ID { idNumber :: Word32 }
  deriving newtype ( Eq, Ord, Enum, Put )

instance Show ID where
  show (ID n) = "%" ++ show n

newtype TyID = TyID { tyID :: ID }
  deriving newtype ( Eq, Ord, Enum, Put, Show )

pattern MkTyID :: Word32 -> TyID
pattern MkTyID i = TyID (ID i)

data Instruction
  = Instruction
    { operation :: SPIRV.Operation
    , resTy     :: Maybe TyID
    , resID     :: Maybe ID
    , args      :: Args
    }
  deriving stock Show

prependArg :: ( Show a, Put a ) => a -> Instruction -> Instruction
prependArg arg instr@Instruction { args = oldArgs }
  = instr { args = Arg arg oldArgs }

setArgs :: Args -> Instruction -> Instruction
setArgs as instr = instr { args = as }
