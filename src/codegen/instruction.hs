{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CodeGen.Instruction where

-- base
import Control.Category((>>>))
import Data.Word(Word32)

-- binary
import qualified Data.Binary.Put as Binary

-- fir
import Data.Binary.Class.Put(Put(put, sizeOf))
import qualified SPIRV.Operation as SPIRV

----------------------------------------------------------------------------
-- args

data Args where
  EndArgs :: Args
  Arg     :: ( Show a, Put a ) => a -> Args -> Args

deriving instance Show Args

-- mono-foldable business
foldrArgs :: (forall a. (Show a, Put a) => a -> b -> b) -> b -> Args -> b
foldrArgs _ b0 EndArgs    = b0
foldrArgs f b0 (Arg a as) = f a ( foldrArgs f b0 as )

putArgs :: Args -> Binary.Put
putArgs = foldrArgs (put >>> (>>)) (pure ())

arity :: Num a => Args -> a
arity = foldrArgs (const (+1)) 0

wordCount :: Args -> Word32
wordCount = foldrArgs ( (+) . sizeOf ) 0

toArgs :: ( Show a, Put a, Foldable t ) => t a -> Args
toArgs = foldr Arg EndArgs

newtype Pairs a = Pairs [(a,a)]
 deriving ( Functor, Foldable, Traversable )

----------------------------------------------------------------------------
-- instructions

newtype ID = ID { idNumber :: Word32 }
  deriving ( Eq, Ord, Enum, Put )

instance Show ID where
  show (ID n) = "%" ++ show n

data Instruction
  = Instruction
    { operation :: SPIRV.Operation
    , resTy     :: Maybe ID
    , resID     :: Maybe ID
    , args      :: Args
    }
  deriving Show

prependArg :: ( Show a, Put a ) => a -> Instruction -> Instruction
prependArg arg instr@Instruction { args = oldArgs }
  = instr { args = Arg arg oldArgs }

prependArgs :: ( Show a, Put a ) => [a] -> Instruction -> Instruction
prependArgs = flip ( foldr prependArg )

