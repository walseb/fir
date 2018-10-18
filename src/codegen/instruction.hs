{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module CodeGen.Instruction where

-- base
import Control.Category((>>>))
import Data.Word(Word32)

-- binary
import Data.Binary(Binary)
import qualified Data.Binary     as Binary

-- fir
import qualified SPIRV.OpCodes as SPIRV
import qualified SPIRV.Types   as SPIRV

----------------------------------------------------------------------------
-- args

data Args where
  EndArgs :: Args
  Arg     :: Binary a => a -> Args -> Args

-- mono-foldable business
foldrArgs :: (forall a. Binary a => a -> b -> b) -> b -> Args -> b
foldrArgs _ b0 EndArgs    = b0
foldrArgs f b0 (Arg a as) = f a ( foldrArgs f b0 as )

putArgs :: Args -> Binary.Put
putArgs = foldrArgs (Binary.put >>> (>>)) (pure ())

arity :: Num a => Args -> a
arity = foldrArgs (const (+1)) 0

argsList :: Binary a => [a] -> Args
argsList = foldr Arg EndArgs

prependArg :: Binary a => a -> Instruction -> Instruction
prependArg arg instr@Instruction { args = oldArgs }
  = instr { args = Arg arg oldArgs }

----------------------------------------------------------------------------
-- instructions

newtype ID = ID { identifier :: Word32 }
  deriving ( Eq, Ord, Enum, Binary )
  -- derived binary instance is big-endian

data Instruction
  = Instruction
    { name  :: String
    , code  :: SPIRV.OpCode
    , resTy :: Maybe ID
    , resID :: Maybe ID
    , args  :: Args
    }

data EntryPoint a
  = EntryPoint
      { entryPoint        :: String
      , entryModel        :: SPIRV.ExecutionModel
      , entryID           :: a
      , interface         :: [ a ]
      , executionMode     :: SPIRV.ExecutionMode
      , executionModeArgs :: [ Word32 ]
      }

