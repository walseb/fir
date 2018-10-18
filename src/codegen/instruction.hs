{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module CodeGen.Instruction where

-- base
import Control.Category((>>>))
import Data.Word(Word32)

-- binary
import Data.Binary(Binary)
import qualified Data.Binary as Binary

-- text-utf8
import Data.Text(Text)

-- fir
import qualified SPIRV.OpCodes as SPIRV
import qualified SPIRV.Types   as SPIRV

----------------------------------------------------------------------------
-- args

data Args where
  EndArgs :: Args
  Arg     :: ( Show a, Binary a ) => a -> Args -> Args

deriving instance Show Args

-- mono-foldable business
foldrArgs :: (forall a. (Show a, Binary a) => a -> b -> b) -> b -> Args -> b
foldrArgs _ b0 EndArgs    = b0
foldrArgs f b0 (Arg a as) = f a ( foldrArgs f b0 as )

putArgs :: Args -> Binary.Put
putArgs = foldrArgs (Binary.put >>> (>>)) (pure ())

arity :: Num a => Args -> a
arity = foldrArgs (const (+1)) 0

argsList :: ( Show a, Binary a ) => [a] -> Args
argsList = foldr Arg EndArgs

prependArg :: ( Show a, Binary a ) => a -> Instruction -> Instruction
prependArg arg instr@Instruction { args = oldArgs }
  = instr { args = Arg arg oldArgs }

----------------------------------------------------------------------------
-- instructions

newtype ID = ID { idNumber :: Word32 }
  deriving ( Eq, Show, Ord, Enum, Binary )
  -- derived binary instance is big-endian

data Instruction
  = Instruction
    { name  :: Text
    , code  :: SPIRV.OpCode
    , resTy :: Maybe ID
    , resID :: Maybe ID
    , args  :: Args
    }
  deriving Show

data EntryPoint a
  = EntryPoint
      { entryPointName      :: Text
      , entryPointModel     :: SPIRV.ExecutionModel
      , entryPointID        :: a
      , entryPointInterface :: [ a ]
      , executionMode       :: SPIRV.ExecutionMode
      , executionModeArgs   :: [ Word32 ]
      }
  deriving Show

