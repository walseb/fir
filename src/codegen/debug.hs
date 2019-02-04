{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Debug
  ( debug, putSrcInfo )
  where

-- base
import Control.Monad(when)
import Data.Word(Word32)
import qualified GHC.Stack
import qualified GHC.Stack.Types as GHC.Stack

-- containers
import qualified Data.Map.Strict as Map

-- lens
import Control.Lens(view)

-- mtl
import Control.Monad.Reader(MonadReader)

-- text-utf8
import Data.Text(Text)
import qualified Data.Text as Text

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( stringLitID )
import CodeGen.Instruction
  ( Args(..)
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad
  , liftPut
  , note
  )
import CodeGen.State
  ( CGContext
  , _debugMode
  )
import qualified SPIRV.Operation as SPIRV.Op

----------------------------------------------------------------------------
-- debugging annotations

debug :: MonadReader CGContext m => m () -> m ()
debug action = (`when` action) =<< view _debugMode

sourceInfo :: GHC.Stack.CallStack -> Maybe (Text, Word32, Word32)
sourceInfo GHC.Stack.EmptyCallStack = Nothing
sourceInfo (GHC.Stack.PushCallStack _ loc stack)
  = case sourceInfo stack of
      Nothing
        -> Just ( Text.pack    $ GHC.Stack.srcLocFile      loc
                , fromIntegral $ GHC.Stack.srcLocStartLine loc
                , fromIntegral $ GHC.Stack.srcLocStartCol  loc
                )
      Just info
        -> Just info
sourceInfo (GHC.Stack.FreezeCallStack stack) = sourceInfo stack

putSrcInfo :: GHC.Stack.CallStack -> CGMonad ()
putSrcInfo callstack
  = do  (fileName, lineNo, colNo)
          <- note "putSrcInfo: cannot find source location \
                  \needed for debug statement"
                  ( sourceInfo callstack )
        fileID <- stringLitID fileName
        liftPut $ putInstruction Map.empty
          Instruction
            { operation = SPIRV.Op.Line
            , resTy = Nothing
            , resID = Nothing
            , args  = Arg fileID
                    $ Arg lineNo
                    $ Arg colNo EndArgs
            }