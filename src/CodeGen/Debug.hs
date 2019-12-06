{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: CodeGen.Debug

Functionality for including extra debugging information as comments in SPIR-V modules,
such as including the line numbers of the Haskell source code.
-}

module CodeGen.Debug
  ( whenDebugging, whenAsserting
  , putSrcInfo
  )
  where

-- base
import Control.Monad
  ( when )
import Data.Word
  ( Word32 )
import qualified GHC.Stack
import qualified GHC.Stack.Types as GHC.Stack

-- lens
import Control.Lens
  ( view )

-- mtl
import Control.Monad.Reader
  ( MonadReader )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import CodeGen.Binary
  ( instruction )
import CodeGen.IDs
  ( stringLitID )
import CodeGen.Instruction
  ( Args(..)
  , Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad, note )
import CodeGen.State
  ( CGContext
  , _debugging
  , _asserting
  )
import qualified SPIRV.Operation as SPIRV.Op

----------------------------------------------------------------------------
-- debugging annotations

whenDebugging :: MonadReader CGContext m => m () -> m ()
whenDebugging action = (`when` action) =<< view _debugging

whenAsserting :: MonadReader CGContext m => m () -> m ()
whenAsserting action = (`when` action) =<< view _asserting

sourceInfo :: GHC.Stack.CallStack -> Maybe (ShortText, Word32, Word32)
sourceInfo GHC.Stack.EmptyCallStack = Nothing
sourceInfo (GHC.Stack.PushCallStack _ loc stack)
  = case sourceInfo stack of
      Nothing
        -> Just ( ShortText.pack $ GHC.Stack.srcLocFile      loc
                , fromIntegral   $ GHC.Stack.srcLocStartLine loc
                , fromIntegral   $ GHC.Stack.srcLocStartCol  loc
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
        instruction
          Instruction
            { operation = SPIRV.Op.Line
            , resTy = Nothing
            , resID = Nothing
            , args  = Arg fileID
                    $ Arg lineNo
                    $ Arg colNo EndArgs
            }