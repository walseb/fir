{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

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
import CodeGen.Application
  ( Application(Applied)
  , traverseASTs
  )
import CodeGen.Binary
  ( instruction )
import {-# SOURCE #-} CodeGen.CodeGen
  ( CodeGen(codeGenArgs), codeGen )
import CodeGen.IDs
  ( stringLitID, typeID )
import CodeGen.Instruction
  ( ID(..), Args(..), toArgs
  , Instruction(..)
  )
import CodeGen.Monad
  ( MonadFresh(fresh), CGMonad, note )
import CodeGen.State
  ( CGContext
  , _debugging
  , _asserting
  , requireExtension
  )
import FIR.AST
  ( AST, DebugPrintfF(..) )
import FIR.AST.Type
  ( Nullary )
import qualified SPIRV.Extension as SPIRV
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV

----------------------------------------------------------------------------
-- debug printf

instance CodeGen AST => CodeGen (DebugPrintfF AST) where
  codeGenArgs ( Applied ( DebugPrintfF formatString ) as ) = do
    vals <- traverseASTs @Nullary ( fmap fst . codeGen ) as
    debugPrintf formatString vals

debugPrintf :: ShortText -> [ ID ] -> CGMonad (ID, SPIRV.PrimTy)
debugPrintf formatString vals = do
  whenDebugging do
    requireExtension SPIRV.SPV_KHR_non_semantic_info
    unitTyID <- typeID SPIRV.Unit
    v <- fresh
    formatStringID <- stringLitID formatString
    instruction
      Instruction
        { operation = SPIRV.Op.DebugPrintf
        , resTy = Just unitTyID
        , resID = Just v
        , args  = Arg formatStringID
                $ toArgs vals
        }
  pure (ID 0, SPIRV.Unit) -- ID should not be used

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
