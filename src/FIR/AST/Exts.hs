{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

{-|
Module: FIR.AST.Exts

Additional operations, such as Debug Printf.
-}

module FIR.AST.Exts where

-- base
import Data.Kind
  ( Type )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( pattern VF )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( unpack )

-- fir
import Data.Constraint.All
  ( All )
import FIR.AST.Display
  ( Display(toTreeArgs), named )
import FIR.AST.Type
  ( AugType(..), Eff
  , Nullary, FunArgs
  )
import FIR.ProgramState
  ( ProgramState )

------------------------------------------------------------

pattern DebugPrintf formatString = VF ( DebugPrintfF formatString )

-- | Printf debugging statement (SPIR-V non-semantic extension).
data DebugPrintfF ( ast :: AugType -> Type ) ( t :: AugType ) where
  DebugPrintfF
    :: All Nullary ( FunArgs ( PrintfAugType i as ) )
    => ShortText  -- ^ PrintF format string
    -> DebugPrintfF ast ( PrintfAugType i as )

type family PrintfAugType ( i :: ProgramState ) ( as :: [ Type ] ) = ( r :: AugType ) | r -> as i where
  PrintfAugType i '[]         = Eff i i ()
  PrintfAugType i ( a ': as ) = Val a :--> PrintfAugType i as

------------------------------------------------------------
-- displaying

instance Display ast => Display (DebugPrintfF ast) where
  toTreeArgs = named \(DebugPrintfF formatString) ->
    "DebugPrintF \"" <> ShortText.unpack formatString <> "\""
