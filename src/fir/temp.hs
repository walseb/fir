{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module: FIR.Temp

Functionality for creating new (temporary) variable names at the type level.

Includes various operations to convert between type-level natural numbers
and type-level symbols, using a method by Csongor Kiss
(see https://kcsongor.github.io/symbol-parsing-haskell/).
-}

module FIR.Temp
  ( -- * Type-level ToList
    CompareWith
  , LookupTree
  , LookupChars, LookupPrefixedChars
  -- * Converting between 'Symbol' and 'Nat'
  -- ** Reading symbols as natural numbers
  , ReadNat, ReadPrefixedNat
  -- ** Showing natural numbers as symbols
  , ShowNat
  -- * Creating fresh temporary variable names
  , NewTemp
  ) where

-- base
import Data.Type.Bool
  ( If )
import GHC.TypeLits
  ( Symbol, CmpSymbol, AppendSymbol
  , TypeError, ErrorMessage(..)
  )
import GHC.TypeNats
  ( Nat
  , type (+), type (*)
  , Div, Mod
  )

-- fir
import Data.Type.Map
  ( (:->)((:->)) )

-------------------------------------------------------------------------------------------
-- Type-level ToList

-- | Compare a prefixed symbol with a specified symbol.
-- 
-- Note that the comparison is done relative to the last argument. For instance:
--
--   * @ CompareWith "prefix" "1" "prefix7" = GT @
--   * @ CompareWith "prefix" "7" "prefix7" = EQ @
--   * @ CompareWith "prefix" "9" "prefix7" = LT @
type family CompareWith (prefix :: Symbol) (i :: Symbol) (k :: Symbol) :: Ordering where
  CompareWith prefix i k = k `CmpSymbol` (prefix `AppendSymbol` i)

-- | Character lookup tree.
--
-- Used at the type level as a data kind.
data LookupTree a
  = EnsureGTE a (LookupTree a)
  | EnsureLT  a (LookupTree a)
  | Node a a (LookupTree a) (LookupTree a)
  | Leaf a


-- | Obtain the leading list of characters that succeed lookup.
--
-- For instance, using the numeric digit tree 'DigitTree', one has:
--
-- @ LookupChars DigitTree "127foo89" = '[ "1", "2", "7" ] @
type family LookupChars (tree :: LookupTree Symbol) (k :: Symbol) :: [Symbol] where
  LookupChars tree k = LookupPrefixedChars tree "" k

-- | Same as 'LookupChars', but allows a prefix.
--
-- @ LookupPrefixedChars DigitTree "foo" "foo34bar7" = '[ "3", "4" ] @
type family LookupPrefixedChars
              ( tree   :: LookupTree Symbol )
              ( prefix :: Symbol            )
              ( k      :: Symbol            )
           :: [Symbol]
           where
  LookupPrefixedChars tree prefix k
    = LookupPrefixedCharsFrom tree tree prefix k

-- Recursively traverse the lookup tree.
type family LookupPrefixedCharsFrom
              ( tree   :: LookupTree Symbol )
              ( curr   :: LookupTree Symbol )
              ( prefix :: Symbol            )
              ( k      :: Symbol            )
           :: [Symbol]
           where
  LookupPrefixedCharsFrom tree (EnsureGTE min subtree) prefix k
    = LookupEnsureGTE (CompareWith prefix min k) tree subtree prefix k 
  LookupPrefixedCharsFrom tree (EnsureLT max subtree) prefix k
    = LookupEnsureLT (CompareWith prefix max k) tree subtree prefix k
  LookupPrefixedCharsFrom tree (Node min max left right) prefix k
    = NodeLookup tree left right prefix k min max (CompareWith prefix min k) (CompareWith prefix max k)
  LookupPrefixedCharsFrom tree (Leaf l) prefix k
    = l ': LookupPrefixedChars tree (prefix `AppendSymbol` l) k

type family LookupEnsureGTE
              ( cmp     :: Ordering          )
              ( tree    :: LookupTree Symbol )
              ( subtree :: LookupTree Symbol )
              ( prefix  :: Symbol            )
              ( k       :: Symbol            )
            :: [Symbol] where
  LookupEnsureGTE 'LT _    _       _      _ = '[]
  LookupEnsureGTE _   tree subtree prefix k = LookupPrefixedCharsFrom tree subtree prefix k


type family LookupEnsureLT
              ( cmp     :: Ordering          )
              ( tree    :: LookupTree Symbol )
              ( subtree :: LookupTree Symbol )
              ( prefix  :: Symbol            )
              ( k       :: Symbol            )
            :: [Symbol] where
  LookupEnsureLT 'LT tree subtree prefix k = LookupPrefixedCharsFrom tree subtree prefix k
  LookupEnsureLT _   _    _       _      _ = '[]

type family NodeLookup
              ( tree   :: LookupTree Symbol )
              ( left   :: LookupTree Symbol )
              ( right  :: LookupTree Symbol )
              ( prefix :: Symbol            )
              ( k      :: Symbol            )
              ( min    :: Symbol            )
              ( max    :: Symbol            )
              ( cmpMin :: Ordering          )
              ( cmpMax :: Ordering          )
           :: [Symbol]
           where
  NodeLookup tree left _     prefix k _   _   'LT  _  = LookupPrefixedCharsFrom tree left  prefix k
  NodeLookup tree _    right prefix k _   _   _   'GT = LookupPrefixedCharsFrom tree right prefix k
  NodeLookup tree _    _     prefix k _   max _   'EQ = max ': LookupPrefixedChars tree (prefix `AppendSymbol` max) k
  NodeLookup tree _    _     prefix k min _   _    _  = min ': LookupPrefixedChars tree (prefix `AppendSymbol` min) k


-------------------------------------------------------------------------------------------
-- Converting between 'Symbol' and 'Nat'

-- Reading symbols as natural numbers

-- | Lookup tree for numeric digits.
type DigitTree
  = EnsureGTE "0"
    ( EnsureLT ":"
      ( Node "5" "6"
        ( Node "3" "4"
          ( Node "1" "2"
              ( Leaf "0" )
              ( Leaf "2" )
          )
          ( Leaf "4" )
        )
        ( Node "7" "8"
          ( Leaf "6" )
          ( Node "9" ":"
            ( Leaf "8" )
            ( Leaf "inaccessible" )
          )
        )
      )
    )

-- | Reads a natural number from the leading digits of a literal.
--
--   * @ ReadNat "0123"  = 123 @
--   * @ ReadNat "34abc" = 34  @
--   * @ ReadNat "foo"   = 0   @
type family ReadNat (k :: Symbol) :: Nat where
  ReadNat k = ReadPrefixedNat "" k

-- | Same as 'ReadNat' but allows a prefix.
--
--   * @ ReadPrefixedNat "foo1" "foo1099" = 99 @.
type family ReadPrefixedNat (prefix :: Symbol) (k :: Symbol) :: Nat where
  ReadPrefixedNat prefix k
    = FromDecimalDigitsAcc
        0
        ( LookupPrefixedChars DigitTree prefix k )

-- | Read an individual digit.
type family ReadDigit (d :: Symbol) :: Nat where
  ReadDigit "0" = 0
  ReadDigit "1" = 1
  ReadDigit "2" = 2
  ReadDigit "3" = 3
  ReadDigit "4" = 4
  ReadDigit "5" = 5
  ReadDigit "6" = 6
  ReadDigit "7" = 7
  ReadDigit "8" = 8
  ReadDigit "9" = 9
  ReadDigit x 
    = TypeError
        ( Text "Cannot read digit: " :<>: ShowType x :<>: Text " is not a digit." )

-- Returns a natural number from a base-10 representation.
type family FromDecimalDigitsAcc (acc :: Nat) (ds :: [Symbol]) :: Nat where
  FromDecimalDigitsAcc acc '[] = acc
  FromDecimalDigitsAcc acc (d ': ds)
    = FromDecimalDigitsAcc ( 10 * acc + ReadDigit d ) ds

-- | Turn a type-level natural number into a type-level symbol.
type family ShowNat (n :: Nat) :: Symbol where
  ShowNat 0 = "0"
  ShowNat 1 = "1"
  ShowNat 2 = "2"
  ShowNat 3 = "3"
  ShowNat 4 = "4"
  ShowNat 5 = "5"
  ShowNat 6 = "6"
  ShowNat 7 = "7"
  ShowNat 8 = "8"
  ShowNat 9 = "9"
  ShowNat n = ShowNat (n `Div` 10) `AppendSymbol` ShowNat (n `Mod` 10)

-------------------------------------------------------------------------------------------
-- Creating fresh temporary variable names

type TempPrefix = ( "__tmp" :: Symbol )

-- | Create a new temporary variable name.
type family NewTemp (i :: [Symbol :-> t]) :: Symbol where
  NewTemp i = NextTemp (LastTemp i)

type family NextTemp ( mbk :: Maybe Symbol ) :: Symbol where
  NextTemp Nothing = TempPrefix `AppendSymbol` "0001"
  NextTemp (Just k)
    = TempPrefix `AppendSymbol` ( ShowNat (ReadPrefixedNat TempPrefix k + 1) )

type family ReadTempNat ( k :: Symbol ) :: Nat where
  ReadTempNat k = ReadPrefixedNat TempPrefix k

type family LastTemp (i :: [Symbol :-> t]) :: Maybe Symbol where
  LastTemp '[] = Nothing
  LastTemp ( (k ':-> _) ': ks )
    = If ( InTempRange k )
        ( Just (LastInRange k ks) )
        ( LastTemp ks )

type family LastInRange (k :: Symbol) (i :: [Symbol :-> t]) :: Symbol where
  LastInRange k '[] = k
  LastInRange k ( (l ':-> _) ': ls)
    = If ( InTempRange l )
        ( LastInRange l ls )
        k

type family InTempRange (k :: Symbol) :: Bool where
  InTempRange k
    = InBetween
        ( CmpSymbol k (TempPrefix `AppendSymbol` "0") )
        ( CmpSymbol k (TempPrefix `AppendSymbol` ":") )

type family InBetween (min :: Ordering) (max :: Ordering) :: Bool where
  InBetween GT LT = True
  InBetween _  _  = False
