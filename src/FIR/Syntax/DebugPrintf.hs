{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-|
Module: FIR.Syntax.DebugPrintf
Description: @---@ Printf-like formatting functionality (for shader debugging).
-}

module FIR.Syntax.DebugPrintf
  ( Format, Formatter
  , debugPrintf
  , (%)
  , int32, word32, word32Hex
  , float, scientific
  , vec
  )
  where

-- base
import Data.Int
  ( Int32 )
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )
import Data.String
  ( IsString(..) )
import GHC.Exts
  ( proxy#, Proxy# )
import GHC.TypeNats
  ( Nat, KnownNat
  , type (<=)
  , natVal'
  )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- fir
import Data.Constraint.All
  ( All )
import Data.Type.List
  ( type (:++:) )
import FIR.AST
  ( Code, Syntactic(..)
  , PrintfAugType, pattern DebugPrintf
  )
import FIR.AST.Type
  ( FunArgs, Nullary )
import FIR.Module
  ( Program )
import FIR.ProgramState
  ( ProgramState )
import Math.Linear
  ( V )

----------------------------------------------------------------------------

-- | A printf format, taking parameters of the given types.
data Format ( as :: [ Type ] ) =
  UnsafeFormatFromFormatString { formatString :: !ShortText }
  deriving stock ( Eq, Show )

-- | Print to debug output using a printf-style formatter.
--
-- Need to pass the 'Debug' compiler flag (otherwise this instruction will be ignored).
--
-- /Usage/: say we want to print the following values:
--  
--   > x :: Code Float
--   > y :: Code Int32
--   > z :: Code (V 2 Float)
--
--  We can do this using the command:
--   > debugPrintf ( "x = " % float % " y = " % int32 % " z = " % vec @2 float ) x y z
--
-- '(%)' denotes composition of formatters.
--
-- /Note/: Enabling the OverloadedStrings extension is necessary.
debugPrintf
  :: forall
      ( as :: [Type ]      )
      ( i  :: ProgramState )
  .  ( Syntactic ( ProgPrintf i as )
     , Internal  ( ProgPrintf i as ) ~ PrintfAugType i as
     , All Nullary ( FunArgs ( PrintfAugType i as ) )
     )
  => Format as -> ProgPrintf i as
debugPrintf fmt = fromAST ( DebugPrintf ( formatString fmt ) )

type family ProgPrintf ( i :: ProgramState ) ( as :: [ Type ] ) = ( r :: Type ) | r -> i as where
  ProgPrintf i '[]         = Program i i ( Code () )
  ProgPrintf i ( a ': as ) = Code a -> ProgPrintf i as


instance ( as ~ '[] ) => IsString ( Format as ) where
  fromString
    = UnsafeFormatFromFormatString 
    . fromString
    . escapePercents

escapePercents :: [Char] -> [Char]
escapePercents [] = []
escapePercents ( '%' : as ) = '%' : '%' : escapePercents as
escapePercents (  c  : as ) =         c : escapePercents as

infixr 9 %
-- | Concatenate two formatters.
--
-- See 'debugPrintf' for example usage.
(%) :: Format as -> Format bs -> Format ( as :++: bs )
( UnsafeFormatFromFormatString a ) % ( UnsafeFormatFromFormatString b ) =
  UnsafeFormatFromFormatString ( a <> b )

-- | A class for types which can be formatted.
class Formattable ( a :: Type ) where
  data Formatter a :: Type
  formatterString :: Formatter a -> ShortText

instance Formattable Int32 where
  data Formatter Int32
    = FormatInt32
  formatterString _ = "i"

instance Formattable Word32 where
  data Formatter Word32
    = FormatWord32
    | FormatWord32Hex
  formatterString FormatWord32    = "u"
  formatterString FormatWord32Hex = "X"

instance Formattable Float where
  data Formatter Float
    = FormatFloatDigits
    | FormatFloatScientific
    | FormatFloatGeneric
  formatterString FormatFloatDigits     = "f"
  formatterString FormatFloatScientific = "e"
  formatterString FormatFloatGeneric    = "g"

instance ( KnownNat n, 2 <= n, n <= 4, Formattable a ) => Formattable ( V n a ) where
  data Formatter ( V n a )
    = FormatVec ( Formatter a )
  formatterString ( FormatVec fmt ) = "v" <> n <> formatterString fmt
    where
      n :: ShortText
      n = ShortText.pack . show $ natVal' ( proxy# :: Proxy# n )

mkFormatter :: Formattable a => Formatter a -> Format '[ a ]
mkFormatter fmt = UnsafeFormatFromFormatString ( "%" <> formatterString fmt )

class Int32Formatter r where
  int32 :: r
instance ( as ~ '[ Int32 ] ) => Int32Formatter ( Format as ) where
  int32 = mkFormatter FormatInt32
instance ( a ~ Int32 ) => Int32Formatter ( Formatter a ) where
  int32 = FormatInt32

class Word32Formatter r where
  word32    :: r
  word32Hex :: r
instance ( as ~ '[ Word32 ] ) => Word32Formatter ( Format as ) where
  word32    = mkFormatter FormatWord32
  word32Hex = mkFormatter FormatWord32Hex
instance ( a ~ Word32 ) => Word32Formatter ( Formatter a ) where
  word32    = FormatWord32
  word32Hex = FormatWord32Hex

class FloatFormatter r where
  float      :: r
  scientific :: r
instance ( as ~ '[ Float ] ) => FloatFormatter ( Format as ) where
  float      = mkFormatter FormatFloatDigits
  scientific = mkFormatter FormatFloatScientific
instance ( a ~ Float ) => FloatFormatter ( Formatter a ) where
  float      = FormatFloatDigits
  scientific = FormatFloatScientific

class ( KnownNat n, 2 <= n, n <= 4, Formattable a ) => VecFormatter (n :: Nat) a r | r -> n a where
  vec :: Formatter a -> r
instance ( KnownNat n, 2 <= n, n <= 4, Formattable a, as ~ '[ V n a ] )
      => VecFormatter n a ( Format as ) where
  vec fmt = mkFormatter ( FormatVec fmt )
instance ( KnownNat n, 2 <= n, n <= 4, Formattable a, v ~ V n a )
      => VecFormatter n a ( Formatter v ) where
  vec fmt = FormatVec fmt
