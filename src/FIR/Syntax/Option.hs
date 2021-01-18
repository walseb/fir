{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.Syntax.Option

Custom 'Option' type (like 'Maybe') and associated functionality.
-}

module FIR.Syntax.Option
  ( Option(..)
  , some, none
  , option, fromOption
  )
  where

-- base
import Prelude hiding ((&&))
import Control.Monad
  ( ap )

-- fir
import Control.Type.Optic
  ( Name, ReifiedGetter(view) )
import Data.Type.Map
  ( (:->)((:->)) )
import FIR.AST
  ( Code
  , Syntactic(..)
  , SyntacticVal, InternalType
  , pattern Lit, pattern Undefined, pattern Struct
  )
import FIR.AST.Type
  ( AugType(Val) )
import FIR.Prim.Struct
  ( Struct(..) )
import FIR.Prim.Types
  ( PrimTy )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.IfThenElse
  ( )
import FIR.Syntax.Program
  ( )
import Math.Logic.Class
  ( Choose(..), Boolean((&&)) )

--------------------------------------------------------------------------------------

-- | 'Option' type like in Axelsson–Svenningsson.
data Option a = Option { isSome :: Code Bool, fromSome :: a }
  deriving stock Functor
instance Applicative Option where
  pure = some
  (<*>) = ap
instance Monad Option where
  return = some
  Option { isSome = c, fromSome = a } >>= f
    | Option { isSome = d, fromSome = b } <- f a
    = Option { isSome = c && d, fromSome = b }


instance ( SyntacticVal a, PrimTy (InternalType a) ) => Syntactic (Option a) where
  type Internal (Option a) =
    Val (Struct '[ "isSome" ':-> Bool, "fromSome" ':-> InternalType a ])
  toAST (Option { isSome = c, fromSome = a })
    = choose c
        ( Struct ( Lit True  :& toAST a   :& End ) )
        ( Struct ( Lit False :& Undefined :& End ) )
  fromAST struct
    = Option
      { isSome   =          view @(Name "isSome"  ) struct
      , fromSome = fromAST (view @(Name "fromSome") struct)
      }
some :: a -> Option a
some a = Option { isSome = Lit True, fromSome = a }
none :: ( SyntacticVal a, PrimTy (InternalType a) ) => Option a
none = Option { isSome = Lit False, fromSome = fromAST Undefined }
option
  :: ( SyntacticVal a
     , PrimTy (InternalType a)
     , Choose (Code Bool) '(b,b,b)
     )
  => b -> (a -> b) -> Option a -> b
option b f a_opt =
  choose ( isSome a_opt ) ( f ( fromSome a_opt ) ) b

fromOption :: ( SyntacticVal a, PrimTy (InternalType a), Choose (Code Bool) '(a,a,a) )
           => a -> Option a -> a
fromOption a = option a id
