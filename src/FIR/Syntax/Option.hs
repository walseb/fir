{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
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
  ( AST(Lit, Undefined, Struct), Syntactic(..) )
import FIR.Prim.Singletons
  ( PrimTy )
import FIR.Prim.Struct
  ( Struct(..) )
import FIR.Syntax.AST
  ( )
import FIR.Syntax.Program
  ( )
import Math.Logic.Class
  ( Choose(..), Boolean((&&)) )

--------------------------------------------------------------------------------------

-- | 'Option' type like in Axelsson–Svenningsson.
data Option a = Option { isSome :: AST Bool, fromSome :: a }
  deriving stock Functor
instance Applicative Option where
  pure = some
  (<*>) = ap
instance Monad Option where
  return = some
  Option { isSome = c, fromSome = a } >>= f
    | Option { isSome = d, fromSome = b } <- f a
    = Option { isSome = c && d, fromSome = b }


instance ( Syntactic a, PrimTy (Internal a) ) => Syntactic (Option a) where
  type Internal (Option a) =
    Struct '[ "isSome" ':-> Bool, "fromSome" ':-> Internal a ]
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
none :: ( Syntactic a, PrimTy (Internal a) ) => Option a
none = Option { isSome = Lit False, fromSome = fromAST Undefined }
option
  :: ( Syntactic a
     , PrimTy (Internal a)
     , Choose (AST Bool) '(b,b,b)
     )
  => b -> (a -> b) -> Option a -> b
option b f a_opt =
  choose ( isSome a_opt ) ( f ( fromSome a_opt ) ) b

fromOption :: ( Syntactic a, PrimTy (Internal a), Choose (AST Bool) '(a,a,a) )
           => a -> Option a -> a
fromOption a = option a id
