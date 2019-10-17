{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Syntax.Labels
Description: @---@ __Optional__ /imperative-style/ syntax using overloaded labels.

Imperative-like syntax for variable assignment using overloaded labels. /Completely optional./

To use this functionality, import this module, activating /OverloadedLabels/ and /RebindableSyntax/.
You'll most likely want to use /TypeApplications/ too.

Usages of label syntax, such as @#foo@, will be desugared
using the 'fromLabel' method of the 'IsLabel' class in this module.

This allows for notation such as:

> do
>   #x #= (1 :: Float)  -- defines a new variable named 'x', initialised with value (1 :: Float)
> 
>   #x .= 2             -- sets the value of variable 'x' to 2
> 
>   #x %= (*3)          -- modifies the value of variable 'x' by multiplying it by 3
> 
>   #y @Float #=! 7     -- defines a new constant named 'y', initialised with value (7 :: Float)
> 
>   z <- #x + #y
>   -- 'z' then has value 6+7 = 13

In the first four examples, the overloaded labels are desugared into standalone labels,
which are then consumed by the infix operators @#=@, @.=@, @%=@ and @#=!@.

In the last example, the labels are desugared directly into values,
within the context of the indexed monad of the @do@ block.

The stateful nature of these labels is worth emphasising.
Consider for instance:

> do
>    #x @Float #= 0
>    x <- #x
>    #x .= 1
>    x' <- #x

When we write @x <- #x@, the /current/ value of @#x@ is obtained from the state.
In this example, this means that @x@ will continue to refer to the value @0@,
even after we set @#x@ to 1. When we retrieve the value of @#x@ after modifying it,
we then give @x'@ the (immutable) value of @1@.

This subtlety is especially important when working with @while@ loops,
as one wants to avoid having a constant conditional in the header.

-}

module FIR.Syntax.Labels
 ( -- * Overloaded labels
   IsLabel(fromLabel)
   -- * Infix operators
 , (#=), (#=!), (.=), (%=)
 )
 where

-- base
import Data.Kind
  ( Type )
import qualified GHC.Stack
import GHC.TypeLits
  ( Symbol, KnownSymbol )

-- fir
import Control.Monad.Indexed
  ( Codensity, (:=) )
import Control.Type.Optic
  ( Optic, Name )
import FIR.AST
  ( AST )
import FIR.Binding
  ( Var, R, RW )
import FIR.Syntax.Codensity
  ( def, use, assign, modifying )
import FIR.Prim.Singletons
  ( PrimTy )
import FIR.ProgramState
  ( ProgramState )
import FIR.Validation.Bindings
  ( ValidDef, AddBinding, Has, CanGet, CanPut )

--------------------------------------------------------------------------
-- overloaded labels

data Label (k :: Symbol) (a :: Type) = Label

data LabelUsage
  = Symbolic
  | Use Symbol ProgramState

class IsLabel k a (usage :: LabelUsage) v
    | v -> a, usage v -> k, v k -> usage
    where
  fromLabel :: GHC.Stack.HasCallStack => v

-- | Use a label symbolically.
--
-- That is, @#foo@ stands for a standalone label.
instance ( KnownSymbol k
         , PrimTy a
         , usage ~ Symbolic
         )
       => IsLabel k a usage (Label k a) where
  fromLabel = Label @k @a

-- | Use a label as a monadic value.
--
-- That is, @#foo@ stands for the value bound at @"foo"@
-- in the context of an indexed state monad.
instance ( KnownSymbol k
         , PrimTy a
         , a ~ Has k i
         , CanGet k i
         , r ~ (AST a := i)
         , usage ~ 'Use k i
         )
      => IsLabel k a usage (Codensity AST r i) where
  fromLabel = use @(Name k :: Optic '[] i a)

--------------------------------------------------------------------------
-- infix operators

infixr 1 #=
infixr 1 #=!
infixr 1 .=

-- | Define a new variable using a label.
(#=) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , ValidDef k i
        , PrimTy a
        )
     => Label k a
     -> AST a
     -> Codensity AST (AST a := AddBinding k (Var RW a) i) i
_ #= a = def @k @RW a

-- | Define a new constant using a label.
(#=!) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , ValidDef k i
        , PrimTy a
        )
     => Label k a
     -> AST a
     -> Codensity AST (AST a := AddBinding k (Var R a) i) i
_ #=! a = def @k @R a

-- | Set the value of a variable with given label.
(.=) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , a ~ Has k i
        , CanPut k i
        , PrimTy a
        )
     => Label k a
     -> AST a
     -> Codensity AST (AST () := i) i
_ .= a = assign @(Name k :: Optic '[] i a) a

-- | Modify a variable with given label using a function.
(%=) :: forall a k i.
        ( GHC.Stack.HasCallStack
        , KnownSymbol k
        , a ~ Has k i
        , CanGet k i
        , CanPut k i
        )
     => Label k a
     -> (AST a -> AST a)
     -> Codensity AST (AST () := i) i
_ %= f = modifying @(Name k :: Optic '[] i a) f
