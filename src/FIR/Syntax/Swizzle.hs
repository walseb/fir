{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module: FIR.Syntax.Swizzle

A swizzle is a way of remapping components of a vector, e.g.:

> > view @(Swizzle "xyxz") (V3 1.0 2.0 3.0)
> V4 1.0 2.0 1.0 3.0

This is a convenience layer around the general type-level optics machinery
defined in "Control.Type.Optic", and more specifically a wrapper
around product optics, as @Swizzle "xyxz"@ desugars to the product optic

> Prod ( Index 0 :*: Index 1 :*: Index 0 :*: Index 2 :*: EndProd )

The purpose of this module is simply to implement this desugaring.
-}

module FIR.Syntax.Swizzle
  ( Swizzle )
  where

-- base
import Data.Kind
  ( Type )
import Data.Type.Bool
  ( If )
import Data.Type.Equality
  ( type (==) )
import GHC.TypeLits
  ( Symbol
  , TypeError, ErrorMessage(..)
  )

-- fir
import Control.Type.Optic
  ( Optic(..), Index
  , ProductComponents(ProductO)
  , EndProd
  )
import Data.Type.List
  ( Length )
import Data.Type.String
  ( LookupTree
      ( EnsureGTE, EnsureLT, Node, Leaf, Return )
  , LookupChars
  )
import FIR.AST
  ( AST )
import Math.Linear
  ( V )

--------------------------------------------------------------------------------------------------

-- swizzle sets: xyzw rgba stpq
-- ASCII order: AB...G...PQRST...WXYZ...ab...g...pqrst...wxyz

type SwizzleTree
  = EnsureGTE "A"
    ( EnsureLT "{"
      ( Node "[" "`"
        ( Node "G" "H"
           ( Node "B" "C"
             ( Leaf "A" )
             ( Node "D" "E"
               ( Leaf "C" )
               Return
             )
           )
           ( Node "U" "V"
             ( Node "P" "Q"
                Return
                ( Node "R" "S"
                  ( Leaf "Q" )
                  ( Node "T" "U"
                    ( Leaf "S" )
                    Return
                  )
                )
             )
             ( Node "W" "X"
                Return
                ( Node "Y" "Z"
                  ( Leaf "X" )
                  ( Leaf "Z" )
                )
             )
           )
        )
        ( Node "g" "h"
           ( Node "b" "c"
             ( Leaf "a" )
             ( Node "d" "e"
               ( Leaf "c" )
               Return
             )
           )
           ( Node "u" "v"
             ( Node "p" "q"
                Return
                ( Node "r" "s"
                  ( Leaf "q" )
                  ( Node "t" "u"
                    ( Leaf "s" )
                    Return
                  )
                )
             )
             ( Node "w" "x"
                Return
                ( Node "y" "z"
                  ( Leaf "x" )
                  ( Leaf "z" )
                )
             )
           )
        )
      )
    )


type Swizzle (string :: Symbol)
  = ( Prod_ ( SwizzleFromString (LookupChars SwizzleTree string) )
        :: Optic '[] vec (SwizzleRes vec (LookupChars SwizzleTree string))
    )

--------------------------------------------------------------------------------------------------


type family SwizzleRes (vec :: Type) (ks :: [Symbol]) :: Type where
  SwizzleRes _   '[ ] = TypeError ( Text "Empty swizzle." )
  SwizzleRes vec '[k] = SwizzleComponent vec
  SwizzleRes (V _ a) ks = V (Length ks) a
  SwizzleRes (AST (V _ a)) ks = AST (V (Length ks) a)
  SwizzleRes ty _ =
    TypeError
        ( Text "Cannot use swizzle on non-vector of type " :<>: ShowType ty :<>: Text "." )

type family SwizzleComponent (vec :: Type) :: Type where
  SwizzleComponent (V _ a)       = a
  SwizzleComponent (AST (V _ a)) = AST a
  SwizzleComponent ty =
    TypeError
        ( Text "Cannot use swizzle on non-vector of type " :<>: ShowType ty :<>: Text "." )

type family SwizzleComponents (vec :: Type) (ks :: [Symbol]) :: [Type] where
  SwizzleComponents _   '[]  = '[]
  SwizzleComponents vec '[k] = '[ SwizzleComponent vec ]
  SwizzleComponents vec (k1 ': k2 ': ks) =
    SwizzleComponent vec ': SwizzleComponents vec (k2 ': ks)

type family SwizzleFromChar (k :: Symbol) :: Optic '[] vec (SwizzleComponent vec) where
  SwizzleFromChar "x" = Index 0
  SwizzleFromChar "X" = Index 0
  SwizzleFromChar "y" = Index 1
  SwizzleFromChar "Y" = Index 1
  SwizzleFromChar "z" = Index 2
  SwizzleFromChar "Z" = Index 2
  SwizzleFromChar "w" = Index 3
  SwizzleFromChar "W" = Index 3
  SwizzleFromChar "r" = Index 0
  SwizzleFromChar "R" = Index 0
  SwizzleFromChar "g" = Index 1
  SwizzleFromChar "G" = Index 1
  SwizzleFromChar "b" = Index 2
  SwizzleFromChar "B" = Index 2
  SwizzleFromChar "a" = Index 3
  SwizzleFromChar "A" = Index 3
  SwizzleFromChar "s" = Index 0
  SwizzleFromChar "S" = Index 0
  SwizzleFromChar "t" = Index 1
  SwizzleFromChar "T" = Index 1
  SwizzleFromChar "p" = Index 2
  SwizzleFromChar "P" = Index 2
  SwizzleFromChar "q" = Index 3
  SwizzleFromChar "Q" = Index 3
  SwizzleFromChar  k  =
    TypeError
      (     Text "Unsupported swizzle character " :<>: ShowType k :<>: Text "."
       :$$: Text "Supported swizzle sets are: \"xyzw\", \"rgba\", \"stpq\"."
      )

type family SwizzleFromString
              (swizzle :: [Symbol])
         :: ProductComponents '[] vec (SwizzleComponents vec swizzle)
         where
  SwizzleFromString '[ ] = EndProd
  SwizzleFromString '[k] = SwizzleFromChar k `ProductO` EndProd
  SwizzleFromString (k1 ': k2 ': ks) =
    If ( SwizzleSet k1 == SwizzleSet k2 )
      ( SwizzleFromChar k1 `ProductO` SwizzleFromString (k2 ': ks) )
      ( TypeError
        (    Text "Cannot mix and match different swizzle sets."
        :$$: Text "Swizzle set of " :<>: ShowType k1 :<>: Text " is "
        :<>: ShowType (SwizzleSet k1) :<>: Text "."
        :$$: Text "Swizzle set of " :<>: ShowType k2 :<>: Text " is "
        :<>: ShowType (SwizzleSet k2) :<>: Text "."
        )
      )

type family SwizzleSet ( swizzle :: Symbol ) :: Symbol where
  SwizzleSet "x" = "xyzw"
  SwizzleSet "X" = "xyzw"
  SwizzleSet "y" = "xyzw"
  SwizzleSet "Y" = "xyzw"
  SwizzleSet "z" = "xyzw"
  SwizzleSet "Z" = "xyzw"
  SwizzleSet "w" = "xyzw"
  SwizzleSet "W" = "xyzw"
  SwizzleSet "r" = "rgba"
  SwizzleSet "R" = "rgba"
  SwizzleSet "g" = "rgba"
  SwizzleSet "G" = "rgba"
  SwizzleSet "b" = "rgba"
  SwizzleSet "B" = "rgba"
  SwizzleSet "a" = "rgba"
  SwizzleSet "A" = "rgba"
  SwizzleSet "s" = "stpq"
  SwizzleSet "S" = "stpq"
  SwizzleSet "t" = "stpq"
  SwizzleSet "T" = "stpq"
  SwizzleSet "p" = "stpq"
  SwizzleSet "P" = "stpq"
  SwizzleSet "q" = "stpq"
  SwizzleSet "Q" = "stpq"
  SwizzleSet k
    = TypeError
        (     Text "Unsupported swizzle character detected in " :<>: ShowType k :<>: Text "."
         :$$: Text "Supported swizzle sets are: \"xyzw\", \"rgba\", \"stpq\"."
        )
