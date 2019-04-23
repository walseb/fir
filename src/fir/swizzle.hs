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
{-# LANGUAGE ViewPatterns           #-}

module FIR.Swizzle
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
  ( Optic(..), Index )
import Data.Type.List
  ( Length )
import Data.Type.String
  ( LookupTree
      ( EnsureGTE, EnsureLT, Node, Leaf, Return )
  , LookupChars
  )
import FIR.AST
  ( AST )
import FIR.Instances.AST
  ( )
import FIR.Instances.Optics
  ( )
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
  = ( SwizzleFromString (LookupChars SwizzleTree string)
        :: Optic '[] vec (SwizzleRes vec (LookupChars SwizzleTree string))
    )

--------------------------------------------------------------------------------------------------

type family SwizzleRes (vec :: Type) (ks :: [Symbol]) :: Type where
  SwizzleRes (V _ a)       '[_] = a
  SwizzleRes (AST (V _ a)) '[_] = AST a
  SwizzleRes (V _ a)         ks = V (Length ks) a
  SwizzleRes (AST (V _ a))   ks = AST (V (Length ks) a)
--  SwizzleRes ty _
--    = TypeError
--        ( Text "Cannot use swizzle on non-vector of type " :<>: ShowType ty :<>: Text "." )

type family SwizzleFromString (swizzle :: [Symbol]) :: Optic '[] vec (SwizzleRes vec swizzle) where
  SwizzleFromString '[ ]   = TypeError ( Text "Error: empty swizzle (no swizzle characters recognised)." )
  SwizzleFromString '["x"] = Index 0
  SwizzleFromString '["X"] = Index 0
  SwizzleFromString '["y"] = Index 1
  SwizzleFromString '["Y"] = Index 1
  SwizzleFromString '["z"] = Index 2
  SwizzleFromString '["Z"] = Index 2
  SwizzleFromString '["w"] = Index 3
  SwizzleFromString '["W"] = Index 3
  SwizzleFromString '["r"] = Index 0
  SwizzleFromString '["R"] = Index 0
  SwizzleFromString '["g"] = Index 1
  SwizzleFromString '["G"] = Index 1
  SwizzleFromString '["b"] = Index 2
  SwizzleFromString '["B"] = Index 2
  SwizzleFromString '["a"] = Index 3
  SwizzleFromString '["A"] = Index 3
  SwizzleFromString '["s"] = Index 0
  SwizzleFromString '["S"] = Index 0
  SwizzleFromString '["t"] = Index 1
  SwizzleFromString '["T"] = Index 1
  SwizzleFromString '["p"] = Index 2
  SwizzleFromString '["P"] = Index 2
  SwizzleFromString '["q"] = Index 3
  SwizzleFromString '["Q"] = Index 3
  SwizzleFromString '[k]
    = TypeError
        (     Text "Unsupported swizzle character detected in " :<>: ShowType k :<>: Text "."
         :$$: Text "Supported swizzle sets are: \"xyzw\", \"rgba\", \"stpq\"."
        )
  SwizzleFromString (k1 ': k2 ': ks)
    = If ( SwizzleSet k1 == SwizzleSet k2 )
        ( ProductO
            ( SwizzleFromString '[k1]        )
            ( SwizzleFromString ( k2 ': ks ) )
        )
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
