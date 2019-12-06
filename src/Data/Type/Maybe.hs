{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module: Data.Type.Maybe

Type families for manipulating 'Maybe'.
-}

module Data.Type.Maybe
  ( FromMaybe, FromJust, IsJust, IsNothing
  , IfNothingThen, SequenceMaybe
  )
  where

-- base
import GHC.TypeLits
  ( TypeError, ErrorMessage )

------------------------------------------------

type family FromMaybe (x :: Maybe k) (y :: k) :: k where
  FromMaybe (Just x) _ = x
  FromMaybe Nothing y  = y

type family FromJust (x :: Maybe k) (y :: ErrorMessage) :: k where
  FromJust (Just x) _  = x
  FromJust Nothing err = TypeError err

type family IsJust (x :: Maybe k) :: Bool where
  IsJust (Just _) = True
  IsJust Nothing  = False

type family IsNothing (x :: Maybe k) :: Bool where
  IsNothing Nothing   = True
  IsNothing (Just _ ) = False

type family IfNothingThen (x :: Maybe k) (y :: Maybe k) :: Maybe k where
  IfNothingThen (Just x) _ = Just x
  IfNothingThen Nothing  y = y

type family SequenceMaybe (m :: Maybe [a]) :: [a] where
  SequenceMaybe Nothing   = '[]
  SequenceMaybe (Just as) = as
