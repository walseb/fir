{-# LANGUAGE CPP             #-} 
{-# LANGUAGE TemplateHaskell #-} 

{-|
Module: Instances.TH.Lift

Some Template Haskell 'Lift' instances required by this library.
-}

module Instances.TH.Lift where

#if !MIN_VERSION_containers(0,6,6)

-- template-haskell
import Language.Haskell.TH.Syntax
  ( Lift(..) )

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList, toList )


instance (Ord a, Lift a) => Lift (Set a) where
  lift      t = [|  Set.fromList  $(lift      $ Set.toList t)  |]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped t = [|| Set.fromList $$(liftTyped $ Set.toList t) ||]
#endif

#endif
