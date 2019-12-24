{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.AST.Display

Graphically displaying syntax trees for inspection.

Each constructor of the AST must provide a `Display` instance.
-}


module FIR.AST.Display
  ( Display(toTreeArgs), toTree, named )
  where

-- base
import Data.Kind
  ( Type )

-- containers
import Data.Tree
 ( Tree(Node) )

-- haskus-utils-variant
import Haskus.Utils.VariantF
  ( VariantF(VariantF), ApplyAll, BottomUp(toBottomUp) )
import Haskus.Utils.EGADT
  ( EGADT(..), HVariantF(..) )

-- mtl
import Control.Monad.State.Lazy
  ( evalState )

-- fir
import CodeGen.Instruction
  ( ID(ID) )
import CodeGen.Monad
  ( MonadFresh, runFreshSuccT )

-------------------------------------------------------------------------------------

class Display (f :: k -> Type) where
  toTreeArgs :: MonadFresh ID m => [Tree String] -> f a -> m (Tree String)

toTree :: Display f => f a -> Tree String
toTree = (`evalState` (ID 1)) . runFreshSuccT . toTreeArgs []

instance BottomUp Display fs => Display (VariantF fs) where
  toTreeArgs as = toBottomUp @Display ( toTreeArgs as )

deriving via VariantF ( ApplyAll (EGADT fs) fs )
  instance BottomUp Display ( ApplyAll (EGADT fs) fs )
        => Display ( EGADT fs )

named :: Monad m => ( f a -> String ) -> ( [Tree String] -> f a -> m (Tree String) )
named name as f = pure ( Node ( name f ) as )
