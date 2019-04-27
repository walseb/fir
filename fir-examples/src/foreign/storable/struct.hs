{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Foreign.Storable.Struct where

-- base
import Control.Applicative
  ( liftA2 )
import Foreign.Storable
  ( Storable(..) )

-- storable-record
import qualified Foreign.Storable.Record as Store

-- fir
import FIR
  ( (:->)((:->))
  , Struct((:&), End)
  , headS, tailS
  )

------------------------------------------------------------

instance Storable (Struct '[]) where
  sizeOf    _ = 0
  alignment _ = 1
  peek _   = pure End
  poke _ _ = pure ()

headTailDict :: (Storable a, Storable (Struct as))
             => Store.Dictionary (Struct ((k ':-> a) ': as ))
headTailDict = Store.run
             $ liftA2 (:&)
                ( Store.element headS )
                ( Store.element tailS )

instance (Storable a, Storable (Struct as))
      => Storable (Struct ((k ':-> a) ': as ))
      where
  sizeOf    = Store.sizeOf    headTailDict
  alignment = Store.alignment headTailDict
  peek      = Store.peek      headTailDict
  poke      = Store.poke      headTailDict
