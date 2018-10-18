module Control.Arrow.Strength where

import Control.Arrow(Arrow(first,second,(***),(&&&)), (>>^), (^>>))

---------------------------------------------------
-- Functor f => Strong (Star f)
-- (Strong p, Strong q) => Strong (ProCompose p q)

firstF :: (Arrow a, Functor f) => a b (f c) -> a (b, d) (f (c, d))
firstF = (>>^ uncurry leftStrength) . first

secondF :: (Arrow a, Functor f) => a b (f c) -> a (d, b) (f (d, c))
secondF = (>>^ uncurry rightStrength) . second 

rightStrength :: Functor f => d -> f c -> f (d,c)
rightStrength = fmap . (,)

leftStrength :: Functor f => f b -> d -> f (b,d)
leftStrength = flip (fmap . flip (,))

strong :: Functor f => f (a,b) -> (f a, f b)
strong = fmap fst &&& fmap snd