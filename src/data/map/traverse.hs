module Data.Map.Traverse where

-- containers
import Data.Map(Map)
import qualified Data.Map.Strict as Map(foldrWithKey)

----------------------------------------------------------------------------

traverseWithKey_ :: Applicative t => (k -> v -> t a) -> Map k v -> t ()
traverseWithKey_ f = Map.foldrWithKey (\k a b -> f k a *> b) (pure ())
