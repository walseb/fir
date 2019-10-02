module Data.Containers.Traversals where

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict as Map
  ( foldrWithKey )
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( foldr' )

----------------------------------------------------------------------------

traverseWithKey_ :: Applicative t => (k -> v -> t u) -> Map k v -> t ()
traverseWithKey_ f = Map.foldrWithKey (\k a b -> f k a *> b) (pure ())

traverseSet_ :: Applicative t => (a -> t u) -> Set a -> t ()
traverseSet_ f = Set.foldr' ( \a b -> f a *> b ) (pure ())
