{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeGen.Phi
  ( phiInstruction, phiInstructions )
  where

-- base
import Data.List
  ( foldl1' )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map.Strict       as Map
import qualified Data.Map.Merge.Strict as Map

-- lens
import Control.Lens
  ( assign )

-- text-utf8
import "text-utf8" Data.Text
  ( Text )


-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( ID, Pairs(Pairs), Instruction(..), toArgs )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh), liftPut )
import CodeGen.State
  ( _localBinding )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV

----------------------------------------------------------------------------
-- phi instructions

conflicts :: forall k a. (Ord k, Eq a)
          => ( k -> Bool )
          -> [ Map k a ]
          -> Map k [a]
conflicts keyIsOK
  = Map.mapMaybeWithKey
      ( \k as -> if keyIsOK k && any (/= head as) as
                 then Just as
                 else Nothing
      )
  . foldl1'
      ( Map.merge
          Map.dropMissing
          Map.dropMissing
          ( Map.zipWithMatched (const (++)) )
      )
  . map (fmap (:[]))

-- 'Pairs ID' has the right traversable instance for the 'toArgs' function
-- (recall that 'Pairs a' is a newtype wrapper around '[(a,a)]')
phiInstruction :: (ID, SPIRV.PrimTy) -> Pairs ID -> CGMonad ()
phiInstruction (v, ty) bdAndBlockIDs
  = do
      tyID <- typeID ty
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.Phi
          , resTy     = Just tyID
          , resID     = Just v
          , args      = toArgs bdAndBlockIDs
          }

phiInstructions :: ( Text -> Bool ) -> [ ID ] -> [ Map Text (ID, SPIRV.PrimTy) ] -> CGMonad (Map Text ID)
phiInstructions isRelevant blocks bindings 
  = Map.traverseMaybeWithKey
      ( \ name idsAndTys ->
        case idsAndTys of
          (_,ty) : _
            -> let  bdAndBlockIDs :: Pairs ID
                    bdAndBlockIDs 
                      = Pairs $ zipWith 
                                  (\(x_ID, _) blk -> (x_ID, blk))
                                  idsAndTys
                                  blocks
               in do
                  v <- fresh
                  phiInstruction (v,ty) bdAndBlockIDs
                  assign ( _localBinding name ) (Just (v, ty))
                  pure (Just v)
          _ -> pure Nothing
      )
      ( conflicts isRelevant bindings )
