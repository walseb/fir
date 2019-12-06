{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: CodeGen.Phi

Code generation for ϕ-functions, which enables definition of variables in branches when using an SSA form.

See also "CodeGen.CFG" which does code generation for the control flow operation themselves.
-}

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

-- text-short
import Data.Text.Short
  ( ShortText )


-- fir
import CodeGen.Binary
  ( instruction )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( ID, Pair(Pair), Instruction(..), toArgs )
import CodeGen.Monad
  ( CGMonad, MonadFresh(fresh) )
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

-- use "Pair" for the correct "Put" instance for list of pairs
phiInstruction :: (ID, SPIRV.PrimTy) -> [ Pair ID ID ] -> CGMonad ()
phiInstruction (v, ty) bdAndBlockIDs
  = do
      tyID <- typeID ty
      instruction
        Instruction
          { operation = SPIRV.Op.Phi
          , resTy     = Just tyID
          , resID     = Just v
          , args      = toArgs bdAndBlockIDs
          }

phiInstructions :: ( ShortText -> Bool )
                -> [ ID ]
                -> [ Map ShortText (ID, SPIRV.PrimTy) ]
                -> CGMonad (Map ShortText ID)
phiInstructions isRelevant blocks bindings 
  = Map.traverseMaybeWithKey
      ( \ name idsAndTys ->
        case idsAndTys of
          (_,ty) : _
            -> let  bdAndBlockIDs :: [Pair ID ID]
                    bdAndBlockIDs 
                      = zipWith
                          (\(x_ID, _) blk -> Pair (x_ID, blk))
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
