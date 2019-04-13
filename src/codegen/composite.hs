module CodeGen.Composite
  ( compositeConstruct
  , compositeInsert, compositeExtract
  )
  where

-- base
import Data.Word
  ( Word32 )

-- containers
import qualified Data.Map.Strict as Map

-- fir
import CodeGen.Binary
  ( putInstruction )
import CodeGen.IDs
  ( typeID )
import CodeGen.Instruction
  ( Args(..), toArgs
  , ID, Instruction(..)
  )
import CodeGen.Monad
  ( CGMonad
  , MonadFresh(fresh)
  , liftPut
  )
import qualified SPIRV.Operation as SPIRV.Op
import qualified SPIRV.PrimTy    as SPIRV

----------------------------------------------------------------------------
-- composite structures

compositeConstruct :: SPIRV.PrimTy -> [ ID ] -> CGMonad (ID, SPIRV.PrimTy)
compositeConstruct compositeType constituents
  = do tyID <- typeID compositeType
       v <- fresh
       liftPut $ putInstruction Map.empty
         Instruction
           { operation = SPIRV.Op.CompositeConstruct
           , resTy = Just tyID
           , resID = Just v
           , args  = toArgs constituents
           }
       pure (v, compositeType)

compositeExtract
  :: SPIRV.PrimTy
  -> [ Word32 ]
  -> (ID, SPIRV.PrimTy)
  -> CGMonad (ID, SPIRV.PrimTy)
compositeExtract constituentTy indices (compositeID, _)
  = do 
      constituentTyID <- typeID constituentTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.CompositeExtract
          , resTy     = Just constituentTyID
          , resID     = Just v
          , args      = Arg compositeID
                      $ toArgs indices
          }
      pure (v, constituentTy)

compositeInsert
  :: ID
  -> (ID, SPIRV.PrimTy)
  -> [ Word32 ]
  -> CGMonad (ID, SPIRV.PrimTy)
compositeInsert inserteeID (compositeID, compositeTy) indices
  = do
      compositeTyID <- typeID compositeTy
      v <- fresh
      liftPut $ putInstruction Map.empty
        Instruction
          { operation = SPIRV.Op.CompositeInsert
          , resTy     = Just compositeTyID
          , resID     = Just v
          , args      = Arg inserteeID
                      $ Arg compositeID
                      $ toArgs indices
          }
      pure (v, compositeTy)
