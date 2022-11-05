{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module FIR.Examples.DearImGui where

-- base
import Control.Monad
  ( void )
import Data.Int
  ( Int32 )
import Data.IORef
  ( IORef, readIORef, newIORef, writeIORef )
import Data.Kind
  ( Constraint, Type )
import GHC.TypeLits
  ( Symbol )

-- dear-imgui
import qualified DearImGui

-- text
import Data.Text
  ( Text )

-- transformers
import Control.Monad.IO.Class
  ( MonadIO(liftIO) )

-- fir
import FIR
  ( Struct(..), (:->)(..) )

--------------------------------------------------------------------------------

-- | A Controller is parameterized by its static /range/ and its dynamic value /dyn/.
data Controller range dyn where
  Slider         :: Controller (Float, Float) Float
  DiscreteSlider :: Controller (Int,   Int)   Int32
  Toggle         :: Controller ()             Int32

createController :: MonadIO m => Text -> Controller range dyn -> range -> IORef dyn -> m ()
createController controllerName controllerType range ref =
  case controllerType of
    Slider ->
      let (start, end) = range
       in void $ DearImGui.sliderFloat controllerName ref start end
    DiscreteSlider -> do
      pos <- liftIO $ do
        -- Create a IORef Int from the original Int32 controller ref
        val <- readIORef ref
        newIORef (fromIntegral val)
      let (start, end) = range
      void $ DearImGui.sliderInt controllerName pos start end
      liftIO $ do
        -- Convert back the Int to the Int32 controller ref value
        newPos <- readIORef pos
        writeIORef ref (fromIntegral newPos)
    Toggle -> do
      bref <- liftIO $ do
        -- Create a IORef Bool from the original Int32 controller ref
        val <- readIORef ref
        newIORef (val /= 0)
      void $ DearImGui.checkbox controllerName bref
      liftIO $ do
        -- Convert back the Bool to the Int32 controller ref value
        bval <- readIORef bref
        writeIORef ref (if bval then 1 else 0)

--------------------------------------------------------------------------------

-- | ControllerRef defines the three values a controller can be represented in:
--     * 'InitValue' for initialization with concret values (e.g. the shader provide that).
--     * 'Ref' for update through IORef (e.g. the createController use that to call DearImGui).
--     * 'Value' for rendering the actual value (e.g. the shader use that to update the UBO).
data ControllerRef = InitValue | Ref | Value

-- | ControllerData defines the actual types for each ControllerRef
type ControllerData :: ControllerRef -> Type -> Type -> Type
type family ControllerData ref range dyn where
  -- | (name, controller, range, initial value)
  ControllerData 'InitValue range dyn = ( Text, Controller range dyn, range, dyn )
  -- | (name, controller, range, ref value)
  ControllerData 'Ref       range dyn = ( Text, Controller range dyn, range, IORef dyn )
  -- | the value
  ControllerData 'Value     _     dyn = dyn

-- | 'controllerInitValues' is used by the Shader to convert a list of controller into the 'InputData'
type ControllerInitValues :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> Constraint
class ControllerInitValues as bs | as -> bs where
  controllerInitValues :: Struct as -> Struct bs
instance ControllerInitValues '[] '[] where
  controllerInitValues _ = End
instance ( ControllerInitValues as bs
         , k1 ~ k2
         , v ~ ( Text, Controller range dyn, range, dyn )
         , r ~ dyn
         )
      => ControllerInitValues ( ( k1 ':-> v ) ': as ) ( ( k2 ':-> r ) ': bs )
      where
  controllerInitValues ( ( _, _, _, dyn ) :& as ) =
    dyn :& controllerInitValues as

-- | 'createControllerRefs' is used by the Application to convert the initial values provided by the shader:
--     * This is used to read the values for updating the UBO, and,
--     * To create the DearImGui draw calls.
type CreateControllerRefs :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> Constraint
class CreateControllerRefs as bs | as -> bs, bs -> as where
  createControllerRefs :: MonadIO m => Struct as -> m ( Struct bs )
instance CreateControllerRefs '[] '[] where
  createControllerRefs _ = pure End
instance ( CreateControllerRefs as bs
         , k1 ~ k2
         , v ~ ( Text, Controller range dyn, range, dyn )
         , r ~ ( Text, Controller range dyn, range, IORef dyn )
         )
      => CreateControllerRefs ( ( k1 ':-> v ) ': as ) ( ( k2 ':-> r ) ': bs )
      where
  createControllerRefs ( ( nm, ct, range, dyn ) :& as ) = do
    ref  <- liftIO $ newIORef dyn
    refs <- createControllerRefs as
    pure ( ( nm, ct, range, ref ) :& refs )

-- | 'createControllers' calls the 'createController' for each controller to create all the DearImGui draw calls.
type CreateControllers :: [ Symbol :-> Type ] -> Constraint
class CreateControllers as where
  createControllers :: MonadIO m => Struct as -> m ()
instance CreateControllers '[] where
  createControllers _ = pure ()
instance ( CreateControllers as
         , r ~ ( Text, Controller range dyn, range, IORef dyn )
         )
      => CreateControllers ( ( k ':-> r ) ': as )
      where
  createControllers ( ( nm, ct, range, dyn ) :& rs ) =
    createController nm ct range dyn *> createControllers rs

-- | 'readControllers' convert the controller in their value to update the UBOs.
type ReadControllers :: [ Symbol :-> Type ] -> [ Symbol :-> Type ] -> Constraint
class ReadControllers as bs | as -> bs where
  readControllers :: MonadIO m => Struct as -> m ( Struct bs )
instance ReadControllers '[] '[] where
  readControllers _ = pure End
instance ( ReadControllers as bs
         , k1 ~ k2
         , r ~ ( Text, Controller range dyn, range, IORef dyn )
         , b ~ dyn
         )
      => ReadControllers ( ( k1 ':-> r ) ': as ) ( ( k2 ':-> b ) ': bs )
      where
  readControllers ( ( _, _, _, r ) :& rs ) = do
    a  <- liftIO $ readIORef r
    as <- readControllers rs
    pure ( a :& as )
