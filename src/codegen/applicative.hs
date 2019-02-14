{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeGen.Applicative
  ( idiomatic
  , pattern Idiomatic
  )
  where

-- base
import Data.Foldable
  ( toList )
import Data.Kind
  ( Type )
import Data.Word
  ( Word32 )

-- mtl
import Control.Monad.Except
  ( throwError )

-- text-utf8
import qualified Data.Text as Text

-- fir
import {-# SOURCE #-} CodeGen.CodeGen
  ( codeGen )
import CodeGen.Composite
  ( compositeConstruct )
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( CGMonad )
import Data.Type.Known
  ( knownValue )
import Data.Type.List
  ( Snoc )
import FIR.AST
  ( AST((:$), Fmap, Pure, Ap, MkID)
  , Syntactic(fromAST)
  )
import FIR.Instances.AST
  ( ) -- 'PrimFunc' instances
import FIR.Prim.Singletons
  ( PrimTy
  , PrimFunc(distributeAST, primFuncSing)
  , SPrimFunc(..)
  )
import Math.Linear
  ( V, M )
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- code generation for functor and applicative operations

data Idiom (f :: Type -> Type) (as :: [Type]) (b :: Type) where
  Val       :: AST (f b) -> Idiom f '[] b -- 'b' should never be a function type in this case
  PureIdiom :: forall f b. AST b -> Idiom f '[] b
  ApIdiom   :: PrimTy a => Idiom f as (a -> b) -> AST (f a) -> Idiom f (as `Snoc` a) b
deriving instance Show (Idiom f as b)

data AnIdiom (f :: Type -> Type) (b :: Type) where
  AnIdiom :: Idiom f as b -> AnIdiom f b
deriving instance Show (AnIdiom f b)

data ASTF where
  ASTF :: (Applicative f, PrimFunc f) => AST (f a) -> ASTF

anIdiom :: (Applicative f, PrimFunc f) => AST (f b) -> AnIdiom f b
anIdiom ( Pure :$ a )
  = AnIdiom $ PureIdiom a
anIdiom ( Fmap :$ f :$ ( Pure :$ a ) )
  = AnIdiom $ PureIdiom (fromAST f a)
anIdiom ( Ap :$ ( Pure :$ f ) :$ ( Pure :$ a ) )
  = AnIdiom $ PureIdiom (fromAST f a)
anIdiom ( Fmap :$ f :$ a )
  = AnIdiom $ ApIdiom (PureIdiom f) a
anIdiom ( Ap :$ ( Pure :$ f ) :$ a )
  = AnIdiom $ ApIdiom (PureIdiom f) a
anIdiom ( Ap :$ f :$ a )
  = case anIdiom f of
      AnIdiom i -> AnIdiom (ApIdiom i a)
anIdiom fb = AnIdiom $ Val fb

recogniseIdiom :: AST ast -> Maybe ASTF
recogniseIdiom ast@(Pure :$ _     ) = Just $ ASTF ast
recogniseIdiom ast@(Fmap :$ _ :$ _) = Just $ ASTF ast
recogniseIdiom ast@(Ap   :$ _ :$ _) = Just $ ASTF ast
recogniseIdiom _                    = Nothing

pattern Idiomatic :: ASTF -> AST a
pattern Idiomatic astf <- ( recogniseIdiom -> Just astf )

idiomatic :: ASTF -> CGMonad (ID, SPIRV.PrimTy)
idiomatic ( ASTF astf )
  = case anIdiom astf of
      AnIdiom i -> idiom i

idiom :: forall f as b. ( Applicative f, PrimFunc f )
             => Idiom f as b
             -> CGMonad (ID, SPIRV.PrimTy)
idiom (Val fb) = codeGen fb
idiom (PureIdiom b)
  = do
      elt@(eltID, eltTy) <- codeGen b
      case primFuncSing @f of
        sFuncVector@SFuncVector
          -> case sFuncVector of
              ( _ :: SPrimFunc (V n) )
                -> let n :: Num a => a
                       n = fromIntegral (knownValue @n)
                   in compositeConstruct
                        ( SPIRV.Vector n eltTy )
                        ( replicate n eltID )
        sFuncMatrix@SFuncMatrix
          -> case sFuncMatrix of
              ( _ :: SPrimFunc (M m n) )
                -> do constituentTy
                        <- case eltTy of
                              SPIRV.Scalar ty
                                 -> pure ty
                              ty -> throwError 
                                      ( "codeGen: matrix contains non-scalars of type "
                                       <> Text.pack (show ty)
                                      )
                      let colDim :: Word32
                          colDim = knownValue @m
                          rowDim :: Num a => a
                          rowDim = fromIntegral (knownValue @n)
                      col <- fst <$> idiom (PureIdiom @(V m) (MkID elt) )
                      compositeConstruct
                        ( SPIRV.Matrix colDim rowDim constituentTy )
                        ( replicate rowDim col )
idiom (ApIdiom i a) =
  let b :: f (AST b)
      b = applyIdiom i a
  in case primFuncSing @f of
        sFuncVector@SFuncVector
          -> case sFuncVector of
              ( _ :: SPrimFunc (V n) )
                -> do ids <- toList <$> traverse codeGen b
                      eltTy <- case ids of
                          ((_, ty) : _) -> pure ty
                          _             -> throwError "codeGen: empty vector"
                      compositeConstruct
                        ( SPIRV.Vector (knownValue @n) eltTy )
                        ( map fst ids )
        sFuncMatrix@SFuncMatrix
          -> case sFuncMatrix of
              ( _ :: SPrimFunc (M m n) )
                -> throwError "codeGen: TODO, applicative support for matrices WIP"


applyIdiom :: forall f as a b.
              ( PrimFunc f
              , PrimTy a
              , Applicative f
              )
           => Idiom f as (a -> b) -> AST (f a) -> f (AST b)
applyIdiom (Val _) _
  = error "applyIdiom: unexpected value used as a function"
applyIdiom (PureIdiom f) a
  = fromAST f <$> distributeAST a
applyIdiom (ApIdiom f a') a
  = let f' :: f ( AST a -> AST b )
        f' = fromAST <$> applyIdiom f a'
    in  f' <*> distributeAST a
