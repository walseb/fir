{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures -Wno-missing-signatures #-}

{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: FIR.AST.Prim

Primitive operations of the AST:
  * lambda abstraction,
  * function application,
  * constant literals;
  * primitive SPIR-V operations,
  * vector/array/matrix creation,
  * ...
-}

module FIR.AST.Prim where

-- base
import Data.Foldable
  ( toList )
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol )
import Data.Proxy
  ( Proxy )
import Data.Word
  ( Word32 )
import GHC.TypeLits
  ( KnownSymbol, symbolVal )
import GHC.TypeNats
  ( KnownNat, natVal )
import Numeric.Natural
  ( Natural )
import Data.Type.Map

-- containers
import Data.Tree
 ( Tree(Node) )

-- haskus-utils-variant
import Haskus.Utils.EGADT
  ( EGADT, pattern VF, (:<!) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( knownLength', toList )

-- fir
import CodeGen.Instruction
  ( ID )
import CodeGen.Monad
  ( MonadFresh(fresh) )
import Data.Constraint.All
  ( All )
import {-# SOURCE #-} Data.Product
  ( HList )
import Data.Type.Known
  ( knownValue )
import FIR.AST.Display
  ( Display(toTreeArgs), named )
import FIR.AST.Type
  ( AugType(Val, (:-->)), Eff
  , FunArgs, FunRes
  , ApplyFAug
  )
import FIR.Prim.Array
  ( Array(MkArray) )
import FIR.Prim.Op
  ( PrimOp(PrimOpAugType, opName)
  , PrimTyVal
  )
import {-# SOURCE #-} FIR.Prim.Struct
  ( Struct, ASTStructFields, traverseStructASTs )
import {-# SOURCE #-} FIR.Prim.Types
  ( PrimTy, primTy
  , PrimTyMap, primTyMap
  , PrimFunc, primFuncName
  )
import Math.Algebra.GradedSemigroup
  ( GradedSemigroup(Grade, (:<!>:)) )
import Math.Linear
  ( V, M )
import qualified SPIRV.PrimOp as SPIRV
  ( op )
import qualified SPIRV.PrimTy as SPIRV
  ( PrimTy )
import qualified SPIRV.Stage  as SPIRV
  ( Backend(Vulkan) )

------------------------------------------------------------

infixl 9 :$
infixr 5 `ConsHList`

pattern Lam :: forall fun fs
            .  ( LamF :<! fs )
            => forall a b
            .  ( fun ~ ( Val a :--> b ) )
            => ( EGADT fs ( Val a ) -> EGADT fs b )
            -> EGADT fs fun
pattern Lam   f             = VF (LamF f )
pattern (:$)  f a           = VF (AppF   f a)
pattern Lit   l             = VF (LitF   l  )
pattern MkID  i             = VF (MkIDF  i  )
pattern Value v             = VF (ValueF v  )
pattern UnsafeCoerce        = VF UnsafeCoerceF
pattern Return              = VF ReturnF
pattern Bind                = VF BindF
pattern PrimOp a op         = VF (PrimOpF a op)

pattern Undefined :: forall a fs
                  .  ( PrimTy a, UndefinedF :<! fs )
                  => EGADT fs (Val a)
pattern Undefined           = VF UndefinedF
pattern GradedMappend       = VF GradedMappendF
pattern Pure px   a         = VF (PureF px   a)
pattern Ap   px f a         = VF (ApF   px f a)
pattern MkVector v          = VF (MkVectorF v)
pattern Mat                 = VF MatF
pattern UnMat               = VF UnMatF
pattern Struct st           = VF (StructF st)
pattern Array arr           = VF (ArrayF arr)
pattern ArrayLength px1 px2 = VF ( ArrayLengthF px1 px2 )
pattern NilHList            = VF NilHListF
pattern ConsHList           = VF ConsHListF


-- | Lambda abstraction.
data LamF ( ast :: AugType -> Type ) ( t :: AugType ) where
  LamF :: ( ast (Val a) -> ast b ) -> LamF ast ( Val a :--> b )

-- | Function application.
data AppF ( ast :: AugType -> Type ) ( t :: AugType ) where
  AppF :: ast ( a :--> b ) -> ast a -> AppF ast b

-- | Haskell-level constants can be embedded into the AST.
data LitF ( ast :: AugType -> Type ) ( t :: AugType ) where
  LitF :: PrimTy a => a -> LitF ast (Val a)

-- | Create an object of the given type from its ID.
-- These are the internal @SPIR-V@ identifiers, in SSA form.
-- Used internally during code-generation (evaluation not possible).
data MkIDF ( ast :: AugType -> Type ) ( t :: AugType ) where
  MkIDF :: ( ID, SPIRV.PrimTy ) -> MkIDF ast (Val a)

-- | Embed a value into the AST.
--
-- Used internallly to implement evaluation (code-generation not possible).
data ValueF ( ast :: AugType -> Type ) ( t :: AugType ) where
  ValueF :: a -> ValueF ast (Val a)

-- | Coercions (unsafe).
data UnsafeCoerceF ( ast :: AugType -> Type ) ( t :: AugType ) where
  UnsafeCoerceF :: UnsafeCoerceF ast (Val a :--> Val b)


data IxMonadF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- Indexed monadic operations (for the AST itself).
  -- | Indexed /return/
  ReturnF :: IxMonadF ast (Val a :--> Eff i i a)
  -- | Indexed /angelic bind/
  BindF :: IxMonadF ast ( Eff i j a :--> ( Val a :--> q j ) :--> q i )

-- | @SPIR-V@ primitive operations.
data PrimOpF ( ast :: AugType -> Type ) ( t :: AugType ) where
  PrimOpF :: PrimOp op a => Proxy a -> Proxy op -> PrimOpF ast (PrimOpAugType op a)

-- | Undefined (result of @SPIR-V@'s @OpUndef@).
data UndefinedF ( ast :: AugType -> Type ) ( t :: AugType ) where
  UndefinedF :: PrimTy a => UndefinedF ast (Val a)

-- | Graded mappend to concatenate objects (e.g. two vectors).
data GradedMappendF ( ast :: AugType -> Type ) ( t :: AugType ) where
  GradedMappendF
    :: ( GradedSemigroup g k, a ~ Grade k g i, b ~ Grade k g j )
    => GradedMappendF ast ( Val a :--> Val b :--> Val (Grade k g (i :<!>: j)) )

data ApplicativeF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | pure/return for applicative functors within the AST.
  PureF
    :: forall f a v ast
    .  ( PrimFunc f
       , All PrimTyVal (FunArgs a)
       , FunRes a ~ Val v, PrimTy v
       )
    => Proxy f
    -> ast a
    -> ApplicativeF ast ( ApplyFAug f a )
  -- | ap/(<*>) for applicative functors within the AST.
  ApF
    :: forall f a b ast. ( PrimFunc f, PrimTy a )
    => Proxy f
    -> ast ( Val (f a) :--> ApplyFAug f b )
    -> ast ( Val (f a) )
    -> ApplicativeF ast ( ApplyFAug f b )

-- | Create a vector from its components.
data MkVectorF ( ast :: AugType -> Type ) ( t :: AugType ) where
  MkVectorF
    :: ( KnownNat n, PrimTy a )
    => V n (ast (Val a))
    -> MkVectorF ast (Val (V n a))

data MatF ( ast :: AugType -> Type ) ( t :: AugType ) where
  -- | Newtype wrapping for matrices.
  MatF
    :: forall m n a ast
    .  ( KnownNat m, KnownNat n )
    => MatF ast ( Val (V n (V m a)) :--> Val (M m n a) )

  -- | Newtype unwrapping for matrices.
  UnMatF
    :: forall m n a ast
    .  ( KnownNat n, KnownNat m )
    => MatF ast ( Val (M m n a) :--> Val (V n (V m a)) )

-- | Construct a structure from statically-known parts.
data StructF ( ast :: AugType -> Type ) ( t :: AugType ) where
  StructF
    :: ( PrimTyMap bs, ASTStructFields ast as bs, bs ~ StructFResultFromArgs as, as ~ StructFArgsFromResult bs ast)
    => Struct as
    -> StructF ast ( Val (Struct bs) )

type family StructFResultFromArgs (as :: [ Symbol :-> Type ]) :: [ Symbol :-> Type ] where
  StructFResultFromArgs '[] = '[]
  StructFResultFromArgs ((k ':-> ast (Val b)) : as) = ((k ':-> b) : StructFResultFromArgs as)

type family StructFArgsFromResult (bs :: [ Symbol :-> Type ]) (ast :: AugType -> Type) :: [ Symbol :-> Type ] where
  StructFArgsFromResult '[] ast = '[]
  StructFArgsFromResult ((k ':-> b) : bs) ast = ((k ':-> ast (Val b)) : StructFArgsFromResult bs ast)

-- | Construct an array from its components.
data ArrayF ( ast :: AugType -> Type ) ( t :: AugType ) where
  ArrayF
    :: PrimTy a
    => Array n ( ast (Val a) )
    -> ArrayF ast ( Val (Array n a) )

-- | Query length of a run-time array.
data ArrayLengthF ( ast :: AugType -> Type ) ( t :: AugType ) where
  ArrayLengthF
    :: ( KnownSymbol structName, KnownNat arrayPosition )
    => Proxy structName
    -> Proxy arrayPosition
    -> ArrayLengthF ast ( Eff i i Word32 )

-- | Internal HList data type.
--
-- Only used for providing multiple run-time indices to product optics.
-- See [FIR issue #13](https://gitlab.com/sheaf/fir/issues/13).
data HListF ( ast :: AugType -> Type ) ( t :: AugType ) where

  ConsHListF
    :: PrimTy a
    => HListF ast ( Val a :--> Val (HList as) :--> Val (HList (a ': as)) )

  NilHListF
    :: HListF ast ( Val ( HList '[] ) )

------------------------------------------------------------
-- displaying

instance ( ast ~ EGADT cs, MkIDF :<! cs, Display ast ) => Display (LamF ast) where
  toTreeArgs as (LamF f) = do
    v <- fresh
    let var = MkID (v, undefined)
    body <- toTreeArgs @_ @ast [] (f var)
    return case as of
      [] -> Node ("Lam " ++ show v) [body]
      _  -> Node  ":$"              (body : as)
instance Display ast => Display (AppF ast) where
  toTreeArgs as (f `AppF` a) = do
    at <- toTreeArgs [] a
    toTreeArgs (at:as) f
instance Display (LitF ast) where
  toTreeArgs = named \ (LitF (a :: ty)) ->
    "Lit @" ++ show (primTy @ty) ++ ") " ++ show a
instance Display (MkIDF ast) where
  toTreeArgs = named ( \ (MkIDF (v,_)) -> show v )
instance Display (ValueF ast) where
  toTreeArgs = error "internal error: unexpected 'Value' constructor, unsupported in pretty-printing"
instance Display (UnsafeCoerceF ast) where
  toTreeArgs = named (const "UnsafeCoerce")
instance Display (IxMonadF ast) where
  toTreeArgs = named \case
    ReturnF -> "Return"
    BindF   -> "Bind"
instance Display (PrimOpF ast) where
  toTreeArgs = named
    ( \( PrimOpF ( _ :: Proxy a ) ( _ :: Proxy op ) ) ->
    -- use the Vulkan backend as a default for printing operation names
        "PrimOp" ++ show ( SPIRV.op SPIRV.Vulkan ( opName @_ @_ @op @a ) )
    )
instance Display (UndefinedF ast) where
  toTreeArgs = named (const "Undefined")
instance Display (GradedMappendF ast) where
  toTreeArgs = named (const "(<!>)")
instance Display ast => Display (ApplicativeF ast) where
  toTreeArgs as (PureF ( _ :: Proxy f ) a) = do
    at <- toTreeArgs [] a
    pure $ Node ( "Pure @(" ++ primFuncName @f ++ ")" ) (at:as)
  toTreeArgs as (ApF ( _ :: Proxy f ) f a) = do
    ft <- toTreeArgs [] f
    at <- toTreeArgs [] a
    pure $ Node ( "(<*>) @(" ++ primFuncName @f ++ ")" ) (ft:at:as)
instance Display ast => Display (MkVectorF ast) where
  toTreeArgs as (MkVectorF (vec :: V n (ast (Val a)))) = do
    trees <- toList <$> traverse ( toTreeArgs @_ @ast []) vec
    pure $ Node ("Vec" ++ show ( knownValue @n ) ) ( trees ++ as )
instance Display (MatF ast) where
  toTreeArgs = named \case
    MatF   -> "Mat"
    UnMatF -> "UnMat"
instance Display ast => Display (StructF ast) where
  toTreeArgs as mkStruct@(StructF elts) = case mkStruct of
    ( _ :: StructF ast (Val (Struct bs)) ) -> do
      trees <- traverseStructASTs (toTreeArgs @_ @ast []) elts
      let
        fieldNames :: [ (Maybe ShortText, SPIRV.PrimTy) ]
        fieldNames = map ( \(k,t,_) -> (k,t) ) ( primTyMap @_ @bs )
      pure $ Node ("Struct @" ++ show fieldNames) (trees ++ as)
instance Display ast => Display (ArrayF ast) where
  toTreeArgs as mkArray@(ArrayF (MkArray vec)) = case mkArray of
    ( _ :: ArrayF ast (Val (Array n a)) ) -> do
      let
        comps = Vector.toList vec
        lg = Vector.knownLength' vec ( \px -> show ( natVal px ) )
      trees <- traverse (toTreeArgs []) comps
      pure $
        Node
          ( "Array @" ++ lg ++ " @" ++ show (primTy @a) )
          ( trees ++ as )
instance Display (ArrayLengthF ast) where
  toTreeArgs as (ArrayLengthF px_name px_pos) =
    let
      structName :: String
      structName = symbolVal px_name
      structPos :: Natural
      structPos = natVal px_pos
    in
      pure $ Node ( "ArrayLength @" <> structName <> " @" <> show structPos ) as
instance Display (HListF ast) where
  toTreeArgs = named \case
    NilHListF  -> "NilHList"
    ConsHListF -> "ConsHList"
