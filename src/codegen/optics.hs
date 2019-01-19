{-# OPTIONS_GHC -fno-warn-unused-matches   #-} -- WIP
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- WIP
 
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module CodeGen.Optics
  ( loadThroughAccessChain
  , storeThroughAccessChain
  , extractUsingGetter
  , insertUsingSetter
  ) where

-- base
import Control.Arrow(first)
import Data.Word(Word32)
import GHC.TypeNats(natVal)

-- text-utf8
import Data.Text(Text)

-- mtl
import Control.Monad.Except(MonadError, throwError)

-- fir
import CodeGen.AST(ASTIndexList(INil, ICons))
import {-# SOURCE #-} CodeGen.CodeGen(codeGen)
import CodeGen.IDs(constID)
import CodeGen.Instruction(ID)
import CodeGen.Monad(CGMonad)
import Control.Type.Optic(Optic, ProductIndices)
import Data.Type.Map(type (:++:), SLength(SZero, SSucc))
import FIR.AST(AST((:$), Fst, Snd))
import FIR.Instances.Optics(SOptic(..))
import FIR.Prim.Singletons(HasField(fieldIndex))
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- optics

data OpticalNode
  = Leaf     ID
  | Continue ID OpticalTree
  | Combine  [OpticalTree]

data OpticalTree = Node IndexSafeness OpticalNode

data IndexSafeness
  = Unsafe
  | Safe
  deriving ( Eq, Show )

instance Semigroup IndexSafeness where
  Safe <> x = x
  _    <> _ = Unsafe

composedIndices
  :: SLength is
  -> ASTIndexList (is :++: js)
  -> (ASTIndexList is, ASTIndexList js)
composedIndices SZero js = ( INil, js )
composedIndices (SSucc tail_is) (k `ICons` ks)
  = first ( k `ICons` ) (composedIndices tail_is ks)

combinedIndices
  :: SLength is
  -> SLength js
  -> ASTIndexList (ProductIndices is js)
  -> (ASTIndexList is, ASTIndexList js)
combinedIndices SZero SZero _ = ( INil, INil )
combinedIndices SZero (SSucc _) ks = ( INil, ks )
combinedIndices (SSucc _) SZero ks = ( ks, INil )
combinedIndices (SSucc is) (SSucc js) (k1k2 `ICons` ks)
  = case combinedIndices is js ks of
         ( is', js' ) -> ( (Fst :$ k1k2) `ICons` is', (Snd :$ k1k2) `ICons` js' )

opticalTree :: forall k is (s :: k) a (optic :: Optic is s a).
               ASTIndexList is -> SOptic optic -> CGMonad OpticalTree
opticalTree (i `ICons` _) (SAnIndexV   _) = Node Unsafe . Leaf . fst <$> codeGen i
opticalTree (i `ICons` _) (SAnIndexRTA _) = Node Unsafe . Leaf . fst <$> codeGen i
opticalTree (i `ICons` _) (SAnIndexA   _) = Node Unsafe . Leaf . fst <$> codeGen i
opticalTree _ (SIndex n_px)
  = let n :: Word32
        n = fromIntegral ( natVal n_px )
    in Node Safe . Leaf <$> constID n
opticalTree _ (SName k bds)
  = let n :: Word32
        n = fieldIndex k bds
    in Node Safe . Leaf <$> constID n
opticalTree is (SComposeO lg1 opt1 opt2)
  = do  let (is1, is2) = composedIndices lg1 is
        res1 <- opticalTree is1 opt1
        res2 <- opticalTree is2 opt2
        pure ( res1 `continue` res2 )
    where continue :: OpticalTree -> OpticalTree -> OpticalTree
          continue (Node safe1 (Leaf i) ) next@(Node safe2 _)
            = Node (safe1 <> safe2) ( Continue  i next )
          continue (Node safe1 (Continue i t) ) next@(Node safe2 _)
            = Node (safe1 <> safe2) ( Continue i (t `continue` next) )
          continue (Node safe1 (Combine ts)) next@(Node safe2 _)
            = Node (safe1 <> safe2)
            . Combine
            $ map (`continue` next) ts
opticalTree is (SProductO lg1 lg2 o1 o2)
  = do  let (is1, is2) = combinedIndices lg1 lg2 is
        t1 <- opticalTree is1 o1
        t2 <- opticalTree is2 o2
        let combined = case ( t1, t2 ) of
              ( Node safe1 n1, Node safe2 n2 )
                -> let children = case ( n1, n2 ) of
                          (Combine ts1, Combine ts2) -> ts1 ++ ts2
                          (Combine ts1, _          ) -> ts1 ++ [t2]
                          (_          , Combine ts2) -> t1 : ts2
                          (_          , _          ) -> [t1, t2]
                   in Node (safe1 <> safe2) (Combine children)
        pure combined
opticalTree _ (SBinding _)
  = throwError "opticalTree: trying to access a binding within a binding"
opticalTree is (SJoint opt) = error "todo"



loadThroughAccessChain
  :: MonadError Text m
  => ID -> [ID] -> SOptic optic -> m (ID, SPIRV.PrimTy)
loadThroughAccessChain basePtrID indices soptic
  = throwError "loadThroughAccessChain: todo"

extractUsingGetter
  :: MonadError Text m
  => ID -> [ID] -> SOptic optic -> m (ID, SPIRV.PrimTy)
extractUsingGetter baseID indices soptic
  = throwError "extractUsingGetter: todo"

storeThroughAccessChain
  :: MonadError Text m
  => ID -> ID -> [ID] -> SOptic optic -> m ()
storeThroughAccessChain ptrID valID indices soptic
  = throwError "storeThroughAccessChain: todo"

insertUsingSetter
  :: MonadError Text m
  => ID -> ID -> [ID] -> SOptic optic -> m ()
insertUsingSetter baseID valID indices soptic
  = throwError "insertUsingSetter: todo"
