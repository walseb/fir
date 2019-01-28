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
import {-# SOURCE #-} CodeGen.CodeGen(codeGen)
import CodeGen.IDs(constID)
import CodeGen.Instruction(ID)
import CodeGen.Monad(CGMonad)
import Control.Type.Optic(Optic, ProductIndices)
import Data.Type.Map(type (:++:), SLength(SZero, SSucc))
import FIR.AST(AST((:$), Fst, Snd), ASTs(NilAST,ConsAST))
import FIR.Instances.Optics(SOptic(..))
import FIR.Prim.Singletons(HasField(fieldIndex))
import qualified SPIRV.PrimTy as SPIRV

----------------------------------------------------------------------------
-- optics

data OpticalNode
  = Leaf     ID
  | Continue ID OpticalTree
  | Combine  [OpticalTree]
  | Join     OpticalTree

data OpticalTree = Node IndexSafeness OpticalNode

data IndexSafeness
  = Unsafe
  | Safe
  deriving ( Eq, Show )

instance Semigroup IndexSafeness where
  Safe <> x = x
  _    <> _ = Unsafe

composedIndices :: SLength is -> ASTs (is :++: js) -> (ASTs is, ASTs js)
composedIndices SZero js = ( NilAST, js )
composedIndices (SSucc tail_is) (k `ConsAST` ks)
  = first ( k `ConsAST` ) (composedIndices tail_is ks)

combinedIndices :: SLength is -> SLength js -> ASTs (ProductIndices is js) -> (ASTs is, ASTs js)
combinedIndices SZero SZero _ = ( NilAST, NilAST )
combinedIndices SZero (SSucc _) ks = ( NilAST, ks )
combinedIndices (SSucc _) SZero ks = ( ks, NilAST )
combinedIndices (SSucc is) (SSucc js) (k1k2 `ConsAST` ks)
  = case combinedIndices is js ks of
         ( is', js' ) -> ( (Fst :$ k1k2) `ConsAST` is', (Snd :$ k1k2) `ConsAST` js' )

opticalTree :: forall k is (s :: k) a (optic :: Optic is s a).
               ASTs is -> SOptic optic -> CGMonad OpticalTree
opticalTree (i `ConsAST` _) (SAnIndexV   _) = Node Unsafe . Leaf . fst <$> codeGen i
opticalTree (i `ConsAST` _) (SAnIndexRTA _) = Node Unsafe . Leaf . fst <$> codeGen i
opticalTree (i `ConsAST` _) (SAnIndexA   _) = Node Unsafe . Leaf . fst <$> codeGen i
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
            = Node (safe1 <> safe2) ( Continue i next )
          continue (Node safe1 (Continue i t) ) next@(Node safe2 _)
            = Node (safe1 <> safe2) ( Continue i (t `continue` next) )
          continue (Node safe1 (Combine ts)) next@(Node safe2 _)
            = Node (safe1 <> safe2)
            . Combine
            $ map (`continue` next) ts
          continue (Node safe1 (Join o)) next@(Node safe2 _)
            = Node (safe1 <> safe2)
            . Join
            $ continue o next
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
