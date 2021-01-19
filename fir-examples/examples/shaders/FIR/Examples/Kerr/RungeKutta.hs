{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedWildCards         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE UnicodeSyntax          #-}

module FIR.Examples.Kerr.RungeKutta where

-- fir
import FIR

------------------------------------------------

a₂₁, a₃₁, a₃₂, a₄₁, a₄₂, a₄₃, a₅₁, a₅₂, a₅₃, a₅₄ :: ( PrimTy a, DivisionRing a ) => Code a
a₂₁ = Lit $ 1/5
a₃₁ = Lit $ 3/40
a₃₂ = Lit $ 9/40
a₄₁ = Lit $ 44/45
a₄₂ = Lit $ -56/15
a₄₃ = Lit $ 32/9
a₅₁ = Lit $ 19372/6561
a₅₂ = Lit $ -25360/2187
a₅₃ = Lit $ 64448/6561
a₅₄ = Lit $ -212/729

a₆₁, a₆₂, a₆₃, a₆₄, a₆₅, a₇₁, a₇₃, a₇₄, a₇₅, a₇₆ :: ( PrimTy a, DivisionRing a ) => Code a
a₆₁ = Lit $ 9017/3168
a₆₂ = Lit $ -355/33
a₆₃ = Lit $ 46732/5247
a₆₄ = Lit $ 49/176
a₆₅ = Lit $ -5103/18656
a₇₁ = Lit $ 35/384
-- a₇₂ = Lit $ 0
a₇₃ = Lit $ 500/1113
a₇₄ = Lit $ 125/192
a₇₅ = Lit $ -2187/6784
a₇₆ = Lit $ 11/84

c₂, c₃, c₄, c₅ :: ( PrimTy a, DivisionRing a ) => Code a
c₂  = Lit $ 1 / 5
c₃  = Lit $ 3 / 10
c₄  = Lit $ 4 / 5
c₅  = Lit $ 8 / 9
-- c₆  = Lit $ 1
-- c₇  = Lit $ 1

b₁, b₃, b₄, b₅, b₆ :: ( PrimTy a, DivisionRing a ) => Code a
b₁  = Lit $ 35/384
--b₂  = Lit $ 0
b₃  = Lit $ 500/1113
b₄  = Lit $ 125/192
b₅  = Lit $ -2187/6784
b₆  = Lit $ 11/84
--b₇  = Lit $ 0

d₁, d₃, d₄, d₅, d₆, d₇ :: ( PrimTy a, DivisionRing a ) => Code a
d₁ = Lit $ (35/384    ) - (5179/57600   )
-- d₂ = Lit $ (0         ) - (0            )
d₃ = Lit $ (500/1113  ) - (7571/16695   )
d₄ = Lit $ (125/192   ) - (393/640      )
d₅ = Lit $ (-2187/6784) - (-92097/339200)
d₆ = Lit $ (11/84     ) - (187/2100     )
d₇ = Lit $ (0         ) - (1/40         )

type StepResult a
  = Struct
      '[ "continue" ':-> Bool
       , "data"     ':-> a
       ]

data StepData t y
  = StepData
  { stepOrigin :: ( Code t, Code y )
  , stepSize   :: ( Code t, Code y )
  }

-- defining a 'Syntactic" instance by hand, hopefully this can be automated using deriving via in the future
instance ( Integrable t y ) => Syntactic (StepData t y) where
  type Internal (StepData t y) =
    Val
      ( Struct
        '[ "origin" ':-> InternalType ( Code t, Code y )
         , "step"   ':-> InternalType ( Code t, Code y )
         ]
      )
  toAST (StepData origin size) = Struct ( toAST origin :& toAST size :& End )
  fromAST struct =
    StepData
      ( fromAST $ view @(Field "origin") struct )
      ( fromAST $ view @(Field "step"  ) struct )


infixl 6 ^+^
infix  8 *^
class ( Semiring t, DivisionRing t, Floating t
      , ScalarTy t
      , Eq t, Logic t ~ Bool, Ord t
      , PrimTy y
      ) => Integrable t y | y -> t where
  (^+^) :: Code y -> Code y -> Code y
  (*^)  :: Code t -> Code y -> Code y
  absV  :: Code y -> Code y
  maxAdaptiveStepFactor
     :: Code y -> Code y
     -> ( Code t -> Code t -> Code t )
     -> Code t


data Parameters t y a
  = Parameters
  { start         :: ( Code t, Code y, Code a )
  , function      :: forall st. Code t -> Code y -> Program st st (Code y)
  , tolerances    :: forall st. Code t -> Code y -> Program st st (Code y)
  , stepper       :: forall st. StepData t y -> Code a -> Program st st (Code (StepResult a))
  , startStepSize :: Code t
  , minStepSize   :: forall st. Code t -> Code y -> Program st st (Code t)
  , maxStepSize   :: forall st. Code t -> Code y -> Program st st (Code t)
  , maxIterations :: Code Word32
  }

dormandPrince :: forall t y a s
              .  ( PrimTy a, HasOpaqueType a ~ False, HasOpaqueType y ~ False
                 , Integrable t y
                 )
              => Parameters t y a -> Program s s (Code a)
dormandPrince
  Parameters
    { start         = ( t₀, y₀, a₀ )
    , function      = f
    , tolerances    = tolFunction
    , stepper       = nextStep
    , startStepSize = hInit
    , minStepSize   = hMinFunction
    , maxStepSize   = hMaxFunction
    , maxIterations = maxIters
    }
  = purely do

    -- initialisations...
    _ <- def @"iter"     @RW ( 0 :: Code Word32 )
    _ <- def @"t"        @RW t₀
    _ <- def @"h"        @RW hInit
    _ <- def @"y"        @RW y₀

    _ <- def @"i" @RW @Word32 0

    _ <- def @"data" @RW @a $ a₀

    loop do

      t <- get @"t"
      h <- get @"h"

      y  <- get @"y"
      k₁ <- let' =<< f  t          y
      k₂ <- let' =<< f (t + c₂*h) (y ^+^ h*^(a₂₁*^k₁))
      k₃ <- let' =<< f (t + c₃*h) (y ^+^ h*^(a₃₁*^k₁ ^+^ a₃₂*^k₂))
      k₄ <- let' =<< f (t + c₄*h) (y ^+^ h*^(a₄₁*^k₁ ^+^ a₄₂*^k₂ ^+^ a₄₃*^k₃))
      k₅ <- let' =<< f (t + c₅*h) (y ^+^ h*^(a₅₁*^k₁ ^+^ a₅₂*^k₂ ^+^ a₅₃*^k₃ ^+^ a₅₄*^k₄))
      k₆ <- let' =<< f (t +    h) (y ^+^ h*^(a₆₁*^k₁ ^+^ a₆₂*^k₂ ^+^ a₆₃*^k₃ ^+^ a₆₄*^k₄ ^+^ a₆₅*^k₅))
      k₇ <- let' =<< f (t +    h) (y ^+^ h*^(a₇₁*^k₁             ^+^ a₇₃*^k₃ ^+^ a₇₄*^k₄ ^+^ a₇₅*^k₅ ^+^ a₇₆*^k₆))

      err <- let' ( absV ( d₁*^k₁    ^+^ d₃*^k₃ ^+^ d₄*^k₄ ^+^ d₅*^k₅ ^+^ d₆*^k₆ ^+^ d₇*^k₇ ) )
      y'  <- let'        ( b₁*^k₁    ^+^ b₃*^k₃ ^+^ b₄*^k₄ ^+^ b₅*^k₅ ^+^ b₆*^k₆            )

      tols <- let' =<< tolFunction  t y
      hMin <- let' =<< hMinFunction t y
      hMax <- let' =<< hMaxFunction t y
      let delta0
            = maxAdaptiveStepFactor err tols
            $ \errr tol -> 0.8 * ( tol / errr ) ** 0.2
          delta = min 4 . max 0.1 $ delta0
      modify @"h" ( min hMax . max hMin . ( * delta ) )

      if delta < 0.8 -- too much error
      then do
        new_h <- get @"h"
        when ( new_h >= h ) ( break @1 )
      else do
        a <- get @"data"
        let
          stepData :: StepData t y
          stepData = StepData
            { stepOrigin = (t, y )
            , stepSize   = (h, y')
            }
        stepResult <- let' =<< nextStep stepData a
        put @"data" $ view @(Name "data") stepResult
        unless ( view @(Name "continue") stepResult ) ( break @1 )
        modify @"t" (+ h)
        modify @"y" (^+^ (h *^ y'))

      modify @"iter" (+1)
      nextIter <- get @"iter"
      unless (nextIter < maxIters) ( break @1 )

    get @"data"
