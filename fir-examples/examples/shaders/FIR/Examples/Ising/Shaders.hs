{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module FIR.Examples.Ising.Shaders where

-- base
import qualified Prelude
import Control.Monad
  ( sequence_ )
import Data.Proxy
  ( Proxy(..) )
import Data.Type.Bool
  ( If )
import Data.Type.Equality
  ( type (==) )
import Data.Maybe
  ( fromJust )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( KnownNat, type (*), Mod
  , natVal
  )

-- filepath
import System.FilePath
  ( (</>) )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- text-short
import Data.Text.Short
  ( ShortText )

-- fir
import FIR
import Math.Linear

-- fir-examples
import FIR.Examples.Paths
  ( shaderDir )

------------------------------------------------
-- Work sizes.

type Width      = 1280 `WithDivisor` ( 2 * LocalSizeX )
type Height     = 720  `WithDivisor` LocalSizeY
type SS         = 4
type LocalSizeX = 16
type LocalSizeY = 8
width, height, ss, localSizeX, localSizeY :: Semiring a => a
width      = fromInteger ( Prelude.toInteger $ natVal ( Proxy @Width ) )
height     = fromInteger ( Prelude.toInteger $ natVal ( Proxy @Height ) )
ss         = fromInteger ( Prelude.toInteger $ natVal ( Proxy @SS ) )
localSizeX = fromInteger ( Prelude.toInteger $ natVal ( Proxy @LocalSizeX ) )
localSizeY = fromInteger ( Prelude.toInteger $ natVal ( Proxy @LocalSizeY ) )

checkerboardSize :: Word32
checkerboardSize = ( ss * ss * width * height ) `div` 2

-- Step algorithm sizes:
-- Underlying lattice has size ( SS * Width ) * ( SS * Height ),
-- but we only handle half at a time (checkerboard pattern).
-- globalSizeX * localSizeX = SS * Width / 2
-- globalSizeY * localSizeY = SS * Height

-- Resolve algorithm size:
-- globalSizeX * localSizeX = Width
-- globalSizeY * localSizeY = Height

infixl 7 `WithDivisor`
type family n `WithDivisor` d where
  n `WithDivisor` d =
    If ( n `Mod` d == 0 )
      n
      ( TypeError ( ShowType d :<>: Text " does not divide " :<>: ShowType n ) )

------------------------------------------------
-- Ising model simulation.

data Parity = Even | Odd
data SParity ( parity :: Parity ) where
  SEven :: SParity Even
  SOdd  :: SParity Odd
parity :: SParity bool -> Parity
parity SEven = Even
parity SOdd  = Odd

type IsingParameters =
  Struct 
   '[ "temperature"   ':-> Float
    , "interaction"   ':-> Float
    , "magneticField" ':-> Float
    , "time"          ':-> Float
    , "reset"         ':-> Word32
    ]

-- Descriptor sets:
-- 
-- Descriptor set 0:     ubo,     output, outEvenSpins, outOddSpins
-- Descriptor set 1: prevUBO, prevOutput,  inEvenSpins,  inOddSpins
--                   ^^^^^^^  ^^^^^^^^^^
--                      ^---------^--------- unused
-- Even step (assumed to be take place first):
--   inEvenSpins, inOddSpins -> outEvenSpins
-- Odd step:
--  outEvenSpins, inOddSpins -> outOddSpins
-- Resolve step:
--  outEvenSpins, outOddSpins -> output
type StepDefs ( parity :: Parity ) =
  '[ "ubo"        ':-> Uniform '[ DescriptorSet 0, Binding 0 ] IsingParameters
   , "evenSpins"  ':-> Image2D 
                         '[ DescriptorSet ( If ( parity == Even ) 1 0 )
                          , Binding 2
                          , NonWritable
                          ]
                          ( R32 F )
   , "oddSpins"   ':-> Image2D 
                         '[ DescriptorSet 1
                          , Binding 3
                          , NonWritable
                          ]
                          ( R32 F )
   , "outSpins"   ':-> Image2D 
                         '[ DescriptorSet 0
                          , Binding ( If ( parity == Even ) 2 3 )
                          , NonReadable
                          ]
                          ( R32 F )
   , "localSpins" ':-> Workgroup '[] ( Array ( LocalSizeX * LocalSizeY ) Float )
   , "main"       ':-> EntryPoint '[ LocalSize LocalSizeX LocalSizeY 1 ] Compute
   ]

-- | Update all spins of lattice sites with the given checkerboard parity.
stepShader :: forall parity. _ => SParity parity -> Module ( StepDefs parity )
stepShader sParity = Module $ entryPoint @"main" @Compute do

    isingParameters <- get @"ubo"

    localIndex@( ~( Vec2 i_local_x i_local_y ) )
      <- use @( Name "gl_LocalInvocationID"  :.: Swizzle "xy" )
    globalIndex
      <- use @( Name "gl_GlobalInvocationID" :.: Swizzle "xy" )

    localArrayIndex  <- let' $ i_local_x + localSizeX * i_local_y

    -- Read spins at current index, and write spins for lattice sites of opposite checkerboard colouring into local storage.
    evenSpin <- imageRead @"evenSpins" globalIndex
    oddSpin  <- imageRead @"oddSpins"  globalIndex

    let
      currentSpin, otherSpin :: Code Float
      ( currentSpin, otherSpin ) = case sParity of
        SEven -> ( evenSpin,  oddSpin )
        SOdd  -> (  oddSpin, evenSpin )
    assign @( Name "localSpins" :.: AnIndex Word32 ) localArrayIndex otherSpin
    
    -- Wait until all invocations in the workgroup have written the spin values into local storage.
    controlBarrier Workgroup Nothing

    -- Simulation step: update spins of given parity by reading spins of opposite parity.
    -- We can read the spins from local storage, except at the boundary of the workgroup.
    neighbourSpins <- lookupNeighbourSpins sParity otherSpin localIndex globalIndex
    newCurrentSpin <- updateSpin sParity isingParameters globalIndex currentSpin neighbourSpins

    imageWrite @"outSpins" globalIndex newCurrentSpin


updateSpin
  :: SParity parity -> Code IsingParameters -> Code ( V 2 Word32 )
  -> Code Float -> Code ( V 4 Float ) -> Program _s _s ( Code Float )
updateSpin sParity isingParameters ( Vec2 ix iy ) s ( Vec4 u l r d ) = do
  temperature   <- let' $ view @( Name "temperature"   ) isingParameters
  interaction   <- let' $ view @( Name "interaction"   ) isingParameters
  magneticField <- let' $ view @( Name "magneticField" ) isingParameters
  time          <- let' $ view @( Name "time"          ) isingParameters
  reset         <- let' $ view @( Name "reset"         ) isingParameters

  -- Generate a random probability using a hash function.
  -- The probabilities should be as decorrelated as possible
  --  - between different lattice sites (x and y indices as well as parity);
  --  - over time.
  ~( Vec3 hx hy _ ) <- let' @( Code ( V 3 Word32 ) ) =<< pcg3d ( Vec3 ix iy ( floor $ time * 1000 ) )
  prob <- let'
    ( word32ToProbability $ case sParity of { SEven -> hx; _ -> hy } )

  -- Change in Hamiltonian resuling from flipping the current spin.
  delta <- let' $ 2 * s * ( magneticField + interaction * ( u + l + r + d ) )
  pure $
    if reset > 0 then ( if prob <= 0.5 then (-1) else 1 ) -- reset the grid
    else
      -- Metropolis–Hastings algorithm.
      -- Flip this lattice site if this would decrease energy (delta < 0)
      -- or if the temperature is high enough.
      if prob <= exp ( - delta / temperature )
      then negate s
      else s

lookupNeighbourSpins
  :: forall parity _s
  .  _
  => SParity parity
  -> Code Float
  -> Code ( V 2 Word32 ) -> Code ( V 2 Word32 )
  -> Program _s _s ( Code ( V 4 Float ) )
lookupNeighbourSpins sParity otherSpin ( Vec2 i_local_x i_local_y ) ( Vec2 i_global_x i_global_y ) = locally do

  -- Lattice site above: decrement y-index by 1.
  spinU <-
    if i_local_y > 0
    then use @( Name "localSpins" :.: AnIndex Word32 ) ( i_local_x + localSizeX * ( i_local_y - 1 ) )
    else do
      let
        neighbourGlobalIndex :: Code ( V 2 Word32 )
        neighbourGlobalIndex =
          if i_global_y > 0
          then Vec2 i_global_x ( i_global_y - 1 )
          else Vec2 i_global_x      lastRow
      readGlobalSpin neighbourGlobalIndex

  -- Side lattice sites:
  -- - if parity of row equals parity of current lattice site
  --    * decrement x-index by 1 for left spin
  --    * right spin is at current index (which we already know)
  -- - otherwise
  --    * left spin is at current index (which we already know)
  --    * increment x-index by 1 for right spin
  ~( Vec2 spinL spinR ) <-
    if ( case sParity of { SEven -> i_global_x `mod` 2 == 0 ; SOdd -> i_global_x `mod` 2 == 1 } :: Code Bool )
    then do
      spinL <-
        if i_local_x > 0
        then use @( Name "localSpins" :.: AnIndex Word32 ) ( i_local_x - 1 + localSizeX * i_local_y )
        else do
          let
            neighbourGlobalIndex :: Code ( V 2 Word32 )
            neighbourGlobalIndex =
              if i_global_x > 0
              then Vec2 ( i_global_x - 1 ) i_global_y
              else Vec2     lastColumn     i_global_y
          readGlobalSpin neighbourGlobalIndex
      pure $ Vec2 spinL otherSpin
    else do
      spinR <-
        if i_local_x < localSizeX - 1
        then use @( Name "localSpins" :.: AnIndex Word32 ) ( i_local_x + 1 + localSizeX * i_local_y )
        else do
          let
            neighbourGlobalIndex :: Code ( V 2 Word32 )
            neighbourGlobalIndex =
              if i_global_x < lastColumn
              then Vec2 ( i_global_x + 1 ) i_global_y
              else Vec2         0          i_global_y
          readGlobalSpin neighbourGlobalIndex
      pure $ Vec2 otherSpin spinR

  -- Lattice site below: increment y-index by 1.
  spinD <-
    if i_local_y < localSizeY - 1
    then use @( Name "localSpins" :.: AnIndex Word32 ) ( i_local_x + localSizeX * ( i_local_y + 1 ) )
    else do
      let
        neighbourGlobalIndex :: Code ( V 2 Word32 )
        neighbourGlobalIndex =
          if i_global_y < lastRow
          then Vec2 i_global_x ( i_global_y + 1 )
          else Vec2 i_global_x         0
      readGlobalSpin neighbourGlobalIndex

  let' ( Vec4 spinU spinL spinR spinD )

  where
    lastColumn, lastRow :: Code Word32
    lastColumn = Lit . fromIntegral $ ( ( width * ss ) `div` 2 - 1 :: Int32 )
    lastRow    = Lit . fromIntegral $ ( height * ss - 1 :: Int32 )
    readGlobalSpin :: _ => Code ( V 2 Word32 ) -> Program _s _s ( Code Float )
    readGlobalSpin = case parity sParity of
      Even -> imageRead @"oddSpins"
      Odd  -> imageRead @"evenSpins"

pcg3d :: Code ( V 3 Word32 ) -> Program s s ( Code ( V 3 Word32 ) )
pcg3d v = purely do
  ~( Vec3 x y z ) <- def @"v" @RW ( 1664525 *^ v ^+^ pureAST 1013904223 )
  x <- def @"x" @RW ( x + y * z )
  y <- def @"y" @RW ( y + z * x )
  z <- def @"z" @RW ( z + x * y )
  let
    scramble :: Code ( V 3 Word32 ) -> Code ( V 3 Word32 )
    scramble = fmapAST ( \ a -> a `xor` ( a `shiftR` ( 16 :: Code Word32 ) ) )
  ~( Vec3 x y z ) <- ( put @"v" $ scramble ( Vec3 x y z ) ) *>> get @"v"
  x <- put @"x" ( x + y * z ) *>> get @"x"
  y <- put @"y" ( y + z * x ) *>> get @"y"
  z <- put @"z" ( z + x * y ) *>> get @"z"
  pure ( Vec3 x y z )

word32ToProbability :: Code Word32 -> Code Float
word32ToProbability x =
  bitcast ( ( x `shiftR` ( 9 :: Code Word32 ) ) .|. 0x3f800000 ) - 1.0

------------------------------------------------
-- Supersampling.

type ResolveDefs =
  '[ "outputImage"':-> Image2D '[ DescriptorSet 0, Binding 1, NonReadable ] ( RGBA8 UNorm )
   , "evenSpins"  ':-> Image2D '[ DescriptorSet 0, Binding 2, NonWritable ] ( R32 F )
   , "oddSpins"   ':-> Image2D '[ DescriptorSet 0, Binding 3, NonWritable ] ( R32 F )
   , "main"       ':-> EntryPoint '[ LocalSize LocalSizeX LocalSizeY 1 ] Compute
   ]

resolveShader :: Module ResolveDefs
resolveShader = Module $ entryPoint @"main" @Compute do

  globalIndex@( ~( Vec2 i_global_x i_global_y ) )
    <- use @( Name "gl_GlobalInvocationID" :.: Swizzle "xy" )

  -- Compute total spin over all lattice sites that correspond to the current pixel (supersampling).
  _ <- def @"totalSpin" @RW @Float 0
  supersamplingLoop \ ss_x ss_y -> locally do
    checkerboardIndex <-
      def @"checkerboardIndex" @R @( V 2 Word32 ) $
        Vec2
          ( ( ss_x + ss * i_global_x ) `div` 2 )
          ( ss_y + ss * i_global_y )
    spin <-
      if ( ss_x + ss_y + ss * ( i_global_x + i_global_y ) ) `mod` 2 == 0 -- is the lattice site even?
      then imageRead @"evenSpins" checkerboardIndex
      else imageRead @"oddSpins"  checkerboardIndex
    modify @"totalSpin" ( + spin )
    pure ()
  totalSpin <- get @"totalSpin"

  -- Colour mapping.
  colour <- gradient ( totalSpin / ( ss * ss ) ) ( Lit magneticColours )

  -- Write the result to output image.
  imageWrite @"outputImage" globalIndex colour

supersamplingLoop
  :: ( Code Word32 -> Code Word32 -> Program _s _s () )
  -> Program _s _s ()
supersamplingLoop prog = locally do
  _ <- def @"ssX" @RW @Word32 0
  _ <- def @"ssY" @RW @Word32 0
  while ( ( < ss ) <<$>> get @"ssX") do
    ssX <- get @"ssX"
    put @"ssY" 0
    while ( ( < ss ) <<$>> get @"ssY" ) do
      ssY <- get @"ssY"
      embed ( prog ssX ssY )
      put @"ssY" ( ssY + 1 )
    put @"ssX" ( ssX + 1 )
  pure ()

------------------------------------------------
-- Colour mapping.

-- | Gradient for input values between -1 and 1.
gradient
  :: forall n s
  .  KnownNat n
  => Code Float
  -> Code ( Array n ( V 4 Float ) )
  -> Program s s ( Code (V 4 Float) )
gradient t colors = do
  t'   <- let' $ 0.5 * (t+1)
  n    <- let' $ Lit ( fromIntegral $ knownValue @n )
  i    <- let' $ ( floor ( (n-1) * t' ) :: Code Word32 )
  s    <- let' $ (n-1) * t' - fromIntegral i
  cols <- let' $ colors
  res  <- let'
    (    ( (1-s) *^ ( view @(AnIndex _)  i    cols ) )
     ^+^ (    s  *^ ( view @(AnIndex _) (i+1) cols ) )
    )
  pure res

magneticColours :: Array 3 (V 4 Float)
magneticColours =
  MkArray . fromJust . Vector.fromList $
    [ V4 0.9 0.9  0.5  1
    , V4 0.4 0.7  0.9  1
    , V4 0.4 0.1  0.3  1
    ]
{-
    [ V4 0.9 0.3  0.25 1
    , V4 0.1 0.05 0.15 1
    , V4 0.2 0.3  0.8  1
    ]
-}

------------------------------------------------
-- compiling

evenStepPath, oddStepPath, resolvePath :: FilePath
evenStepPath = shaderDir </> "ising_even_comp.spv"
oddStepPath  = shaderDir </> "ising_odd_comp.spv"
resolvePath  = shaderDir </> "ising_resolve_comp.spv"

compileEvenStepShader, compileOddStepShader, compileResolveShader :: IO ( Either ShortText ModuleRequirements )
compileEvenStepShader = compileTo evenStepPath [Debug, SPIRV $ Version 1 0] ( stepShader SEven )
compileOddStepShader  = compileTo  oddStepPath [Debug, SPIRV $ Version 1 0] ( stepShader SOdd  )
compileResolveShader  = compileTo  resolvePath [Debug, SPIRV $ Version 1 0] resolveShader

compileAllShaders :: IO ()
compileAllShaders = sequence_
  [ compileEvenStepShader
  , compileOddStepShader
  , compileResolveShader
  ]
