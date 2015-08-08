module Random (getContestGen) where

{-

The pseudo-random sequence of numbers will be computed from a given
seed using a linear congruential generator with the following
parameters: modulus: 232 multiplier: 1103515245 increment: 12345

https://en.wikipedia.org/wiki/Linear_congruential_generator

The random number associated with a seed consists of bits 30..16 of
that seed, where bit 0 is the least significant bit. For example, here
are the first 10 outputs for the sequence starting with seed 17:
0,24107,16552,12125,9427,13152,21440,3383,6873,16117.

-}

import           Data.Bits     (shiftR, (.&.))
import           System.Random (RandomGen (..))

data GeneratorParams = GeneratorParams { gpModulus    :: Int
                                       , gpMultiplier :: Int
                                       , gpIncrement  :: Int
                                       } deriving Show

standardParams :: GeneratorParams
standardParams = GeneratorParams 4294967296 1103515245 12345


nextSeed :: GeneratorParams -> Int -> Int
nextSeed params prev = ((gpMultiplier params) * prev + (gpIncrement params)) `mod` (gpModulus params)


data LinearCongruentialGenerator = LinearCongruentialGenerator { genSeed   :: Int
                                                               , genParams :: GeneratorParams
                                                               } deriving Show

instance RandomGen LinearCongruentialGenerator where
  next gen = let generated = nextSeed (genParams gen) (genSeed gen)
                 num = shiftR (genSeed gen .&. 0x7fff0000) 16
             in (num, gen { genSeed = generated })
  split = undefined


getContestGen :: Int -> LinearCongruentialGenerator
getContestGen seed = LinearCongruentialGenerator seed standardParams

