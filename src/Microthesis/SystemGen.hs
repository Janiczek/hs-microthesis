module Microthesis.SystemGen (SystemGen, fromSeed) where

import Data.Word (Word64)
import System.Random (RandomGen)
import System.Random.SplitMix (SMGen)
import qualified System.Random
import qualified System.Random.SplitMix as SplitMix

newtype SystemGen = SystemGen SMGen
instance RandomGen SystemGen where
  genRange _ = (minBound, maxBound)
  next  (SystemGen g) = case SplitMix.nextInt g of (x, g') -> (x, SystemGen g')
  split (SystemGen g) =
    case SplitMix.splitSMGen g of (g1, g2) -> (SystemGen g1, SystemGen g2)

fromSeed :: Word64 -> SystemGen
fromSeed n = SystemGen $ SplitMix.mkSMGen n
