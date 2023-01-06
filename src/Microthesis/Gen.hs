module Microthesis.Gen
    ( Gen, run, replay
    , uniformInt
    )
    where

import Microthesis.GenPhase (GenPhase(..))
import Microthesis.Recording (Recording)
import Microthesis.SystemGen (SystemGen)
import qualified Data.List.Zipper as ListZipper
import qualified Microthesis.GenPhase as GenPhase
import qualified Microthesis.Recording as Recording
import qualified System.Random as Random

data Gen a =
    Gen (GenPhase -> (Either String a, GenPhase))

instance Functor Gen where
  fmap f (Gen g) = Gen $ \phase ->
    case g phase of
      (Right val, phase') -> (Right (f val), phase')
      (Left err,  phase') -> (Left err,      phase')

instance Applicative Gen where
  pure x = Gen $ \phase -> (Right x, phase)
  Gen genFn <*> Gen genA = Gen $ \phase ->
    case genFn phase of
      (Left err, phase') -> (Left err, phase')
      (Right fn, phase') ->
        case genA phase' of
          (Left err, phase'') -> (Left err, phase'')
          (Right a,  phase'') -> (Right (fn a), phase'')
      
instance Monad Gen where
  Gen g >>= f = Gen $ \phase ->
    case g phase of
      (Left err, phase') -> (Left err, phase')
      (Right val, phase') -> 
        let (Gen g') = f val in
        g' phase'

run :: GenPhase -> Gen a -> (Either String a, GenPhase)
run phase (Gen fn) = fn phase

replay :: Recording -> Gen a -> (Either String a, GenPhase)
replay recording gen = run (GenPhase.replay recording) gen

{-| Draw an integer between 0 and n inclusive.
Will shrink towards 0, but draws uniformly over the whole range.
-}
uniformInt :: Int -> Gen Int
uniformInt n =
  rollDice n $ Random.uniformR (0,n)

rollDice :: Int -> (SystemGen -> (Int, SystemGen)) -> Gen Int
rollDice maxValue next = Gen $ \phase ->
  let err msg = (Left ("Microthesis.Gen.rollDict: " ++ msg), phase) in
  if Recording.isFull $ GenPhase.getRecording phase
     then err "Your generators have hit the max size of Recording (generating too much data)"
     else
       case phase of
         Generation systemGen recording ->
           let (roll, newGen) = next systemGen in
           if roll < 0 then err "Bug: generated a choice < 0"
           else if roll > maxValue then err "Bug: generated a choice > maxChoice"
           else ( Right roll
                , GenPhase.Generation newGen (Recording.append roll recording)
                )

         Replay zipper ->
           case ListZipper.safeCursor zipper of
             Nothing -> err "Replay recording run out of numbers"
             Just choice ->
               if choice < 0 then err "Bug: generated a choice < 0"
               else if choice > maxValue then err "Bug: generated a choice > maxChoice"
               else ( Right choice
                    , GenPhase.Replay (ListZipper.right zipper)
                    )
