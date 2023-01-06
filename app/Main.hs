module Main (main) where

import Data.Word (Word64)
import Microthesis (Options(..), Test, TestResult(..), FailsWithShrinksData(..))
import Microthesis.Gen (Gen)
import qualified Data.List as List
import qualified Microthesis as M
import qualified Microthesis.Gen as Gen
import qualified Microthesis.SystemGen as SystemGen
import qualified System.Random as Random

example1 :: Test Int
example1 = 
  M.test "Shrunk values hold generation invariants" 
    (fmap (* 100) $ Gen.uniformInt 20)
    (\n -> n < 321)

expected1 :: Int
expected1 = 400

example2 :: Test [Int]
example2 = 
  M.test "Monadic bind can shrink both 'before' and 'after'" 
    (naiveListGen (Gen.uniformInt 20))
    (\list -> list == List.sort list)

expected2 :: [Int]
expected2 = [1,0]

naiveListGen :: Gen a -> Gen [a]
naiveListGen innerGen = do
  len <- Gen.uniformInt 5
  go len []
    where
      go 0    acc = pure $ reverse acc
      go todo acc = do
        nextItem <- innerGen
        go (todo - 1) (nextItem : acc)

main :: IO ()
main = do
  seed <- Random.randomIO
  putStrLn $ "Seed: " ++ show seed ++ "\n"
  print example1
  let result1 = runTest seed example1
  print result1
  putStrLn ""
  print example2
  let result2 = runTest seed example2
  print result2
  case (result1,result2) of
    (FailsWithShrinks f1,FailsWithShrinks f2) ->
      if f1.finalValue == expected1 && f2.finalValue == expected2
         then pure ()
         else error "Didn't shrink like we expected to"
    _ -> error "Unexpected result"

runTest :: Word64 -> Test a -> TestResult a
runTest seed =
  M.runWith 
    (M.defaultOptions { showShrinkHistory = True }) 
    (SystemGen.fromSeed seed) 
