module Microthesis
    ( Test, test
    , TestResult(..), run, runWith
    , Options(..), defaultOptions
    , Recording
    , FailsWithShrinksData(..), CannotGenerateValuesData(..)
    ) where

import Data.Function ((&))
import Data.Map (Map)
import Microthesis.Gen (Gen)
import Microthesis.Recording (Recording)
import Microthesis.SystemGen (SystemGen)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Microthesis.Gen as Gen
import qualified Microthesis.GenPhase as GenPhase
import qualified Microthesis.Recording as Recording
import qualified Microthesis.Shrink as Shrink

data Options = Options
    { maxExamples :: Int
    , showShrinkHistory :: Bool
    }

defaultOptions :: Options
defaultOptions =
    Options
        { maxExamples = 100
        , showShrinkHistory = False
        }

data Test a = Test
    { label_ :: String
    , generator :: Gen a
    , userTestFn :: a -> Bool
    }

instance Show (Test a) where
  show t = t.label_

data TestResult a
    = Passes
    | FailsWith a
    | FailsWithShrinks (FailsWithShrinksData a)
    | CannotGenerateValues CannotGenerateValuesData

instance Show a => Show (TestResult a) where
  show = \case
    Passes -> "Passes"
    FailsWith a -> "Fails with counterexample: " ++ show a
    FailsWithShrinks fws -> 
      "Fails with counterexample: " ++ show fws.finalValue ++
      "\n  Recording: " ++ show fws.finalRecording ++
      "\n  Shrink history:\n" ++
          bulletList (map show fws.history)
    CannotGenerateValues cgv ->
      "Cannot generate values, with most common rejections:\n" ++
        bulletList cgv.mostCommonRejections

bulletList :: [String] -> String
bulletList items =
  items
    & map (\s -> "  - " ++ s)
    & List.intercalate "\n" 


data FailsWithShrinksData a =
    FailsWithShrinksData
        { finalValue :: a
        , finalRecording :: Recording
        , history :: [Step a]
        }
    deriving Show

data Step a = Step
    { value :: a
    , recording :: Recording
    , shrinkerUsed :: String
    }
    deriving Show

toShrinkStep :: Shrink.Step a -> Step a
toShrinkStep step =
  Step { value = step.value
       , recording = step.recording
       , shrinkerUsed =
          case step.shrinkerUsed of
            Nothing -> "Initial"
            Just cmd -> show cmd
       }

data CannotGenerateValuesData =
    CannotGenerateValuesData
        { mostCommonRejections :: [String] }
    deriving Show

test :: String -> Gen a -> (a -> Bool) -> Test a
test label generator userTestFn = 
    Test
        { label_ = label
        , generator = generator
        , userTestFn = userTestFn
        }

run :: SystemGen -> Test a -> TestResult a
run = runWith defaultOptions


runWith :: Options -> SystemGen -> Test a -> TestResult a
runWith options systemGen test_ = 
    initLoop options systemGen test_
        & generateAndTest
        & shrink
        & toResult

data LoopState a = LoopState
    -- user input
    { label_ :: String
    , generator :: Gen a
    , userTestFn :: a -> Bool
    , options :: Options
    , maxGenerationAttempts :: Int
    , systemGen :: SystemGen

    -- actual loop state
    , status :: Status a
    , generationAttempts :: Int
    , valuesGenerated :: Int
    , passingTests :: Int
    , rejections :: Map String Int
    }

data Status a
    = Undecided
    | Passing
    | FailingWith (FailingWithData a)
    | UnableToGenerate

data FailingWithData a = FWD
    { value :: a
    , recording :: Recording
    , shrinkHistory :: [Shrink.Step a]
    }

initLoop :: Options -> SystemGen -> Test a -> LoopState a
initLoop options systemGen test_ =
    LoopState
        -- user input
        { label_ = test_.label_
        , generator = test_.generator
        , userTestFn = test_.userTestFn
        , options = options
        , maxGenerationAttempts = options.maxExamples * 10 -- arbitrary.
        , systemGen = systemGen

        -- actual loop state
        , status = Undecided
        , generationAttempts = 0
        , valuesGenerated = 0
        , passingTests = 0
        , rejections = Map.empty
        }

generateAndTest :: LoopState a -> LoopState a
generateAndTest state =
    if shouldTryToGenerate then
        case generate state of
            Left newState -> generateAndTest newState

            Right (recording, value, newState) ->
                if newState.userTestFn value then
                    newState
                        & incPassingTests
                        & generateAndTest
                else
                    newState
                        & setStatus (FailingWith $ FWD
                            { value = value
                            , recording = recording 
                            , shrinkHistory = [ Shrink.Step value recording Nothing ]
                            }
                          )

    else if sawPassingTests then
        setStatus Passing state

    else
        setStatus UnableToGenerate state

    where
        shouldTryToGenerate =
            state.valuesGenerated < state.options.maxExamples
            && state.generationAttempts < state.maxGenerationAttempts

        sawPassingTests = state.passingTests > 0

        incPassingTests (s@LoopState{passingTests}) =
            s { passingTests = passingTests + 1 }

generate :: LoopState a -> Either (LoopState a) (Recording, a, LoopState a)
generate state =
    case Gen.run (GenPhase.generation state.systemGen) state.generator of
        (Left reason, genPhase) ->
            Left $ state { generationAttempts = state.generationAttempts + 1 
                         , rejections = addRejection reason state.rejections
                         , systemGen = Maybe.fromMaybe state.systemGen $ GenPhase.getSystemGen genPhase
                         }
            where
                addRejection :: String -> Map String Int -> Map String Int
                addRejection rejection counter =
                    Map.alter (Just . maybe 0 (+1)) rejection counter

        (Right value, genPhase) ->
            Right
                ( GenPhase.getRecording genPhase
                , value
                , state { generationAttempts = state.generationAttempts + 1
                        , valuesGenerated = state.valuesGenerated + 1
                        , systemGen = Maybe.fromMaybe state.systemGen $ GenPhase.getSystemGen genPhase
                        }
                )

setStatus :: Status a -> LoopState a -> LoopState a
setStatus status state =
    state { status = status }

shrink :: LoopState a -> LoopState a
shrink state = 
    case state.status of
        Undecided        -> state
        Passing          -> state
        UnableToGenerate -> state

        FailingWith (FWD{..}) ->
            if Recording.isEmpty recording then
                -- Can't really improve on []! No randomness was used.
                state
            else
                let
                  finalShrinkState =
                    Shrink.shrink $ Shrink.State
                      { recording = recording
                      , value = value
                      , history = shrinkHistory
                      , generator = state.generator
                      , userTestFn = state.userTestFn
                      }
                in
                state & setStatus (FailingWith $ FWD 
                   { recording = finalShrinkState.recording
                   , value = finalShrinkState.value
                   , shrinkHistory = finalShrinkState.history
                   }
                )


toResult :: LoopState a -> TestResult a
toResult state =
    case state.status of
        Undecided -> error "Microthesis bug: stuck in Undecided. `generateAndTest` should have prevented this!"
        Passing -> Passes

        FailingWith (FWD{..}) ->
            if state.options.showShrinkHistory then
                FailsWithShrinks $
                    FailsWithShrinksData
                        { finalValue = value
                        , finalRecording = recording
                        , history = List.reverse (map toShrinkStep shrinkHistory)
                        }
            else
                FailsWith value

        UnableToGenerate ->
            CannotGenerateValues $
                CannotGenerateValuesData
                    { mostCommonRejections =
                        state.rejections
                            & Map.toList
                            & List.sortOn snd
                            & List.take 3
                            & List.map fst
                    }
