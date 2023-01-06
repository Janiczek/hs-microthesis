module Microthesis.Shrink
    ( State(..)
    , ShrinkResult
    , Step(..)
    , shrink
    )
    where

import Data.Function ((&))
import Microthesis.Chunk (Chunk(..))
import Microthesis.Gen (Gen)
import Microthesis.Recording (Recording)
import Microthesis.Shrink.Cmd (Cmd(..), Type(..))
import qualified Microthesis.Recording as Recording
import qualified Microthesis.Shrink.Cmd as Cmd
import qualified Microthesis.Gen as Gen
import qualified Microthesis.GenPhase as GenPhase
import qualified Data.List as List

data State a = State
    { recording :: Recording
    , generator :: Gen a
    , value :: a
    , userTestFn :: a -> Bool
    , history :: [Step a]
    }


data ShrinkResult a = SR
    { wasImprovement :: Bool
    , newState :: State a
    }

data Step a = Step
    { value :: a
    , recording :: Recording
    , shrinkerUsed :: Maybe Cmd
    }
    deriving Show

shrink :: State a -> State a
shrink state = 
    if Recording.isEmpty state.recording then
        -- It's already [], we can't do any better!
        state
    else
      shrinkWhileProgress state

shrinkWhileProgress :: State a -> State a
shrinkWhileProgress state =
  let nextState = shrinkOnce state in
  if state.recording == nextState.recording
      then nextState
      else shrinkWhileProgress nextState

shrinkOnce :: State a -> State a
shrinkOnce state =
  runCmds (Cmd.cmdsForRecording state.recording) state

runCmds :: [Cmd] -> State a -> State a
runCmds [] state = state
runCmds (cmd:rest) state =
  let result = runCmd cmd state
      newLength = Recording.length result.newState.recording
      newRest =
        if result.wasImprovement && newLength < Recording.length state.recording
           -- Throw away Cmds that won't work because they need more data than we have
           then rest & List.filter (\c -> newLength >= c.minLength)
           else rest
      newState =
        if result.wasImprovement
           then let step = Step { value = result.newState.value
                                , recording = result.newState.recording
                                , shrinkerUsed = Just cmd
                                }
                in result.newState { history = step : result.newState.history }
           else result.newState
  in
  runCmds newRest newState


-- Shrink Cmd implementations
-- TODO move them into their own module somehow?
-- TODO rewrite these into some kind of state monad? They could be so much more succint!

runCmd :: Cmd -> State a -> ShrinkResult a
runCmd (Cmd{..}) =
  case type_ of
    ReplaceChunkWithZero chunk -> replaceChunkWithZero chunk
    SortChunk chunk            -> sortChunk chunk
    MinimizeWithBinarySearch r -> minimizeWithBinarySearch r
    RedistributeSum r          -> redistributeSum r
    DeleteChunkAndMaybeDecrementPrevious chunk ->
      deleteChunkAndMaybeDecrementPrevious chunk

-----------------------
-- SHRINK STRATEGIES --
-----------------------

deleteChunkAndMaybeDecrementPrevious :: Chunk -> State a -> ShrinkResult a
deleteChunkAndMaybeDecrementPrevious chunk = withChunk chunk $ do
    oneOf
      [ deleteAndDecrement
      , deleteOnly
      ]
    where
        deleteAndDecrement =
           Recording.update (chunk.startIndex - 1) (subtract 1)
           . Recording.deleteChunk chunk

        deleteOnly =
            Recording.deleteChunk chunk

replaceChunkWithZero :: Chunk -> State a -> ShrinkResult a
replaceChunkWithZero chunk = withChunk chunk $
    runChange $ Recording.replaceChunkWithZero chunk


sortChunk :: Chunk -> State a -> ShrinkResult a
sortChunk chunk = withChunk chunk $
    runChange $ Recording.sortChunk chunk

minimizeWithBinarySearch :: Cmd.MWBS -> State a -> ShrinkResult a
minimizeWithBinarySearch r = withChunk implicitChunk $ \state ->
    case Recording.getAt r.index state.recording of
      Nothing -> noShrink state
      Just value ->
        binarySearchShrink
          (BSS
              { low = 0
              , high = value
              , fn = \val -> Recording.setAt r.index val
              }
          )
          state
    where
        implicitChunk = Chunk{startIndex = r.index, size = 1}

redistributeSum :: Cmd.RS -> State a -> ShrinkResult a
redistributeSum r = withChunk implicitChunk $ \state ->
    case Recording.swapIfOutOfOrder (r.leftIndex, r.rightIndex) state.recording of
      Nothing -> noShrink state -- This only happens when the indexes are out of bounds
      Just (newRecording, newLeft, newRight) ->
        let swappedAttempt = runRecording newRecording state in
        binarySearchShrink 
          (BSS
            { low = 0
            , high = newLeft
            , fn = \val ->
                Recording.replace
                    [ (r.leftIndex, val) -- sum stays constant:
                    , (r.rightIndex, newRight + newLeft - val)
                    ]
            }
          )
          swappedAttempt.newState
    where
      implicitChunk = Chunk
        { startIndex = r.leftIndex
        , size = r.rightIndex - r.leftIndex + 1
        }

-- HELPERS

isInBounds :: Chunk -> Recording -> Bool
isInBounds chunk recording =
    Recording.isInBounds chunk recording


noShrink :: State a -> ShrinkResult a
noShrink state =
  SR 
    { wasImprovement = False
    , newState = state
    }

oneOf :: [Recording -> Recording] -> State a -> ShrinkResult a
oneOf [] state = noShrink state
oneOf (change:changes) state =
  let new = runChange change state in
  if new.wasImprovement then new
  else oneOf changes new.newState

runChange :: (Recording -> Recording) -> State a -> ShrinkResult a
runChange f state =
  runRecording (f state.recording) state

runRecording :: Recording -> State a -> ShrinkResult a
runRecording recording state =
  case compare state.recording recording of
    LT -> noShrink state
    EQ -> noShrink state
    GT -> 
      let (result, _) = Gen.run (GenPhase.replay recording) state.generator in
      case result of
        Left _ -> noShrink state
        Right value ->
          if state.userTestFn value then
            noShrink state
          else
            SR
              { wasImprovement = True
              , newState =
                  state
                    & setRecording recording
                    & setValue value
              }

setValue :: a -> State a -> State a
setValue v state = State
  { recording = state.recording
  , generator = state.generator
  , value = v
  , userTestFn = state.userTestFn
  , history = state.history
  }

setRecording :: Recording -> State a -> State a
setRecording r state = State
  { recording = r
  , generator = state.generator
  , value = state.value
  , userTestFn = state.userTestFn
  , history = state.history
  }
  

withChunk :: Chunk -> (State a -> ShrinkResult a) -> (State a -> ShrinkResult a)
withChunk chunk f state = 
  if isInBounds chunk state.recording then
    f state
  else 
    noShrink state

data BSS = BSS
    { low :: Int
    , high :: Int
    , fn :: Int -> Recording -> Recording
    }

instance Show BSS where
  show bss = show (bss.low, bss.high)

binarySearchShrink :: BSS -> State a -> ShrinkResult a
binarySearchShrink (r@BSS{..}) state =
  let bestCaseAttempt = runChange (fn low) state in
  if bestCaseAttempt.wasImprovement then 
    bestCaseAttempt
  else
    binarySearchLoop False r state

binarySearchLoop :: Bool -> BSS -> State a -> ShrinkResult a
binarySearchLoop wasImprovement (r@BSS{..}) state =
  if low + 1 < high then
        let mid = low + ((high - low) `div` 2)
            midAttempt = runChange (fn mid) state
            newR = if midAttempt.wasImprovement then r { high = mid }
                   else                              r { low = mid }
        in
        binarySearchLoop midAttempt.wasImprovement newR midAttempt.newState
  else
    SR 
      { wasImprovement = wasImprovement
      , newState = state
      }
