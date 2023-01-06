module Microthesis.GenPhase
    ( GenPhase(..), generation, replay
    , getSystemGen, getRecording
    )
    where

import Microthesis.Recording (Recording)
import Microthesis.SystemGen (SystemGen)
import qualified Microthesis.Recording as Recording

data GenPhase
    = Generation SystemGen Recording
    | Replay Recording.Zipper

generation :: SystemGen -> GenPhase
generation systemGen = Generation systemGen Recording.empty

replay :: Recording -> GenPhase
replay recording = Replay $ Recording.toZipper recording

getSystemGen :: GenPhase -> Maybe SystemGen
getSystemGen (Generation systemGen _) = Just systemGen
getSystemGen (Replay _) = Nothing

getRecording :: GenPhase -> Recording
getRecording (Generation _ recording) = recording
getRecording (Replay zipper) = Recording.fromZipper zipper
