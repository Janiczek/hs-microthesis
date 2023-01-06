module Microthesis.Shrink.Cmd
    ( Cmd(..), Type(..), MWBS(..), RS(..)
    , label
    , cmdsForRecording
    )
    where

import Data.Function ((&))
import Microthesis.Chunk (Chunk(..))
import Microthesis.Recording (Recording)
import qualified Microthesis.Recording as Recording

data Cmd = Cmd
  { type_ :: Type
  , minLength :: Int
  }

instance Show Cmd where
  show cmd = show cmd.type_

data Type
    = DeleteChunkAndMaybeDecrementPrevious Chunk
    | ReplaceChunkWithZero Chunk
    | SortChunk Chunk
    | MinimizeWithBinarySearch MWBS
    | RedistributeSum RS

instance Show Type where
  show = \case 
    DeleteChunkAndMaybeDecrementPrevious c ->
      "Delete chunk and maybe decrement previous" & withChunk c 
    ReplaceChunkWithZero c ->
      "Replace chunk with zero" & withChunk c 
    SortChunk c ->
      "Sort chunk" & withChunk c 
    MinimizeWithBinarySearch mwbs ->
      "Minimize with binary search" & withIndex mwbs.index
    RedistributeSum rs ->
      "Redistribute sum" & withIndexes rs.leftIndex rs.rightIndex

withChunk :: Chunk -> String -> String
withChunk c str =
  str ++ " (i=" ++ show c.startIndex ++ ".." ++ show (c.startIndex + c.size - 1) ++ ")"

withIndex :: Int -> String -> String
withIndex i str =
  str ++ " (i=" ++ show i ++ ")"

withIndexes :: Int -> Int -> String -> String
withIndexes i1 i2 str =
  str ++ " (i=" ++ show i1 ++ "," ++ show i2 ++ ")"

data MWBS = MWBS 
    { index :: Int }
    deriving (Show)

data RS = RS
    { leftIndex :: Int
    , rightIndex :: Int 
    }
    deriving (Show)

label :: Cmd -> String
label (Cmd{type_}) =
  case type_ of
    DeleteChunkAndMaybeDecrementPrevious c  -> "DeleteChunkAndMaybeDecrementPrevious(" ++ chunk c ++ ")"
    ReplaceChunkWithZero c                  -> "ReplaceChunkWithZero(" ++ chunk c ++ ")"
    SortChunk c                             -> "SortChunk(" ++ chunk c ++ ")"
    MinimizeWithBinarySearch r              -> "MinimizeWithBinarySearch(" ++ show r.index ++ ")"
    RedistributeSum r                       -> "RedistributeSum(" ++ show r.leftIndex ++ ", " ++ show r.rightIndex ++ ")"
    where
        chunk :: Chunk -> String
        chunk c = show c.startIndex ++ ".." ++ show (c.startIndex + c.size - 1)

{-| 
TODO: what ordering of these would be the best?
TODO: we could fix the seed and input, and vary the orderings, and report and do some statistics
-}
cmdsForRecording :: Recording -> [Cmd]
cmdsForRecording recording = 
    concat
        [ chunkCmds DeleteChunkAndMaybeDecrementPrevious len AllowSize1Chunks
        , chunkCmds ReplaceChunkWithZero len DisallowSize1Chunks -- already happens in binary searches
        , chunkCmds SortChunk len DisallowSize1Chunks -- doesn't make sense for sorting
        , minimizeCmds
        , redistributeCmds 2
        , redistributeCmds 1
        ]
    where
        len = Recording.length recording
        minimizeCmds =
            [len-1,len-2..0] 
                & map (\index -> Cmd (MinimizeWithBinarySearch MWBS{..}) (index + 1) )
        redistributeCmds offset = 
            [len-1-offset,len-2-offset..0]
                & map (\leftIndex -> 
                        let rightIndex = leftIndex + offset in
                        Cmd (RedistributeSum RS{..}) (rightIndex + 1)
                      ) 

data Size1Chunks
    = AllowSize1Chunks
    | DisallowSize1Chunks

chunkCmds :: (Chunk -> Type) -> Int -> Size1Chunks -> [Cmd]
chunkCmds toType recordingLen size1 =
    go initSize 0 []
    where
        initSize = case size1 of
            AllowSize1Chunks -> 1
            DisallowSize1Chunks -> 2

        go size startIndex acc =
            if startIndex > recordingLen - size then
                if size == 8 then
                    acc
                else
                    go (size * 2) 0 acc
            else
                go 
                  size 
                  (startIndex + 1) 
                  (Cmd (toType Chunk{..}) (startIndex + size) : acc)
