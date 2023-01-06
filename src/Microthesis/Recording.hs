module Microthesis.Recording 
    ( Recording, Zipper
    -- Creation
    , empty, fromZipper
    -- Query
    , isEmpty, isFull, length, getAt, isInBounds
    -- Transformation
    , setAt, replace, update, swapIfOutOfOrder, append
    -- Transformation on Chunks
    , deleteChunk, sortChunk, replaceChunkWithZero
    -- Conversion
    , toZipper
    ) 
    where

import Data.Function ((&))
import Microthesis.Chunk (Chunk(..))
import Prelude hiding (length)
import qualified Data.List as List
import qualified Data.List.Zipper as ListZipper
import qualified Prelude (length)

newtype Recording = Recording [Int]
    deriving (Eq, Ord, Show)

type Zipper = ListZipper.Zipper Int

maxLength :: Int
maxLength =
    64 * 1024

-- CREATION

empty :: Recording
empty = Recording []

fromZipper :: Zipper -> Recording 
fromZipper zipper = Recording $ ListZipper.toList zipper

-- QUERY

isEmpty :: Recording -> Bool
isEmpty (Recording list) = null list

isFull :: Recording -> Bool
isFull (Recording list) = Prelude.length list == maxLength

length :: Recording -> Int
length (Recording list) = Prelude.length list

getAt :: Int -> Recording -> Maybe Int
getAt index (Recording list) = nth index list
  where
    nth :: Int -> [a] -> Maybe a
    nth _ []       = Nothing
    nth 0 (x : _)  = Just x
    nth n (_ : xs) = nth (n - 1) xs

isInBounds :: Chunk -> Recording -> Bool
isInBounds (Chunk{..}) (Recording list) =
  -- TODO PERF maybe we can hold the length next to the list
  startIndex + size <= Prelude.length list

-- TRANSFORMATION

setAt :: Int -> Int -> Recording -> Recording
setAt index value (Recording list) =
  Recording newList
  where
    newList = go index value list
    go i a ls
      | i < 0 = ls
      | otherwise = go' i ls
      where
        go' 0 (_:xs) = a : xs
        go' n (x:xs) = x : go' (n-1) xs
        go' _ []     = []

replace :: [(Int,Int)] -> Recording -> Recording
replace places (Recording list) =
  Recording newList
  where
    newList = replaceList places list 
    replaceList [] r = r
    replaceList ((index,value):rest) r =
      replaceList rest (modifyAt index (\_ -> value) r)

update :: Int -> (Int -> Int) -> Recording -> Recording
update index fn (Recording list) = 
  Recording (modifyAt index fn list)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = f x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

swapIfOutOfOrder :: (Int,Int) -> Recording -> Maybe (Recording, Int, Int)
swapIfOutOfOrder (leftIndex, rightIndex) recording = do
    left  <- getAt leftIndex recording
    right <- getAt rightIndex recording
    pure $ 
      if left > right then
        (replace
            [ (leftIndex, right)
            , (rightIndex, left)
            ]
            recording
        , right
        , left
        )
      else
        (recording, left, right)

append :: Int -> Recording -> Recording
append word (Recording list) =
  -- TODO perf: some Queue structure other than list might be better for this
  Recording (list ++ [word])

-- TRANSFORMATION ON CHUNKS

deleteChunk :: Chunk -> Recording -> Recording
deleteChunk chunk (Recording list) = 
  Recording $ 
    List.take chunk.startIndex list
    ++ List.drop (chunk.startIndex + chunk.size) list


sortChunk :: Chunk -> Recording -> Recording
sortChunk = 
  mapChunk List.sort


replaceChunkWithZero :: Chunk -> Recording -> Recording
replaceChunkWithZero =
  mapChunk (map (\_ -> 0))

mapChunk :: ([Int] -> [Int]) -> Chunk -> Recording -> Recording
mapChunk f chunk (Recording list) =
  Recording newList
  where
    newList = before ++ f middle ++ after
    before = List.take chunk.startIndex list
    middle = 
      list
        & List.drop chunk.startIndex
        & List.take chunk.size
    after = List.drop (chunk.startIndex + chunk.size) list

-- CONVERSION

toZipper :: Recording -> Zipper
toZipper (Recording list) = ListZipper.fromList list
