module Microthesis.Chunk (Chunk(..)) where

data Chunk = Chunk
    { startIndex :: Int
    , size :: Int
    }
    deriving (Show)
