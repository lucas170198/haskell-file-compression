{-# LANGUAGE OverloadedStrings #-}

module Compression.LZ77 where

-- Tripla de valores que substitue cada simbolo (Offset, Index, nextChar)

import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (inits)
import qualified Data.Text as DT
import qualified Data.Text.Internal.Search as T
import Data.Word
import Control.Monad

data TextToken = Empty Int Int Char | Token Int Int deriving (Show)

type TextTokenArr = [TextToken]

--- Para poder ser encodado pela arvore de huffman
instance DB.Binary TextToken where
  put (Empty ofs len c) = do
    DB.put (0 :: Word8)
    DB.put ofs
    DB.put len
    DB.put c
  put (Token ofs len) = do
    DB.put (1 :: Word8)
    DB.put ofs
    DB.put len
  get = do tag <- DB.getWord8
           case tag of
             0 -> liftM3 Empty DB.get DB.get DB.get
             1 -> liftM2 Token DB.get DB.get
             _ -> undefined

-- TODO: Utilizar zippers
data Buffer = Buffer [Char] [Char] deriving (Show)

moveRigth :: Buffer -> Buffer
moveRigth (Buffer dic lf) = Buffer (dic ++ [head lf]) (tail lf)

moveRigthN :: Int -> Buffer -> Buffer
moveRigthN 0 b = b
moveRigthN n b = moveRigthN (n -1) (moveRigth b)

windows :: [a] -> [[a]]
windows [] = []
windows xs = inits xs ++ windows (tail xs)

bestMatch :: Buffer -> String
bestMatch (Buffer dic lf) =
  if (not . null) matches
    then maximum matches
    else ""
  where
    matches = filter (`elem` inits lf) $ filter (not . null) (windows dic)

calculateToken :: Buffer -> TextToken
calculateToken (Buffer dic ls) =
  if null match
    then Empty 0 0 (head ls)
    else Token (length dic - index) matchSize
  where
    match = bestMatch (Buffer dic ls)
    matchSize = length match
    index = head $ T.indices (DT.pack match) (DT.pack dic)

positionsToMove :: TextToken -> Int
positionsToMove Empty {} = 1
positionsToMove (Token _ x) = x

encode' :: Buffer -> [TextToken]
encode' (Buffer _ []) = []
encode' (Buffer dic lf) = actualToken : encode' newBuffer
  where
    buffer = Buffer dic lf
    actualToken = calculateToken buffer
    newBuffer = moveRigthN (positionsToMove actualToken) buffer

encode :: B.ByteString -> [TextToken]
encode b = encode' initalBuffer
  where
    initalBuffer = Buffer "" (unpack b)

--- ref para essa funcao: https://www.joachim-breitner.de/blog/600-On_taking_the_last_n_elements_of_a_list
takeLastN :: Int -> [a] -> [a]
takeLastN n = reverse . take n . reverse 

decodeToken :: String -> TextToken -> String
decodeToken decoded (Empty _ _ a) = decoded <> [a]
decodeToken decoded (Token ofs len) = decoded <> take len (takeLastN ofs decoded)

decode :: [TextToken] -> B.ByteString
decode t = pack (foldl decodeToken "" t)
