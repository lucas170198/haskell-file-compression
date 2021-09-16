module Compression.HuffmanTree where

import Control.Monad
import Data.Binary (Binary, get, getWord8, put)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.HashMap as H
import Data.List (group, intercalate, sort, sortOn)
import Data.Word

data HuffmanTree = Empty | Leaf Char Int | Node Int HuffmanTree HuffmanTree
  deriving (Eq, Show)

instance Ord HuffmanTree where
  Empty `compare` _ = undefined
  (Node val1 _ _) `compare` (Node val2 _ _) = compare val1 val2
  (Leaf _ f1) `compare` (Leaf _ f2) = compare f1 f2
  (Node val1 _ _) `compare` (Leaf _ f) = compare val1 f
  a `compare` b = b `compare` a

freqSum :: HuffmanTree -> HuffmanTree -> Int
Empty `freqSum` _ = undefined
(Leaf _ f1) `freqSum` (Leaf _ f2) = f1 + f2
(Leaf _ f) `freqSum` (Node val _ _) = f + val
(Node val _ _) `freqSum` (Node val2 _ _) = val + val2
a `freqSum` b = freqSum b a

instance Semigroup HuffmanTree where
  Empty <> Empty = Empty
  Empty <> a = a
  a <> Empty = a
  treeA <> treeB = Node (freqSum treeA treeB) left rigth
    where
      left = min treeA treeB
      rigth = max treeA treeB

instance Monoid HuffmanTree where
  mempty = Empty

instance Binary HuffmanTree where
  put Empty = undefined
  put (Leaf c i) = do
    put (0 :: Word8)
    put c
    put i
  put (Node s e1 e2) = do
    put (1 :: Word8)
    put s
    put e1
    put e2

  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 Leaf get get
      1 -> liftM3 Node get get get
      _ -> undefined

type FrequencyArr = [(Char, Int)]

calculateCharsFrequency :: B.ByteString -> FrequencyArr
calculateCharsFrequency bytes = countFreq $ unpack bytes
  where
    countFreq :: [Char] -> [(Char, Int)]
    countFreq chars =
      let groupedChars = (group . sort) chars
          freqTuples = map (\s -> (head s, length s)) groupedChars
       in sortOn snd freqTuples


buildHuffmanTree :: FrequencyArr -> HuffmanTree
buildHuffmanTree freq = foldl (<>) Empty leafArr
  where
    leafArr = map (uncurry Leaf) freq

type CodeTable = H.Map Char String

buildCodeTable :: HuffmanTree -> String -> CodeTable
buildCodeTable Empty _ = H.empty
buildCodeTable (Leaf chr _) path = H.singleton chr path
buildCodeTable (Node _ left right) path =
  buildCodeTable left (path ++ "0") <> buildCodeTable right (path ++ "1")

encode :: B.ByteString -> (B.ByteString, HuffmanTree)
encode text = (output, tree)
  where
    freqArr = calculateCharsFrequency text
    tree = buildHuffmanTree freqArr
    table = buildCodeTable tree "" 
    output = pack $ intercalate "" . map (table H.!) $ unpack text

decode :: B.ByteString -> HuffmanTree -> B.ByteString
decode encoded tree = pack $ decode' (unpack encoded) tree
  where
    decode' "" (Leaf chr _) = [chr]
    decode' "" _ = ""
    decode' code Empty = code
    decode' (x : xs) (Node _ esq dir) =
      if x == '0'
        then decode' xs esq
        else decode' xs dir
    decode' code (Leaf chr _) = chr : decode' code tree