module Compression.SimplifiedDeflate where

import qualified Compression.HuffmanTree as HTree
import qualified Compression.LZ77 as LZ77
import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as B

encode :: B.ByteString -> (B.ByteString, B.ByteString)
encode input = (info, tree)
  where
    lzEncoded = LZ77.encode input
    hTreeTuple = HTree.encode $ DB.encode lzEncoded
    info = fst hTreeTuple
    tree = DB.encode (snd hTreeTuple)

decode :: B.ByteString -> B.ByteString -> B.ByteString
decode encodedInfo encodedTree = LZ77.decode lzInfo
  where
    decodedTree = DB.decode encodedTree :: HTree.HuffmanTree
    lzEncoded = HTree.decode encodedInfo decodedTree
    lzInfo = DB.decode lzEncoded :: [LZ77.TextToken]