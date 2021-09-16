{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
import Test.QuickCheck
import qualified Compression.HuffmanTree as HTree
import qualified Compression.LZ77 as LZ77
import Data.ByteString.Lazy as B
import Data.Word

prop_encode_decode_inverse_functions_for_huffman_tree :: [Word8] -> Bool
prop_encode_decode_inverse_functions_for_huffman_tree input = HTree.decode (fst encodedInput) (snd encodedInput) == (B.pack input)
    where encodedInput = HTree.encode (B.pack input)

prop_encode_decode_inverse_functions_for_lz77 :: [Word8] -> Bool
prop_encode_decode_inverse_functions_for_lz77 input = (LZ77.decode . LZ77.encode) encodedInput == encodedInput
    where encodedInput = (B.pack input)

return []
main :: IO ()
main = do True <- $quickCheckAll; return ()
