module Main where

import Compression.SimplifiedDeflate (decode, encode)
import Data.Bits (Bits (bitSize, shiftL, testBit, (.|.)))
import Data.Bits.Bitwise (fromBool)
import qualified Data.Binary as DB
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as DT
import Data.List.Split
import Data.Word (Word8)
import System.Environment (getArgs)

-- Me inspirei no cli que o emilio fez para o minips nesse usage ;P
usage :: IO ()
usage =
  putStrLn . unlines $
    [ "Use: stack run {zip|unzip} arquivo",
      "",
      "",
      "Exemplos:",
      "---------",
      "stack run zip teste.txt",
      "minips run unzip teste.hzip",
      "",
      "",
      "Opções:",
      "-------",
      "zip - Comprime o arquivo, gerando *.hzip, *hzip.meta",
      "unzip - Descomprime arquivo (deve conter *.hzip na extensão do arquivo"
    ]

-- | Ref : Stackoverflow https://stackoverflow.com/questions/28522395/converting-an-int-to-a-list-of-bits-represented-by-bool

-- | Convert a little-endian list of bits to 'Bits'.
fromListLE :: (Num b, Bits b) => [Bool] -> b
fromListLE = foldr f 0
  where
    f b i = fromBool b .|. (i `shiftL` 1)

-- | Convert a 'Bits' (with a defined 'bitSize') to a list of bits, in
toListLE :: (Num b, Bits b) => b -> [Bool]
toListLE b = map (testBit b) [0 .. bitSize b - 1]

-------------------------------------------------------------------------------------------------------------------------

byteStringToWord8Arr :: B.ByteString -> [Word8]
byteStringToWord8Arr bs = map fromListLE $ chunksOf 8 $ map wordBitToBool $ B.unpack bs
  where
    wordBitToBool b = case b of
      49 -> True -- String "1"
      48 -> False -- String "0"
      _ -> undefined

flat :: [[a]] -> [a]
flat xs = [y | x <- xs, y <- x]

decompress :: B.ByteString -> B.ByteString
decompress b = B.pack $ map (\x -> if x then 49::Word8 else 48::Word8) $ flat $ map toListLE $ (DB.decode b :: [Word8])


run :: String -> FilePath -> IO ()
run opt path = do
  if opt == "zip"
    then do
      content <- B.readFile path
      let encodedTuple = encode content
      B.writeFile (fileName <> ".hzip") (DB.encode $ byteStringToWord8Arr $ fst encodedTuple)
      B.writeFile (fileName <> ".hzip.meta") (snd encodedTuple)
      putStrLn "Arquivo compactado com sucesso!"
      
    else
      if opt == "unzip"
        then do
          putStrLn ""
          encoded <- B.readFile (fileName <> ".hzip")
          meta <- B.readFile (fileName <> ".hzip.meta")
          let originalContent = decode (decompress encoded) meta
          putStrLn "Conteúdo original:"
          B.putStr originalContent
        else usage
  where
    fileName = DT.unpack $ fst $ DT.breakOn (DT.pack ".") (DT.pack path)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [opt, path] -> run opt path
    _ -> usage
