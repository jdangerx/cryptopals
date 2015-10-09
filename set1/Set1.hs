module Main where

import qualified Data.Bits as B
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Numeric (readHex, showHex)

-- Challenge 1 - hex to b64
hexTo6BitChunks :: String -> [Int]
hexTo6BitChunks (a:b:c:xs) =
  (++ hexTo6BitChunks xs) . (\i -> [i `div` 64, i `mod` 64]) . fst . head . readHex $ [a,b,c]
hexTo6BitChunks _ = []

intToB64 :: Int -> Char
intToB64 i
  | i < 26 =  chr (i + 65)
  | i < 52 =  chr (i + 71)
  | i < 62 =  chr (i - 4)
  | i == 62 = '+'
  | i == 63 = '/'
  | otherwise = intToB64 (i `mod` 64)

hexToB64 :: String -> String
hexToB64 = (intToB64 <$>) . hexTo6BitChunks

testHexToB64 :: Bool
testHexToB64 =
  hexToB64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

-- Challenge 2: fixed length XOR
hexToInt :: Char -> Int
-- hexToInt = fst . head . readHex . (:[])
hexToInt '0' = 0
hexToInt '1' = 1
hexToInt '2' = 2
hexToInt '3' = 3
hexToInt '4' = 4
hexToInt '5' = 5
hexToInt '6' = 6
hexToInt '7' = 7
hexToInt '8' = 8
hexToInt '9' = 9
hexToInt 'a' = 10
hexToInt 'b' = 11
hexToInt 'c' = 12
hexToInt 'd' = 13
hexToInt 'e' = 14
hexToInt 'f' = 15

intToHex :: Int -> Char
-- intToHex = fst . head . readHex . (:[])
intToHex 0 = '0'
intToHex 1 = '1'
intToHex 2 = '2'
intToHex 3 = '3'
intToHex 4 = '4'
intToHex 5 = '5'
intToHex 6 = '6'
intToHex 7 = '7'
intToHex 8 = '8'
intToHex 9 = '9'
intToHex 10 = 'a'
intToHex 11 = 'b'
intToHex 12 = 'c'
intToHex 13 = 'd'
intToHex 14 = 'e'
intToHex 15 = 'f'


fixedXor :: String -> String -> String
fixedXor a b =
  let
    aInts = map hexToInt a
    bInts = map hexToInt b
    xorInts = zipWith B.xor aInts bInts
  in
    map intToHex xorInts

xorA = "1c0111001f010100061a024b53535009181c"
xorB = "686974207468652062756c6c277320657965"

testFixedXor :: Bool
testFixedXor =
  fixedXor xorA xorB == "746865206b696420646f6e277420706c6179"

-- Challenge 3: 1-byte XOR cipher
byteXor :: String -> Int -> String
byteXor s i =
  let leftHex = showHex (i `mod` 16) ""
      rightHex = showHex (i `div` 16) ""
  in
   fixedXor s $ concat . replicate (length s `div` 2) $ (leftHex ++ rightHex)

letters :: M.Map Char Int
letters = M.fromList (zip "etaoinsrhdlucmfywgpbvkxqjz" (reverse [0..25]))

score :: String -> Int
score s = sum . mapMaybe (`M.lookup` letters) $ s

hexToChars :: String -> String
hexToChars (a:b:bs) =
  let int = hexToInt a * 16 + hexToInt b
  in chr int : hexToChars bs
hexToChars _ = []

sbXorCipher :: String -> [(String, Char)]
sbXorCipher s =
  take 1 . reverse . sortOn (score . fst) $
  (\(hex, c) -> (hexToChars hex, c)) . (\i -> (byteXor s i, chr i)) <$> [0..255]

sbXorCipher_ :: String -> String
sbXorCipher_ s =
  last . sortOn score $ hexToChars . byteXor s <$> [0..255]
-- drops the key info.

testSbXorCipher :: Bool
testSbXorCipher =
  (fst . head . sbXorCipher $ "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
  == "Cooking MC's like a pound of bacon"

-- Challenge 4: detect 1-byte XOR cipher
detectXor :: [String] -> String
-- detectXor ss = fst . last . sortOn (score . fst) . concatMap sbXorCipher $ ss
detectXor ss = last . sortOn score . map sbXorCipher_ $ ss

testDetectXor :: IO Bool
testDetectXor =
  do
    inp <- readFile "4.txt"
    let result = detectXor . lines $ inp
    return $ result == "Now that the party is jumping\n"

main = testDetectXor >>= print
