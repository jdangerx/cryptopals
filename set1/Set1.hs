{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (chr, ord, intToDigit, digitToInt)
import Data.Function (on)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Word (Word8)

import Numeric (readHex, showHex)

type Bytes = BSC.ByteString
type Hexes = String
type Base64s = String

-- Challenge 1 - hex to b64
fromHexes :: Hexes -> Bytes
fromHexes hexes@(a:b:rest)
  | length hexes `mod` 2 /= 0 = fromHexes ('0' : hexes)
  | otherwise =
    let aInteger = fromIntegral $ digitToInt a
        bInteger = fromIntegral $ digitToInt b
    in
     ((aInteger * 16 + bInteger) :: Word8) `BS.cons` fromHexes rest
fromHexes [] = ""

toB64 :: Bytes -> Base64s
toB64 bytes =
  case BS.length bytes of
   0 -> ""
   1 -> (take 2 . toB64 $ BS.append bytes "00") ++ "=="
   2 -> (take 3 . toB64 $ BS.append bytes "0") ++ "="
   _ -> threeBytesToFourChunks (BS.take 3 bytes) ++ toB64 (BS.drop 3 bytes)

threeBytesToFourChunks :: Bytes -> Base64s
threeBytesToFourChunks =
  (\i -> intToB64 <$> [ i `div` 64^3 `mod` 64
                     , i `div` 64^2 `mod` 64
                     , i `div` 64 `mod` 64
                     , i `mod` 64]) . threeBytesToInt

threeBytesToInt :: Bytes -> Int
threeBytesToInt = snd . BS.foldr (\c (i, s) -> (i + 1, fromIntegral c * 256^i + s)) (0, 0)

intToB64 :: Int -> Char
intToB64 i
  | i < 26 =  chr (i + 65)
  | i < 52 =  chr (i + 71)
  | i < 62 =  chr (i - 4)
  | i == 62 = '+'
  | i == 63 = '/'
  | otherwise = intToB64 (i `mod` 64)

hexToB64 :: String -> String
hexToB64 = toB64 . fromHexes

testHexToB64 :: Bool
testHexToB64 =
  hexToB64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
  == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

-- Challenge 2: fixed length XOR

toHexes :: Bytes -> Hexes
toHexes =
  BS.foldr
  (\byte hexes ->
    (++ hexes) $ intToDigit <$> [fromIntegral byte `div` 16
                                , fromIntegral byte `mod` 16])
  ""

bytesXor :: Bytes -> Bytes -> Bytes
bytesXor b b' =
  let numReps = BS.length b `div` BS.length b' + 1
  in BS.pack . BS.zipWith B.xor b $ BS.concat (replicate numReps b')

fixedXor :: Hexes -> Hexes -> Hexes
fixedXor h =
  toHexes . (bytesXor `on` fromHexes) h

xorA :: Hexes
xorA = "1c0111001f010100061a024b53535009181c"

xorB :: Hexes
xorB = "686974207468652062756c6c277320657965"

testFixedXor :: Bool
testFixedXor =
  fixedXor xorA xorB == "746865206b696420646f6e277420706c6179"

-- Challenge 3: 1-byte XOR cipher
byteXor :: Bytes -> Word8 -> Bytes
byteXor bytes word8 =
  bytesXor bytes (BS.pack [word8])

letters :: M.Map Word8 Int
letters = M.fromList . zip (BS.unpack "etaoinsrhdlucmfywgpbvkxqjz") $ [25, 24..0]

-- all non-letter word8's get 0 value
score :: Bytes -> Int
score = sum . mapMaybe (`M.lookup` letters) . BS.unpack

sbXorCipher :: Hexes -> Maybe Bytes
sbXorCipher s =
  listToMaybe . reverse . sortOn score $ byteXor (fromHexes s) <$> [0..255]

testSbXorCipher :: Bool
testSbXorCipher =
  sbXorCipher "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
  == Just "Cooking MC's like a pound of bacon"

-- Challenge 4: detect 1-byte XOR cipher
detectXor :: [Hexes] -> Bytes
detectXor = last . sortOn score . mapMaybe sbXorCipher

testDetectXor :: IO Bool
testDetectXor =
  do
    inp <- readFile "4.txt"
    let result = detectXor . lines $ inp
    return $ result == "Now that the party is jumping\n"

-- Challenge 5: repeating XOR cipher

iceLines :: Bytes
iceLines = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal" 

testBytesXor :: Bool
testBytesXor =
  (toHexes . bytesXor iceLines $ "ICE") ==
  "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

-- Challenge 6: Break repeating key XOR

stringEditDistance :: Bytes -> Bytes -> Int
stringEditDistance bs bs' =
  sum (BS.zipWith (\w w' -> B.popCount (w `B.xor` w')) bs bs')

testStringEditDistance :: Bool
testStringEditDistance = stringEditDistance "this is a test" "wokka wokka!!!" == 37

getKeySize :: (Int, Int) -> Bytes -> [(Int, Double)] -- input should be ciphertext in bytes, not hex digits or b64.
getKeySize (low, high) bs =
  let
    editDists = (`tryKeySize` bs) <$> [low..high]
  in sortOn snd $ editDists

tryKeySize :: Int -> BS.ByteString -> (Int, Double)
tryKeySize n bs =
  let fstBlock = BS.take n bs
      sndBlock = BS.take n . BS.drop n $ bs
      thirdBlock = BS.take n . BS.drop n . BS.drop n $ bs
      fourthBlock = BS.take n . BS.drop n . BS.drop n . BS.drop n $ bs
      dist12 = fromIntegral $ stringEditDistance fstBlock sndBlock
      dist23 = fromIntegral $ stringEditDistance sndBlock thirdBlock
      dist34 = fromIntegral $ stringEditDistance thirdBlock fourthBlock
      dist = (dist12 + dist23 + dist34) / 3.0
  in
   (n, dist / fromInteger (fromIntegral n))

testGetKeySize :: [(Int, Double)]
testGetKeySize =
  getKeySize (2, 40) (bytesXor iceIceBabyFull "abc") -- == 3

iceIceBabyFull :: Bytes
iceIceBabyFull = "Yo, VIP, let's kick it!\n\nIce ice baby\nIce ice baby\nAll right stop\nCollaborate and listen\nIce is back with my brand new invention\nSomething grabs a hold of me tightly\nThen I flow that a harpoon daily and nightly\nWill it ever stop?\nYo, I don't know\nTurn off the lights and I'll glow\nTo the extreme I rock a mic like a vandal\nLight up a stage and wax a chump like a candle\n\nDance\nBum rush the speaker that booms\nI'm killin' your brain like a poisonous mushroom\nDeadly, when I play a dope melody\nAnything less that the best is a felony\nLove it or leave it\nYou better gain way\nYou better hit bull's eye\nThe kid don't play\nIf there was a problem\nYo, I'll solve it\nCheck out the hook while my DJ revolves it\n\nIce ice baby Vanilla\nIce ice baby Vanilla\nIce ice baby Vanilla\nIce ice baby Vanilla\n\nNow that the party is jumping\nWith the bass kicked in, the fingers are pumpin'\nQuick to the point, to the point no faking\nI'm cooking MC's like a pound of bacon\nBurning them if they're not quick and nimble\nI go crazy when I hear a cymbal\nAnd a hi hat with a souped up tempo\nI'm on a roll and it's time to go solo\nRollin in my 5.0\nWith my ragtop down so my hair can blow\nThe girlies on standby\nWaving just to say hi\nDid you stop?\nNo, I just drove by\nKept on pursuing to the next stop\nI busted a left and I'm heading to the next block\nThat block was dead\n\nYo so I continued to a1a Beachfront Ave\nGirls were hot wearing less than bikinis\nRock man lovers driving Lamborghini\nJealous 'cause I'm out getting mine\nShay with a gauge and Vanilla with a nine\nReady for the chumps on the wall\nThe chumps are acting ill because they're so full of eight balls\nGunshots ranged out like a bell\nI grabbed my nine\nAll I heard were shells\nFallin' on the concrete real fast\nJumped in my car, slammed on the gas\nBumper to bumper the avenue's packed\nI'm tryin' to get away before the jackers jack\nPolice on the scene\nYou know what I mean\nThey passed me up, confronted all the dope fiends\nIf there was a problem\nYo, I'll solve it\nCheck out the hook while my DJ revolves it\n\nIce ice baby Vanilla\nIce ice baby Vanilla\nIce ice baby Vanilla\nIce ice baby Vanilla\n\nTake heed, 'cause I'm a lyrical poet\nMiami's on the scene just in case you didn't know it\nMy town, that created all the bass sound\nEnough to shake and kick holes in the ground\n'Cause my style's like a chemical spill\nFeasible rhymes that you can vision and feel\nConducted and formed\nThis is a hell of a concept\nWe make it hype and you want to step with this\nShay plays on the fade, slice it like a ninja\nCut like a razor blade so fast\nOther DJ's say, 'damn'\nIf my rhyme was a drug\nI'd sell it by the gram\nKeep my composure when it's time to get loose\nMagnetized by the mic while I kick my juice\nIf there was a problem\nYo, I'll solve it!\nCheck out the hook while my DJ revolves it\n\nIce ice baby Vanilla\nIce ice baby Vanilla\nIce ice baby Vanilla\nIce ice baby Vanilla\n\nYo man, let's get out of here\nWord to your mother\n\nIce ice baby\nToo cold\nIce ice baby\nToo cold too cold\nIce ice baby\nToo cold too cold\nIce ice baby\nToo cold too cold\n"

main :: IO ()
main = testDetectXor >>= print
