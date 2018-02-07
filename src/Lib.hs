{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import Data.Bits (xor)
import Data.List (group, sort)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- hexStr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexBytes :: B.ByteString -> B.ByteString
hexBytes str = fst $ B16.decode str

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 str = B64.encode hexStr
  where hexStr = hexBytes str

bsXor key msg = B.zipWith xor key msg

splitByteStr :: B.ByteString -> [B.ByteString]
splitByteStr = B.groupBy (\x y -> False)

counterCell :: B.ByteString -> (B.ByteString, Int)
counterCell bs = (head bs', length bs')
  where bs' = splitByteStr bs


countCells byteStr = map counterCell cells
  where cells = B.group . B.concat . sort . splitByteStr $ byteStr

maxCell :: Ord b => [(a, b)] -> (a, b) -> (a, b)
maxCell ((a,b):[]) (maxVal, maxCt)
  | b > maxCt = (a,b)
  | otherwise = (maxVal, maxCt)


maxCell ((a,b):cs) (maxVal, maxCt)
  | b > maxCt = maxCell cs (a,b)
  | otherwise = maxCell cs (maxVal, maxCt)

--elementCounter :: B.ByteString -> [(B.ByteString, Int)]
--elementCounter str = countCells . B.group . sort $ str

