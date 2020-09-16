module Lib where

import qualified Data.Array            as A
import           Data.Bits             (shift, (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import           Data.Char             (chr, ord)
import           Data.Int
import           Data.Word

--Room represents a single coordinate in a maze and the walls it contains
data Room = Room
    { coord :: (Int, Int)
    }


--Maze represents a series of coordinates:
data Maze = Maze
    { height :: Int
    , width  :: Int
    , walls  :: [Int]
    }

printHeight :: Maze -> Int
printHeight maze = height maze

printWidth :: Maze -> Int
printWidth maze = width maze

decodeMaze :: B.ByteString -> Maze
decodeMaze bstr = Maze (readSize(B.take 4 bstr)) (readSize(B.drop 4 (B.take 8 bstr))) (getByteIntList(B.drop 8 bstr))

readSize :: C.ByteString -> Int
readSize bstr = byte 0
             .|. (byte 1 `shift` 8)
             .|. (byte 2 `shift` 16)
             .|. (byte 3 `shift` 24)
        where byte n = fromIntegral $ ord (bstr `C.index` n)

byteToInt :: C.ByteString -> Int -> Int
byteToInt bstr n = fromIntegral $ ord (bstr `C.index` n)

word8ToInt :: Word8 -> Int
word8ToInt word = fromIntegral word

fstBit :: Int -> Int
fstBit byte = byte `div` 128

sndBit :: Int -> Int
sndBit byte = byte `div` 64

getByteIntList :: B.ByteString -> [Int]
getByteIntList bstr = map word8ToInt unpacked
        where unpacked = B.unpack bstr
