module Main where

import qualified Data.ByteString         as B
import           Data.ByteString.Builder
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8   as C
import           Lib
import           Numeric                 (readHex, showHex)
import           System.Environment      (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
            [inputFile, outputFile] -> run inputFile outputFile
            _                       -> usage


usage :: IO ()
usage = putStrLn "USAGE: haskellproject <input file name> <output file name>"

run :: String -> String -> IO()
run inputFile outputFile = do
  content <- B.readFile inputFile
  let firstByte = B.head content
  let firstFour = B.take 4 content
  print firstByte
  B.putStrLn firstFour
  let height = printHeight (decodeMaze content)
  let width = printWidth (decodeMaze content)
  print height
  print width
  let byte = byteToInt content 9
  print byte
  let firstBit = fstBit byte
  let secondBit = sndBit byte
  print firstBit
  print secondBit
