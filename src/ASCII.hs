module ASCII (asciiTable, ascii2num, num2ascii) where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

asciiTable :: IM.IntMap Char
asciiTable = IM.fromList $ zip [32 ..] asciiChars

asciiNums :: Map.Map Char Int
asciiNums = Map.fromList $ zip asciiChars [32 ..]

asciiChars :: [Char]
asciiChars = " !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

num2ascii :: IM.Key -> Maybe Char
num2ascii num = IM.lookup num asciiTable

ascii2num :: Char -> Maybe Int
ascii2num char = Map.lookup char asciiNums
