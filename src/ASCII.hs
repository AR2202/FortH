module ASCII (asciiTable, ascii2num, num2ascii) where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

-- map from ASCII code to character
asciiTable :: IM.IntMap Char
asciiTable = IM.insert 0 '\n' $ IM.fromList $ zip [32 ..] asciiChars

-- map from character to ASCII code
asciiNums :: Map.Map Char Int
asciiNums =Map.insert  '\n' 0 $ Map.fromList $ zip asciiChars [32 ..]

--list of ASCII Characters
asciiChars :: [Char]
asciiChars = " !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

-- lookup function from ASCII code to character
num2ascii :: IM.Key -> Maybe Char
num2ascii num = IM.lookup num asciiTable

-- lookup function from character to ASCII code
ascii2num :: Char -> Maybe Int
ascii2num char = Map.lookup char asciiNums