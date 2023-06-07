module ASCII (asciiTable, testLookup) where

import qualified Data.IntMap as IM

asciiTable = IM.fromList $ zip [32 ..] " !\"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

testLookup = IM.lookup 42 asciiTable