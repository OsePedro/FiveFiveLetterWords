module Main(main) where

import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Function
import qualified Data.IntMap.Strict as M
import Data.List

main = do
  words <-  filter acceptWord . map BS.unpack . BS.words <$>
            BS.readFile "words_alpha.txt"
  let solutions = allSolutions words
  sequence_ $ map (putStrLn . intercalate ", ") solutions
  putStrLn $ "Found "++show (length solutions)++" solutions"

allSolutions :: [String] -> [[String]]
allSolutions words = wordSets
  where
  anagrams = M.fromListWith (++) $ zip (map wordKey words) $ map (:[]) words
  charHist = charHistogram words

  wordSets =  [ words | keys <- keySets 5 charKeys angrmKeys
                      , words <- traverse (anagrams M.!) keys]
    where
    charKeys = sortBy (compare `on` freqCharKey charHist) $ M.keys charHist
    angrmKeys0 = M.keys anagrams
    angrmKeys = let mfcks = minFreqCharKey charHist <$> angrmKeys0
                in  snd <$> (sortBy (compare `on` fst) $ zip mfcks angrmKeys0)

  keySets 0 _ _ = [[]]
  keySets n charKeys angrmKeys =
    [ angrmKey:angrmKeySubset
    | (angrmKey:nextAngrmKeys) <- tails angrmKeys
    , hasInfrequentChar charKeys angrmKey
    , let isDisjoint = (==0) . (angrmKey .&.)
    , let disjointNextKeys = filter isDisjoint nextAngrmKeys
    , let nextCharKeys = filter isDisjoint charKeys
    , angrmKeySubset <- keySets (n-1) nextCharKeys disjointNextKeys
    ]

  hasInfrequentChar (c0:c1:_) anagramKey = ((c0 .|. c1) .&. anagramKey) /= 0

charHistogram :: [String] -> M.IntMap Int
charHistogram = foldl' (M.unionWith (+)) M.empty . map charHistogram1

charHistogram1 :: String -> M.IntMap Int
charHistogram1 = intHistogram . map charKey

intHistogram :: [Int] -> M.IntMap Int
intHistogram = M.fromListWith (+) . (`zip` repeat 1)

-- Uses charKey to break frequency ties
freqCharKey :: M.IntMap Int -> Int -> (Int,Int)
freqCharKey charHist charKey = (charHist M.! charKey, charKey)

minFreqCharKey :: M.IntMap Int -> Int -> (Int,Int)
minFreqCharKey charHist = minimum . map (freqCharKey charHist) . keyBits

keyBits :: Int -> [Int]
keyBits = unfoldr $ \key ->
  let n = countTrailingZeros key
  in  if key==0 then Nothing else Just (1 `shiftL` n, clearBit key n)

wordKey :: String -> Int
wordKey = foldl' (.|.) 0 . map charKey

charKey :: Char -> Int
charKey = (1 `shiftL`) . subtract 65 . fromEnum . toUpper

acceptWord :: String -> Bool
acceptWord word =
  length word == 5 && all isAlpha word && maximum (charHistogram1 word) == 1
