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
allSolutions words
  | maximum (intHistogram $ M.elems charHist) == 1 = wordSets
  | otherwise = error "This algorithm assumes that all letter frequencies are \
                      \distinct."
  where
  anagrams = M.fromListWith (++) $ zip (map wordKey words) $ map (:[]) words
  charHist = charHistogram words

  wordSets =  [ words
              | keys <- keySets 5 charKeys anagramKeys
              , words <- traverse (anagrams M.!) keys
              ]
    where
    charKeys = sortBy (compare `on` (charHist M.!)) $ M.keys charHist
    anagramKeys0 = M.keys anagrams
    anagramKeys =
      snd <$> (sortBy (compare `on` fst) $
                      zip (minCharFreq charHist <$> anagramKeys0) anagramKeys0)

  keySets 0 _ _ = [[]]
  keySets n charKeys anagramKeys =
    [ key:keySubset
    | (key:nextKeys) <- tails anagramKeys
    , hasInfrequentChar key charKeys
    , let isDisjoint = (==0) . (key .&.)
    , let disjointNextKeys = filter isDisjoint nextKeys
    , let nextCharKeys = filter isDisjoint charKeys
    , keySubset <- keySets (n-1) nextCharKeys disjointNextKeys
    ]

  hasInfrequentChar anagramKey (c0:c1:_) = (anagramKey .&. (c0 .|. c1)) /= 0

charHistogram :: [String] -> M.IntMap Int
charHistogram = foldl' (M.unionWith (+)) M.empty . map charHistogram1

charHistogram1 :: String -> M.IntMap Int
charHistogram1 = intHistogram . map charKey

intHistogram :: [Int] -> M.IntMap Int
intHistogram = M.fromListWith (+) . (`zip` repeat 1)

minCharFreq :: M.IntMap Int -> Int -> Int
minCharFreq charHist = minimum . map (charHist M.!) . keyBits

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
