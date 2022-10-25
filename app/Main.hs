module Main(main) where

import Control.Arrow
import Data.Bits
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Function
import qualified Data.IntMap.Strict as M
import Data.List
import Data.Maybe
import Data.Ord(Down(Down))

readWords = filter  acceptWord . map BS.unpack . BS.words <$>
                    BS.readFile "words_alpha.txt"

testTrie = do
  let words = ["abcde","abcdf","abcdg","abdef","vwxyz"]
  -- words <- readWords
  -- putStrLn $ "Found " ++ show (length words) ++ " words"
  let someWords = {-take 1000 $ -} words
  let charHist = charHistogram someWords
  let anagrams = groupAnagrams someWords
  let ranker = freqRanker charHist
  let angrmKeyRanks = anagramKeyRank ranker <$> sortAnagramKeys ranker anagrams
  let trie0 = trie angrmKeyRanks
  let akrString = map keyChar . unfoldr (popMinCharKey ranker)
  let printTrie = putStrLn . showTrie ranker

  print $ sortBy (compare `on` snd) $ first keyChar <$> M.toList charHist
  print $ akrString <$> angrmKeyRanks
  printTrie trie0

  let akrsTries = filter (not . isEmptyTrie . snd) $
                  zip angrmKeyRanks
                      ((`eraseAnagramKeyRank` trie0) <$> angrmKeyRanks)
  if null akrsTries
  then  putStrLn "All erasures create empty trees"
  else  do  let (akr,trie1) = head akrsTries
            let minCharKeys = unfoldr (popMinCharKey ranker) akr
            putStrLn $ "Erased: " ++ (keyChar <$> minCharKeys)
            printTrie trie1



main = do
  words <- readWords
  let solutions = allSolutions words
  sequence_ $ map (putStrLn . intercalate ", ") solutions
  putStrLn $ "Found "++show (length solutions)++" solutions"

allSolutions :: [String] -> [[String]]
allSolutions words = wordSets
  where
  anagrams = groupAnagrams words

  wordSets =  [ words | keys <- keySets 5 charKeys angrmKeys
                      , words <- traverse (anagrams M.!) keys]
    where
    charHist = charHistogram words
    charKeys = sortBy (compare `on` freqCharKey charHist) $ M.keys charHist
    ranker = freqRanker charHist
    angrmKeys = sortAnagramKeys ranker anagrams

  keySets 0 _ _ = [[]]
  keySets n charKeys angrmKeys =
    [ angrmKey:angrmKeySubset
    | (angrmKey:nextAngrmKeys) <- tails angrmKeys
    , hasInfrequentChar charKeys angrmKey
    , let isDisjoint = disjoint angrmKey
    , let disjointNextKeys = filter isDisjoint nextAngrmKeys
    , let nextCharKeys = filter isDisjoint charKeys
    , angrmKeySubset <- keySets (n-1) nextCharKeys disjointNextKeys
    ]

  hasInfrequentChar (c0:c1:_) angrmKey = ((c0 .|. c1) .&. angrmKey) /= 0

sortAnagramKeys :: FreqRanker -> M.IntMap [String] -> [Int]
sortAnagramKeys ranker anagrams =
  sortBy (compare `on` anagramKeyRank ranker) $ M.keys anagrams

groupAnagrams :: [String] -> M.IntMap [String]
groupAnagrams words =
  M.fromListWith (++) $ zip (map anagramKey words) $ map (:[]) words

charHistogram :: [String] -> M.IntMap Int
charHistogram = foldl' (M.unionWith (+)) M.empty . map charHistogram1

charHistogram1 :: String -> M.IntMap Int
charHistogram1 = intHistogram . map charKey

intHistogram :: [Int] -> M.IntMap Int
intHistogram = M.fromListWith (+) . (`zip` repeat 1)

data FreqCharKey =
  FreqCharKey {minFreq :: Int, fckCharKey :: Int} deriving(Eq,Ord)

-- Uses charKey to break frequency ties
freqCharKey :: M.IntMap Int -> Int -> FreqCharKey
freqCharKey charHist charKey =
  FreqCharKey {minFreq = charHist M.! charKey, fckCharKey = charKey}

minFreqCharKey :: M.IntMap Int -> Int -> FreqCharKey
minFreqCharKey charHist = minimum . map (freqCharKey charHist) . listCharKeys

newtype FreqRank = FreqRank {fromFreqRank :: Int} deriving(Eq)

instance Ord FreqRank where compare = compare `on` (Down . fromFreqRank)

data FreqRanker =
  FreqRanker {
    charKey2FreqRank :: Int -> FreqRank
  , freqRank2CharKey :: FreqRank -> Int
  }

freqRanker :: M.IntMap Int -> FreqRanker
freqRanker charHist =
  let fcks = sort $ map (freqCharKey charHist) $ M.keys charHist
      charKeys = fckCharKey <$> fcks
      invRanks = (1 `shiftL`) <$> [25,24..]
      ck2ifr = M.fromList $ zipWith (curry $ second FreqRank) charKeys invRanks
      ifr2ck = M.fromList $ zip invRanks charKeys
  in  FreqRanker {
        charKey2FreqRank = (ck2ifr M.!)
      , freqRank2CharKey = (ifr2ck M.!) . fromFreqRank
      }

newtype AnagramKeyRank = AnagramKeyRank {fromAnagramKeyRank :: Int} deriving(Eq)

instance Ord AnagramKeyRank where
  compare = compare `on` (Down . fromAnagramKeyRank)

anagramKeyRank :: FreqRanker -> Int -> AnagramKeyRank
anagramKeyRank rnkr =
  AnagramKeyRank . bitUnion . map (fromFreqRank . charKey2FreqRank rnkr) .
  listCharKeys

-- Pops the rank of the least frequent letter
popFreqRank :: AnagramKeyRank -> Maybe (FreqRank, AnagramKeyRank)
popFreqRank (AnagramKeyRank 0) = Nothing
popFreqRank (AnagramKeyRank r) =
  let msBitIndex n = finiteBitSize n - countLeadingZeros n - 1
  in  (FreqRank *** AnagramKeyRank) <$> popBit msBitIndex r

popMinCharKey :: FreqRanker -> AnagramKeyRank -> Maybe (Int, AnagramKeyRank)
popMinCharKey ranker akr = (first $ freqRank2CharKey ranker) <$> popFreqRank akr

listCharKeys :: Int -> [Int]
listCharKeys = unfoldr $ popBit countTrailingZeros

popBit :: (Int -> Int) -> Int -> Maybe (Int,Int)
popBit bitIndex n =
  let i = bitIndex n
  in  if n==0 then Nothing else Just (1 `shiftL` i, clearBit n i)

disjoint :: Int -> Int -> Bool
disjoint key0 key1 = (key0 .&. key1) == 0

bitUnion :: [Int] -> Int
bitUnion = foldl' (.|.) 0

anagramKey :: String -> Int
anagramKey = bitUnion . map charKey

charKey :: Char -> Int
charKey = (1 `shiftL`) . subtract 97 . ord . toLower

keyChar :: Int -> Char
keyChar = chr . (97 +) . countTrailingZeros

acceptWord :: String -> Bool
acceptWord word =
  length word == 5 && all isAlpha word && maximum (charHistogram1 word) == 1

eraseFromKey :: FreqCharKey -> Int -> Int
eraseFromKey fck = ((complement $ fckCharKey fck) .&.)

data Trie =
    EmptyTrie
  | TrieNode {
      freqRank :: FreqRank
    , containers :: Trie
    , lackers :: Trie
    }

-- Pre: the AnagramKeyRanks are sorted, distinct and non-zero.
--
-- A TrieNode has EmptyTrie containers if and only if the TrieNode represents
-- the last minimum-frequency letter that a word has in common with other words.
trie :: [AnagramKeyRank] -> Trie
trie [] = EmptyTrie
trie (angrmKey:angrmKeys)
  | null cntnrPairs = TrieNode {freqRank=fr, containers=EmptyTrie, lackers=lt}
  | null lckrPairs = ct
  | otherwise = TrieNode {freqRank=fr, containers=ct, lackers=lt}
  where
  popIFR = fromJust . popFreqRank
  (fr,akrTail) = popIFR angrmKey
  frTailPairs = zip (popIFR <$> angrmKeys) angrmKeys
  (cntnrPairs,lckrPairs) = span ((== fr) . fst . fst) frTailPairs
  ct = trie $ akrTail : (snd . fst <$> cntnrPairs)
  lt = trie $ snd <$> lckrPairs

isEmptyTrie EmptyTrie = True
isEmptyTrie _ = False

-- Erases all anagram keys containing any letter in the given anagram key.
eraseAnagramKeyRank :: AnagramKeyRank -> Trie -> Trie
eraseAnagramKeyRank _ EmptyTrie = EmptyTrie
eraseAnagramKeyRank (AnagramKeyRank 0) t = t
eraseAnagramKeyRank akr node = case compare fr $ freqRank node of
  EQ -> eraseAnagramKeyRank akrTail $ lackers node
  LT -> eraseAnagramKeyRank akrTail node
  GT -> eraseFromChildren
  where
  (fr,akrTail)= fromJust $ popFreqRank akr
  eraseFromChildren -- this is wrong!!!
    | isEmptyTrie ct = lt
    | isEmptyTrie lt = ct
    | otherwise = TrieNode {freqRank=fr, containers=ct, lackers=lt}
    where
    ct = eraseAnagramKeyRank akr $ containers node
    lt = eraseAnagramKeyRank akr $ lackers node


showTrie :: FreqRanker -> Trie -> String
showTrie ranker = go 0
  where
  go indent t
    | isEmptyTrie t = "."
    | otherwise = leadingSpace ++ [' ',char,':'] ++
                  showChild hasLackers indent (containers t) ++
                  if hasLackers
                  then  leadingSpace ++ ['¬',char,':'] ++
                        showChild False indent (lackers t)
                  else  ""
    where
    leadingSpace = replicate indent ' '
    char = keyChar $ freqRank2CharKey ranker $ freqRank t
    hasLackers = not $ isEmptyTrie $ lackers t

    showChild appendNewline indent child =
      prefix ++ go newIndent child ++ suffix
      where
      newIndent = indent+3
      prefix  | isEmptyTrie child = " "
              | otherwise = "\n"
      suffix  | appendNewline = "\n"
              | otherwise = ""

  -- show = go 0
  --   where
  --   go indent t
  --     | isEmptyTrie t = leadingSpace ++ "."
  --     | otherwise =
  --         leadingSpace ++ ' ':char:":\n" ++
  --         go newIndent (containers t) ++ "\n" ++
  --         leadingSpace ++ '¬':char:":\n" ++
  --         go newIndent (lackers t)
  --     where
  --     leadingSpace = replicate indent ' '
  --     newIndent = indent+3
  --     char = keyChar $ fckCharKey $ minFreqCKey t
