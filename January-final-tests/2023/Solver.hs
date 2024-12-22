module Solver where

import Data.List
import Data.Char
import Control.Monad
import Types
import WordData
import Clues
import Examples

------------------------------------------------------
-- Part I

punctuation :: String
punctuation 
  = "';.,-!?"

cleanUp :: String -> String
cleanUp = map toLower . filter (not . flip elem punctuation)

split2 :: [a] -> [([a], [a])]
split2 xs = [splitAt n xs | n <- [1..length xs - 1]]

split3 :: [a] -> [([a], [a], [a])]
split3 xs = [(x, y', z') | (x, z) <- split2 xs, (y', z') <- ([], z) : split2 z]

uninsert :: [a] -> [([a], [a])]
-- Filter only triples with non-empty middle element representing sublist
-- to be inserted
uninsert xs = map (\(x, y, z) -> (y, x ++ z)) (filter (\(a, b, c) -> not $ null b) (split3 xs))

split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs] 
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs

------------------------------------------------------
-- Part II

isPrefix :: String -> String -> Bool
isPrefix s1 s2 = and (zipWith (==) s1 s2) && (length s1 <= length s2)

matches :: String -> ParseTree -> Bool
matches s (Synonym s')        = s `elem` synonyms s'
matches s (Anagram _ s')      = sort s == sort s'
matches s (Reversal _ t)      = matches (reverse s) t
matches s (Insertion _ t1 t2) = or [matches s1 t1 && matches s2 t2 | (s1, s2) <- uninsert s]
matches s (Charade _ t1 t2)   = or [matches s1 t1 && matches s2 t2 | (s1, s2) <- split2 s]
-- Length of hidden word less than outer word and tail of prefixes is sufficient
-- to ensure hidden word not matches to beginning of outer word
matches s (HiddenWord _ s')   = (length s' > length s) && 
                                any (isPrefix s) (tail (filter ((> length s) . length) (tails s')))

evaluate :: Parse -> Int -> [String]
evaluate (def, _, t) n = filter (`matches` t) syns'
  -- Matches only synonyms that have correct length
  where syns' = filter ((== n) . length) (synonyms (unwords def))

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws,
            parseHidden ws]
    
parseSynonym :: [String] -> [ParseTree]
parseSynonym xs = let t = unwords xs in [Synonym t | not $ null $ synonyms t]

-- Parses constructions with one argument
parse1Arg :: [String] -> Indicator -> ([String] -> [a]) -> (Indicator -> a -> ParseTree) -> [ParseTree]
parse1Arg xs indList fArg ptConstructor = [ptConstructor [unwords ind] t |
                                           (ind, arg) <- split2M xs,
                                           unwords ind `elem` indList,
                                           t <- fArg arg]

parseAnagram :: [String] -> [ParseTree]
parseAnagram xs = parse1Arg xs anagramIndicators ((: []) . concat) Anagram

parseReversal :: [String] -> [ParseTree]
parseReversal xs = parse1Arg xs reversalIndicators parseWordplay Reversal

parseHidden :: [String] -> [ParseTree]
parseHidden xs = parse1Arg xs hiddenWordIndicators ((: []) . concat) HiddenWord

parse2Args :: [String] -> Indicator -> Indicator -> (Indicator -> ParseTree -> ParseTree -> ParseTree) -> [ParseTree]
parse2Args xs inds1 inds2 ptConstructor = [ptConstructor ind t1 t2 | (arg, ind, arg') <- splits1 ++ splits2,
                                                                     t1 <- parseWordplay arg,
                                                                     t2 <- parseWordplay arg']

  where frags = split3 xs
        splits1 = filter (\(_, ind, _) -> unwords ind `elem` inds1) frags
        -- First and last arguments switched according to rules for insertion
        -- and charade
        splits2 = [(a2, ind, a1) | (a1, ind, a2) <- frags, unwords ind `elem` inds2]

parseInsertion :: [String] -> [ParseTree]
parseInsertion xs = parse2Args xs insertionIndicators envelopeIndicators Insertion

parseCharade :: [String] -> [ParseTree]
parseCharade xs = parse2Args xs beforeIndicators afterIndicators Charade

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText xs = [(d, l, pt) | (d, l, p) <- split3M xs, 
                                 unwords l `elem` linkWords,
                                 not $ null $ synonyms $ unwords d,
                                 pt <- parseWordplay p]

solve :: Clue -> [Solution]
solve clue@(desc, n) = [(clue, t, res) | t <- parseClue clue, res <- evaluate t n]


------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]


