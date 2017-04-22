{-
	Rob Troost
	0850579
	INF2M
	
	Rotterdam University
	Functioneel Programmeren (INFFUP01)
	
	Final Assignment (Wordfeud)
-}

import System.IO
import Data.Char
import Data.List
import Data.Maybe


-- For every word in the wordList check a number of things
-- 		1. Does the word start with the user prefered prefix?
--		2. Does the word end with the user prefered suffix?
--		3. Is it possible to create the word with the available letters? A body of letters will be formed, prefix and suffix removed
-- If all of these checks pass, the word will be added to the return list
-- If one or more of these checks doesn't pass, the word will be dismissed
wordSuggestions :: [String] -> [(Char, Int)] -> String -> String -> [String]
wordSuggestions wordList availableLetters prefix suffix  = [ w | w <- wordList, prefix `isPrefixOf` w, suffix `isSuffixOf` w, makeWord (stripWord w (length prefix) (length suffix)) availableLetters ]


-- Check if the letters are avaible to make the word with prefix and/or suffix cut off
-- charCount is a list of required characters needed to make the rest of the word, presented the same way as the letters provided by the user
makeWord :: String -> [(Char, Int)] -> Bool
makeWord [] _ = False -- There are no letters left after cutting off the prefix and/or suffix so just return false
makeWord _ [] = False -- No letters available
makeWord letters availableLetters = and [ (letterCountCheck charCount availableLetters) | charCount <- (countOccurrences letters) ]


-- Check for every amount of each letter in the word we're making, if the user has enough of each
letterCountCheck :: (Char, Int) -> [(Char, Int)] -> Bool
letterCountCheck charCount availableLetters
	|	singleLetterCount >= snd charCount = True
	|	otherwise = False 
			where singleLetterCount = fromMaybe 0 (lookup (fst charCount) availableLetters)	


-- Strips a portion off the start and end of a String
stripWord :: String -> Int -> Int -> String
stripWord w lPrefix lSuffix = drop lPrefix (take (length w - lSuffix) w)


----------------------------
------ wordList preperation
----------------------------

words_file :: FilePath
words_file = "woordenlijst v2.10g.txt"

-- Read each line of the dictionary file into a List
-- Encoding must be set to UTF8 otherwise it won't be able to read chars such as 'ëíê'
getWords :: IO [String]
getWords = do	
	handle <- openFile words_file ReadMode
	hSetEncoding handle utf8
	contents <- hGetContents handle
	return (words contents)
	
-- The start of the entire preperation process
prepareWords :: [String] -> [String]
prepareWords wordList = replaceOdds $ filterScrabbleWords $ wordListToLower wordList

	
-- Turns every word in a List into its lower-case variant
wordListToLower :: [String] -> [String]
wordListToLower wordList = map (map toLower) wordList


-- Filters out any words that could never be made due to letters available in the game
filterScrabbleWords :: [String] -> [String]
filterScrabbleWords wordList = [ w | w <- wordList, all isLetter w ]


-- Loops through the List of words to check for weird letters like 'ëíê' and swaps 
-- them for scrabble letters via the 'replace' function
replaceOdds :: [String] -> [String]
replaceOdds wordList = [ replace w | w <- wordList ]

	
-- Replace weird letters with their scrabble alternatives (Indentation intented)
replace :: String -> String
replace [] = [] 
replace word = concatMap (\y -> 
	if y == 'ä' || y == 'à' || y == 'á' || y == 'â' then "a" else
		if y == 'ë' || y == 'è' || y == 'é' || y == 'ê' then "e" else
 			if y == 'ï' || y == 'ì' || y == 'í' || y == 'î' then "i" else
				if y == 'ö' || y == 'ò' || y == 'ó' || y == 'ô' then "o" else
					if y == 'ü' || y == 'ù' || y == 'ú' || y == 'û' then "u" else
	[y]) word


-- A different implementation of the same method
-- replace :: String -> String
-- replace [] = [] 
-- replace (x:xs) 
-- 	| x == 'ä' || x == 'à' || x == 'á' || x == 'â' = 'a':replace xs
-- 	| x == 'ë' || x == 'è' || x == 'é' || x == 'ê' = 'e':replace xs
-- 	| x == 'ï' || x == 'ì' || x == 'í' || x == 'î' = 'i':replace xs
--	| x == 'ö' || x == 'ò' || x == 'ó' || x == 'ô' = 'o':replace xs
--	| x == 'ü' || x == 'ù' || x == 'ú' || x == 'û' = 'u':replace xs
--	| otherwise = x:replace xs


-----------------------------
----- User-input preperation
-----------------------------

-- Count the occurrences of all different values in a list
-- Example: "maandag" would result in [ ('m', 1), ('a', 2), ('n', 1), ('d', 1), ('a', 1), ('g', 1) ]
countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences [] = []
countOccurrences x = [(head(y), length y) | y <- (group(sort x))]


-----------------------------------------
----- Letter value preperation (Dutch)
-----------------------------------------

{-

	I later noticed this wasn't a required part of the assignment.

-}

-- The value of each letter, taken from Wikipedia: http://en.wikipedia.org/wiki/Scrabble_letter_distributions#Dutch
letterValues = [ 	('e', 1), ('n', 1), ('a', 1), ('o', 1), ('i', 1),
					('d', 2), ('r', 2), ('s', 2), ('t', 2),
					('g', 3), ('k', 3), ('l', 3), ('m', 3), ('b', 3), ('p', 3),
					('u', 4), ('f', 4), ('h', 4), ('j', 4), ('v', 4), ('z', 4),
					('c', 5), ('w', 5),
					('x', 8), ('y', 8),
					('q', 10) ]

{-

-- Sort list by per-letter value
sortByPerLetterValue :: [String] -> [(String, Int)]
sortByPerLetterValue wordList = [ (w, val) | w <- wordList, val <- getPerLetterValue w ]


--Calculate the value of the word
getPerLetterValue :: [Char] -> Int
getPerLetterValue word = 

-}


-------------
------ Start
-------------

main = do

	-- Load the dictionary file into a List
	wordList <- getWords
	
	-- Get the user's available letters to attempt making a word with
	putStrLn "Welke letters heb je tot je beschikking?"
	availableLetters <- getLine
	
	-- Get the user's prefered prefix
	putStrLn "Waar moet het woord mee beginnen?"
	prefix <- getLine
	
	-- Get the user's prefered suffix
	putStrLn "Waar moet het woord mee eindigen?"
	suffix <- getLine
	
	putStrLn "--- Word suggestions ---"
	
	-- function wordSuggestions takes 4 parameters
	-- 		1. A list of words, which we loaded in from the file
	-- 			1.1 Each word in this list was checked if it only contained letters, so words like "'s avonds" were filtered
	--			1.2 Each word was transformed to lower-case since char checking is case sensitive
	--			1.3 Each word is checked for weird letters like 'ëíê' and swapped for scrabble letters
	--		2. A list of letters and their occurrences ("abcabca" would result in [ ('a', 3), ('b', 2), ('c', 2) ])
	--		3. The prefix as received from user-input
	--		4. The suffix as received from user-input
	-- Finalised by printing every String from the List via mapM_
	mapM_ putStrLn (wordSuggestions (prepareWords wordList) (countOccurrences availableLetters) prefix suffix)
