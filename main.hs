import System.Directory
import Data.List
import qualified Data.Map.Strict as Map
import Data.Char
import System.Random (randomRIO)
import Data.List (sortBy)
import Data.Ord (comparing)

type Corpus = [String]
type NGramModel = Map.Map String Int

-- Function to read the content of a text file
readFileContents :: FilePath -> IO String
readFileContents filePath = readFile filePath

-- Function to clean and tokenize text, keeping only words
cleanAndTokenize :: String -> [String]
cleanAndTokenize = words . unwords . words . map toLower . filter (\c -> isAlphaNum c || isSpace c)

-- Function to process a single file
processFile :: FilePath -> IO [String]
processFile filePath = do
    content <- readFileContents filePath
    return $ cleanAndTokenize content

-- Function to process all files in a directory
processDirectory :: FilePath -> IO [String]
processDirectory dirPath = do
    putStrLn "Processing .txt files..."
    files <- listDirectory dirPath
    let txtFiles = filter (\file -> ".txt" `isSuffixOf` file) files
    contents <- mapM (\file -> processFile (dirPath ++ "/" ++ file)) txtFiles
    return $ concat contents

-- Function to create a corpus from a list of files
createCorpus :: [FilePath] -> IO Corpus
createCorpus filePaths = do
    contents <- mapM processDirectory filePaths
    putStrLn "Corpus Created"
    return $ concat contents


-- Function to create a word-level n-gram model
createNGramModel :: Corpus -> Int -> NGramModel
createNGramModel corpus n =
    let ngrams = map (\xs -> take n xs) (tails corpus)
    in foldl' (\acc gram -> Map.insertWith (+) (unwords gram) 1 acc) Map.empty ngrams



-- Function to generate text using the n-gram model
generateText :: NGramModel -> Int -> Int -> IO String
generateText model n lengthLimit = do
    putStrLn "Generating text using n-gram model"
    let startWords = Map.keys model
    case startWords of
        [] -> return "Error: Empty startWords list"
        _  -> do
            start <- randomElement startWords
            case start of
                Just s -> generateText' model n lengthLimit [s]
                Nothing -> return "Error: Failed to select a starting word"


-- Helper function for text generation
generateText' :: NGramModel -> Int -> Int -> [String] -> IO String
generateText' _ _ 0 result = return $ unwords result
generateText' model n lengthLimit currentWords = do
    putStrLn "Helper function to generate the next words"
    possibleNextWords <- return $ Map.toList model
    let probabilities = calculateProbabilities possibleNextWords
        cumulativeDistribution = calculateCumulativeDistribution probabilities
    randomValue <- randomRIO (0, 1) :: IO Float
    let selectedWord = selectWord cumulativeDistribution randomValue
    case selectedWord of
        Just word -> generateText' model n (lengthLimit - 1) (currentWords ++ [word])
        Nothing   -> return $ unwords currentWords

-- Calculate probability mass function
calculateProbabilities :: [(String, Int)] -> [(String, Float)]
calculateProbabilities wordCounts =
    let total = fromIntegral $ sum $ map snd wordCounts
    in map (\(word, count) -> (word, fromIntegral count / total)) wordCounts


-- Calculate cumulative distribution
calculateCumulativeDistribution :: [(String, Float)] -> [(String, Float)]
calculateCumulativeDistribution probabilities =
    sortBy (comparing snd) $ scanl1 (\(_, acc) (word, prob) -> (word, acc + prob)) probabilities


-- Select word based on cumulative distribution
selectWord :: [(String, Float)] -> Float -> Maybe String
selectWord [] _ = Nothing
selectWord ((word, prob):rest) randomValue
    | randomValue <= prob = Just word
    | otherwise = selectWord rest randomValue

-- Helper function to get a random element from a list
randomElement :: [a] -> IO (Maybe a)
randomElement [] = return Nothing
randomElement xs = do
    putStrLn "Getting Random First Words"
    index <- randomRIO (0, length xs - 1)
    return $ Just (xs !! index)



main :: IO ()
main = do
    let bookFilePaths = ["books/"]
    corpus <- createCorpus bookFilePaths

    putStrLn $ "Corpus size: " ++ show (length corpus)
    putStrLn "NGramModel Created"
    let nGramModel = createNGramModel corpus 3

    -- Generate and display multiple phrases
    generateAndDisplayPhrases nGramModel 3 2

-- Function to generate and display multiple phrases
generateAndDisplayPhrases :: NGramModel -> Int -> Int -> IO ()
generateAndDisplayPhrases _ 0 _ = return ()
generateAndDisplayPhrases model numPhrases maxLength = do
    generatedText <- generateText model 3 maxLength
    putStrLn $ generatedText 
    generateAndDisplayPhrases model (numPhrases - 1) maxLength
