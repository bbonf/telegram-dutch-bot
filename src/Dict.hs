module Dict
where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Text.EditDistance
import Config

data DutchWord = DutchWord { dutch :: String, english :: String, wordType :: String }
    deriving (Show)
newtype Dictionary = Dictionary [DutchWord]

lineToDutchWord :: String -> Maybe DutchWord
lineToDutchWord line = helper $ splitOn "\t" line
    where helper (nl:en:t:_) = Just (DutchWord nl en t)
          helper _ = Nothing

loadDictionary :: BotConfig -> IO Dictionary
loadDictionary cfg = do
    allLines <- lines <$> readFile (dictionaryPath cfg)
    return (Dictionary (mapMaybe lineToDutchWord allLines))

unique :: (Ord a) => [a] -> [a]
unique = map head . group . sort

similarWords :: Dictionary -> String -> [String]
similarWords (Dictionary words) word = 
    word:(take 3 byDist)
    where
    distance = levenshteinDistance defaultEditCosts word
    byDist = map head . group $ sortBy (comparing distance) $ map dutch words 
