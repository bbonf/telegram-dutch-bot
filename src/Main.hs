{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Concurrent
import Control.Lens
import Control.Monad
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (pack, Text)
import qualified Data.Text.IO as T
import Data.Scientific
import Data.Random (sample, uniform)
import Data.Random.Extras (choice)
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Feed.Import
import Text.Feed.Query
import Text.Regex.Base
import Text.Regex.PCRE
import Config
import Telegram
import Dict
import Metro

data FillBlanks = FillBlanks { original :: String, question :: String, answer :: String }
    deriving (Show)

quote :: String -> String
quote x = "\"" ++ x ++ "\""

sendFillBlanks :: BotConfig -> Dictionary -> FillBlanks -> IO String
sendFillBlanks cfg dict fb = do
    void (post (apiUrl cfg "sendMessage") ["chat_id" := chatId cfg, "text" := question fb, "reply_markup" := kbd])
    return (answer fb)
    where
    answers = intercalate "],[" $ map quote $ similarWords dict $ answer fb
    kbd = "{\"keyboard\":[[" ++ answers ++ "]]}" :: String

parseInt :: Scientific -> Integer
parseInt x = y
    where (Right y) = floatingOrInteger x

poll :: BotConfig -> Integer -> String -> IO()
poll cfg offset correctAnswer = do
    dict <- loadDictionary cfg
    threadDelay $ 10 ^ 6
    r <- asValue =<< getUpdates cfg offset
    let msg = r ^? responseBody . key "result" . nth 0 . key "message" . key "text" . _String
        next = case r ^? responseBody . key "result" . nth 0 . key "update_id" of
            (Just (Number x)) -> parseInt x
            Nothing -> 0 in do
                case msg of
                    (Just text) -> do
                        void $ sendMessage cfg $ pack $ "correct answer: " ++ correctAnswer
                        randomSentence >>= sendFillBlanks cfg dict >>= poll cfg (next+1)
                    Nothing -> poll cfg (next+1) correctAnswer


sentences :: String -> [String]
sentences = splitOn "."

randomSentence :: IO FillBlanks
randomSentence = do
    broken <- words.head.sentences <$> (fetchSummaries >>= (sample.choice)) 
    chosen <- sample $ uniform 0 $ length broken
    let (pre,post) = splitAt chosen broken
        question = unwords $ pre ++ ["_____"] ++ drop 1 post
        answer = head post in
        return (FillBlanks (unwords broken) question answer)
    
main :: IO()
main = do
    cp <- loadConfig "config.ini"
    case cp of
        (Right config) -> do
            putStrLn "bot started"
            poll config 0 ""

        _ -> putStrLn "could not load config"
