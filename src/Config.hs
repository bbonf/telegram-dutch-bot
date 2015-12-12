module Config
where

import Control.Monad
import Control.Monad.Except
import Data.ConfigFile

data BotConfig = BotConfig { botId::String, token::String, chatId::String,
    dictionaryPath::String } 

loadConfig cfgPath = runExceptT $ do
    cp <- join $ liftIO $ readfile emptyCP cfgPath
    _botId <- get cp "bot" "id"
    _token <- get cp "bot" "token"
    _chatId <- get cp "chat" "id"
    _dictionary <- get cp "dictionary" "path"
    return (BotConfig _botId _token _chatId _dictionary)
