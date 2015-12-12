{-# LANGUAGE OverloadedStrings #-}
module Telegram
where

import Data.Text (pack, Text)
import Network.Wreq
import Control.Monad (void)
import Control.Lens

import Config

apiUrl :: BotConfig -> String -> String
apiUrl cfg method = "https://api.telegram.org/bot" ++
    botId cfg ++ ":" ++ token cfg ++ "/" ++ method

sendMessage :: BotConfig -> Text -> IO()
sendMessage cfg msg =
    void (post (apiUrl cfg "sendMessage") ["chat_id" := chatId cfg, "text" := msg])

getUpdates cfg offset = let opts = (defaults & param "offset" .~ [ pack $ show offset ]) in
    getWith opts $ apiUrl cfg "getUpdates"

