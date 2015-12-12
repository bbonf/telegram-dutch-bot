module Metro
where

import Text.Feed.Types (Item)
import Text.Feed.Import
import Text.Feed.Query
import Control.Lens
import Control.Monad
import Network.Wreq

fetchItems :: IO [Item]
fetchItems = do
    r <- get "http://www.metronieuws.nl/rss.xml"
    return (case parseFeedSource (r ^. responseBody) of
        (Just f) -> feedItems f
        Nothing -> [])

stripHtml :: String -> String
stripHtml = next '<'
    where next _ "" = ""
          next state part =
            case state of
            '<' -> let pre = takeWhile (/='<') part in
                pre ++ next '>' (drop (length pre +1 ) part)
            '>' -> let pre = takeWhile (/='>') part in
                next '<' (drop (length pre + 1) part)
                
summaryOrEmpty :: Item -> String
summaryOrEmpty item = 
    case summary of
        (Just s) -> s
        Nothing -> ""
    where summary = getItemSummary item

fetchSummaries :: IO [String]
fetchSummaries = do
    xs <- fetchItems
    return (map (stripHtml . summaryOrEmpty) xs)
