module Test where

import Text.HTML.TagSoup
import Network.Curl
import Debug.Trace

-- | Helper for getting text of a given page
getURLText :: String -> IO String
getURLText u = curlGetString_ (u) [CurlTimeout 60, CurlSSLVersion 3] >>= \(_,body) -> return (body) 






ndmPapers :: IO ()
ndmPapers = do
  body <- getURLText "http://www.timeanddate.com/worldclock/city.html?n=136"
  let tags = parseTags body
  let tags2 = dropWhile (~/= "<strong id=ct>") tags 
  putStrLn (renderTags tags2)


  