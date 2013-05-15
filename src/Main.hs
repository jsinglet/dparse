{- |
Module      :  Main (DParse)

Author      :  John L. Singleton <jsinglet@gmail.com>
Portability :  portable 

DParse is a program conforming to the streaming map-reduce protocol, designed specifically for use within Amazon's Elastic Map Reduce.
-}

module Main where

-- | General stuff
import Network.Curl
import Data.Maybe
import Data.String.Utils
import Debug.Trace
import Text.HTML.TagSoup
import System.IO
import Control.Exception 
import Prelude hiding (catch)

-- | Project specific imports.
import DTypes
import Queue
import ProductTree
import Hints

-- | Helper for getting text of a given page
getURLText :: String -> IO String
getURLText u = curlGetString_ (u) [CurlTimeout 60, CurlUserAgent userAgent, CurlSSLVersion 3] >>= \(_,body) -> return (body) 


{- | 
   Run BFS over this graph and return the "deepest" node. 
   The deepest node will also be the best and most complete possible match for this page.
-}
bfs :: String -> String -> Graph -> [Node] -> Maybe Node
bfs source text graph [] = bfs source text graph [newRoot]
bfs source text graph nodeQueue = do

  let (unexplored, newQueue)  = dequeue nodeQueue
  let adjacentNodes = adjacencyListLookup (category unexplored) graph

  case adjacentNodes of
    -- Nothing is adjacent to this node (we are at the end)
    Nothing -> if length newQueue == 0 then return unexplored
               else bfs source text graph newQueue
    Just nodes -> do
      let validNodes  = filter (\x -> (matcher x) text (hints source)) nodes 
      if (length newQueue) + (length validNodes)  == 0 then return unexplored
         else bfs source text graph (newQueue ++ validNodes)


{- | 
   Walk up the tree, finding the path we took to get here 
 -}
walk :: Maybe Node -> [Node]
walk Nothing = []
walk node = walk (parentCategory n) ++ [n]
            where
              n = fromJust node

getTaxonomy :: Maybe Node -> [Node]
getTaxonomy base = tail $ walk base




{- | Main taxonomy functions -}

taxonomizeToken :: String -> String -> [String]
taxonomizeToken source token = do
  -- first, find out the depest node that can classify this token
  let bestMatch = bfs source token defaultGraph []

  -- now, find out everything higher than this node, ie, its parents.
  map (\t -> category t) $ getTaxonomy bestMatch


extractId :: String -> String -> String
extractId site body = do 
  let Just h = hints site
  let Just s = productBlock h
  unwords . words $ extract' site s body

extract' :: String -> ElementSpec -> String -> String
extract' site elementSpec body = do


  let tags = parseTags body

  case (elementSpec) of

    -- in this case, we are matching what is INSIDE of the tag
    ElementSpec { 
               elementType = Just et, 
               elementAttribute = Just ea, 
               elementAttributeValue = Just eav, 
               elementValue = Nothing } ->  do
                        fromTagText (dropWhile (~/= tagMatcher) tags !! 1)
                               where 
                                 tagMatcher = "<" ++ et ++ " " ++ ea ++ "=" ++ eav ++ ">"

    ElementSpec { 
               elementType = Just et, 
               elementAttribute = Just ea, 
               elementAttributeValue = Just eav, 
               elementValue = Just ev } ->  do 
                         fromAttrib ev (dropWhile (~/= tagMatcher) tags !! 0)
                                where 
                                  tagMatcher =  "<" ++ et ++ " " ++ ea ++ "=" ++ eav ++ ">"
    _ -> error $ "Don't know anything about site " ++ site ++ " so I can't parse it."
    
    

extractPrice :: String -> String -> String
extractPrice site body = do 
  let Just h = hints site
  let Just s = priceBlock h
  unwords . words $ extract' site s body

extractImage :: String -> String -> String
extractImage site body = do 
  let Just h = hints site
  let Just s = imageBlock h
  unwords . words $ extract' site s body



-- extract the site name from a url http://astr
siteOfUrl :: String -> String
siteOfUrl url 
    | (startswith "https://" url) = extract rest2
    | (startswith "www." rest) = extract restWithoutWWW
    | otherwise = extract rest
    where
      (_,rest) = splitAt 7 url
      (_,rest2) = splitAt 8 url
      (_,restWithoutWWW) = splitAt 11 url
      extract = \s -> takeWhile (\a -> a /= '/') s
  

-- main taxonomy function
taxonomizeURL :: String -> IO [String]
taxonomizeURL url = getURLText url >>= \body -> return (
  taxonomy (taxonomizeToken site $ extractId site body) ++ -- taxonomy
  [extractId site body] ++  -- detailed product title
  [extractPrice site body] ++ -- the price
  [url] ++ -- the url
  [extractImage site body] ++ 
  [site])   -- the site
  where
    site = siteOfUrl url
    taxonomy tax= case (tax) of
                 [t1, t2] -> [t1, t2]
                 [t1]     -> [t1, "N/A"]
                 _       -> ["N/A", "N/A"]
                  
           

                              
        
testURLS :: [String]
testURLS = [ "http://www.cdw.com/shop/products/HP-Compaq-Pro-6300-Desktop-CDW-Exclusive/2828989.aspx"
           ,"http://www.cdw.com/shop/products/HP-EliteBook-8460p-14in-Core-i5-2450M-Windows-7-Professional-64-bit/2610253.aspx?cm_sp=Hub-_-Session-_-Notebook+Computers&ProgramIdentifier=3&RecommendedForEDC=00000001&RecoType=RU"
           ,"http://www.cdw.com/shop/products/Lenovo-ThinkPad-T430-2342-14in-Core-i5-3320M-Windows-7-Professional-64/2743319.aspx"
           ,"http://www.target.com/p/sony-vaio-14-laptop-pc-sve14112fxw-with-640gb-hard-drive-4gb-memory-black-white/-/A-14109249#prodSlot=medium_1_2&term=laptops", "https://sites.google.com/site/cop4331dealaggregator/fake-item"]

--test :: [IO ()]
test = mapM f testURLS
  where
    f x = do 
         tax <- taxonomizeURL x
         putStrLn "|-------Parse Result Start---------|"
         putStrLn $ join "\t" tax
         putStrLn "|-------Parse Result End-----------|"



processURL :: String -> Handle -> IO ()
processURL url fh = do
  tax <- taxonomizeURL url
  let taxLine = join "\t" tax
  putStrLn taxLine
  hPutStrLn fh taxLine

-- | Main Error Handler -- we want to make this not print anything
-- | when we are running this in production
processURLError :: SomeException -> IO ()
--processURLError _ = do
--putStrLn "Error Processing URL"
processURLError _ = return ()


processSTDIN :: Handle -> Integer -> IO ()
processSTDIN fh it = do
  done <- hIsEOF stdin
  if done 
    then return ()
    else 
      do
        url <- getLine
        putStrLn ("Processing URL " ++ show it)
        catch (processURL url fh) processURLError
        processSTDIN fh (it+1)
             

test2 = mapM f testURLS
  where
    f x = do 
         tax <- taxonomizeURL x
         putStrLn "|-------Parse Result Start---------|"
         putStrLn $ join "\t" tax
         putStrLn "|-------Parse Result End-----------|"



  



main :: IO()
main = do 
  fh <- openFile "out.txt" WriteMode
  processSTDIN fh 1
  hClose fh


-- main :: IO()
-- main = do 
--   tax <- taxonomizeURL "https://sites.google.com/site/cop4331dealaggregator/fake-item"
--   let taxLine = join "\t" tax
--   putStrLn taxLine



