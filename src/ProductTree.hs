module ProductTree where


import DTypes
import Data.List
import Data.Char
import Debug.Trace

-- Helper function for finding stuff in the tree 
adjacencyListLookup :: String -> Graph -> Maybe [Node]
adjacencyListLookup what ingraph = lookup what ingraph >>= \(n,nodes) -> return (nodes)


{- Main Tree Taxonomny functions. -}

-- T1
electronics = newNode "Electronics" dm (Just newRoot) 

-- T2
tvs = newNode "Televisions" dm (Just electronics)
homeVideo = newNode "Home Video" dm  (Just electronics)
homeAudio = newNode "Home Audio" dm  (Just electronics)
carAudio  = newNode "Car Audio" dm  (Just electronics)

-- T1
cameras = newNode "Cameras" dm (Just newRoot)
software = newNode "Software" dm (Just newRoot)
cellPhones = newNode "Cell Phones" dm (Just newRoot)

-- T1
computers = newNode "Computers" matchComputers (Just newRoot)
--T2
laptops  = newNode "Laptops" matchLaptops (Just computers)
tablets  = newNode "Tablets" dm (Just computers)
desktops = newNode "Desktops" matchDesktops (Just computers) 



{- The graph we use to produce the classification. -}

defaultGraph :: Graph
defaultGraph = [("root", (newRoot, [electronics, cameras, software, cellPhones, computers]))  -- T1
           , ("Computers", (computers, [laptops, tablets, desktops])) -- T2
           , ("Electronics", (electronics, [tvs, homeVideo, homeAudio, carAudio]))]



{-| 

  The Real category matchers.... 

-}

-- matches computers by trying to look at common brandnames and things found in product titles.
matchComputers :: CategoryMatcher 
matchComputers s h = length (matches \\ us) /= length (matches)
    where
      matches = ["desktop", "laptop", "workstation", "i3", "intel", "windows", "i5", "atom", "amd", "i7"]
      us = words $ map toLower s

matchLaptops :: CategoryMatcher 
matchLaptops s h = length (matches \\ us) /= length (matches)
    where
      matches = ["laptop", "book", "thinkpad", "viao", "elitebook", "ideapad", "pavalon", "macbook", "air" ]
      us = words $ map toLower s


matchDesktops :: CategoryMatcher 
matchDesktops s h = length (matches \\ us) /= length (matches) || find (\e -> e `isInfixOf` rus) fullMatches /= Nothing
    where
      matches = ["desktop", "computer", "compaq", "thinkcentre", "veriton", "elite"]
      fullMatches = ["monitor : none"]
      rus = map toLower s
      us = words $ map toLower s





