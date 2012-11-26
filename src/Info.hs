module Info (applicationName,dataDir) where

import System.Directory

applicationName :: String
applicationName = "dparse"

dataDir :: IO FilePath
dataDir = do 
  dir <- getAppUserDataDirectory applicationName
  createDirectoryIfMissing True dir
  return (dir)


