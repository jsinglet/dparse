module Log (
   info,debug,err,info',debug',err'
) where

import Text.Printf
import Data.Time.Format
import Data.Time
import Info
import System.Locale



data LogLevel = INFO | DEBUG | ERROR deriving Show



descr :: LogLevel -> String -> String -> String
descr level location message = printf "[%s] %s  From: %s  - %s" applicationName l location message
    where
        l = show level


{-| A set of logging functions, intended to help you indicate where in the program a given piece of log message comes from 
-}

info :: String -> String -> IO()
info location message = printLog INFO location message

debug :: String -> String -> IO()
debug location message = printLog DEBUG location message

err :: String -> String -> IO()
err location message = printLog ERROR location message

printLog :: LogLevel -> String -> String -> IO()
printLog level location message = do
    zt <- getZonedTime
    let ts = (formatTime defaultTimeLocale "%F %H:%M:%S,%q" zt)
    putStrLn $ printf "%s %s" ts (descr level location message)


---------------


descr' :: LogLevel -> String -> String
descr' level message = printf "[%s] %s  - %s" applicationName l message
    where
        l = show level

{-| These functions are to be used when you don't care about tracking where a particular message comes from -}

info' :: String -> IO()
info' message = printLog' INFO message

debug' :: String -> IO()
debug' message = printLog' DEBUG message

err' :: String -> IO()
err' message = printLog' ERROR message

printLog' :: LogLevel -> String -> IO()
printLog' level message = do
    zt <- getZonedTime
    let ts = (formatTime defaultTimeLocale "%F %H:%M:%S,%q" zt)
    putStrLn $ printf "%s %s" ts (descr' level message)


    
