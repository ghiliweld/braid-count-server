{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where


import Network.Wai.Handler.Warp (run)  
import Network.Wai
import Network.HTTP.Types.Status (status200)
import Network.HTTP.Types.Method                        (Method, methodGet, methodPut, methodPatch)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import Data.ByteString.Lazy ( ByteString )
import qualified Data.Text.Internal.Lazy as L
import Control.Concurrent.MVar
    ( swapMVar, newMVar, readMVar, MVar )

var :: IO (MVar ByteString)
var = newMVar "yo\n"

application :: MVar ByteString -> Application
application v req respond = 
    if requestMethod req == methodGet
    then readMVar v >>= \b -> respond $ responseLBS status200 [("Content-Type", "text/plain")] b
    else strictRequestBody req >>= \body -> swapMVar v body >> readMVar v >>= \b -> respond $ responseLBS status200 [("Content-Type", "text/plain")] b

server :: IO ()
server = var >>= \v -> run 3000 $ application v
