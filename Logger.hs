-- -----------------------------------------------------------------------------
-- Copyright 2010, Eric Wong.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------
module Logger where

import HTTP
import Request
import Response
import Config

import Data.List (intercalate)
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.Time
import System.Locale (defaultTimeLocale)
import System.IO
import System.IO.Unsafe

-- what should we log? 
data Log = Log { lTime     :: ClockTime,
                 lSockAddr :: SockAddr,
                 lRequest  :: Request,
                 lStatusCode :: StatusCode,
                 lDelay    :: TimeDiff
               }
         | Connect ClockTime SockAddr
         | Disconnect ClockTime SockAddr

-- the interface used by log writers
writeLog :: SockAddr -> Request -> StatusCode -> TimeDiff -> IO ()
writeLog sockAddr req sc delay = do
  time <- getClockTime
  writeChan logChan $ Log time sockAddr req sc delay
 
writeConnectLog :: SockAddr -> IO ()
writeConnectLog sockAddr = do
  time <- getClockTime
  writeChan logChan $ Connect time sockAddr
 
writeDisconnectLog :: SockAddr -> IO ()
writeDisconnectLog sockAddr = do
  time <- getClockTime
  writeChan logChan $ Disconnect time sockAddr

logChan :: Chan Log
logChan = unsafePerformIO newChan

startLogger :: Config -> IO ()
startLogger conf = bracket (openFile (logFile conf) AppendMode)
                           hClose
                           (processLog conf)

processLog :: Config -> Handle -> IO ()
processLog conf h = do
  log <- readChan logChan
  hPutStrLn h $ logToString log
  hFlush h
  processLog conf h

logToString :: Log -> String
logToString Log {lTime = lt,
                 lSockAddr = lsa,
                 lRequest = lr,
                 lStatusCode = lsc,
                 lDelay = ld} = intercalate " ! " [ show lt, show lsa, show $ reqMethod lr, show $ reqURI lr,
                                                    show $ statusAndReason lsc, tdToString ld, agentToString $ reqHeaders lr]
logToString (Connect t sa) = show t ++ " @ " ++ show sa ++ " connected in."
logToString (Disconnect t sa) = show t ++ " @ " ++ show sa ++ " disconnected."

tdToString :: TimeDiff -> String
tdToString td = show (tdMin td) ++ ":" ++ show (tdSec td) ++ ":" ++ show (tdPicosec td)

agentToString :: [Header] -> String
agentToString hdrList = if agentHeader == []
                        then "*"
                        else head agentHeader
    where agentHeader = [ s | User_Agent s <- hdrList ]
