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
module Network where

import HTTP
import Request
import Response
import Config
import Handler
import Logger

import Network.Socket
import Network.BSD
import Control.Monad (unless, when)
import Control.Concurrent (forkIO, myThreadId)
import Control.Exception (IOException, bracket, handle, finally)
import System.IO
import System.Timeout (timeout)
import System.Time

serve :: Config -> IO ()
serve conf = do
  let servePort = fromIntegral $ port conf
  tcpProtocolNum <- getProtocolNumber "tcp"
  bracket (socket AF_INET Stream tcpProtocolNum)
          sClose
          (\serveSocket -> do bindSocket serveSocket $ SockAddrInet servePort iNADDR_ANY
                              listen serveSocket $ maxClients conf
                              acceptAndHandle conf serveSocket
          )

acceptAndHandle :: Config -> Socket -> IO ()
acceptAndHandle conf sock = do
  (incomeSock, incomeSockAddr) <- accept sock
  writeConnectLog incomeSockAddr
  h <- socketToHandle incomeSock ReadWriteMode
  hSetBuffering h LineBuffering
  forkIO (catch (finally (processRequest conf h incomeSockAddr True)  -- all exceptions catched here, and the server won't crash.
                         (hClose h >> writeDisconnectLog incomeSockAddr))
                (\e -> return ())
         )
  acceptAndHandle conf sock

processRequest :: Config -> Handle -> SockAddr -> Bool -> IO ()
processRequest conf h sockAddr first = do
  let timeOut | first = requestTimeout conf * 10^6
              | otherwise = keepAliveTimeout conf * 10^6
  reqResult <- timeout timeOut (getRequest h)
  start <- getClockTime
  case reqResult of
    Just req -> do let request = parseRequest req   -- TODO: detect partial requests and send back 'Bad Request'
                   resp <- handleRequest conf request
                   let statCode = statusCode resp
                   end <- getClockTime
                   writeLog sockAddr request statCode $ diffClockTimes end start
                   hPutStr h $ show resp
                   hFlush h
                   unless (Connection "close" `elem` reqHeaders request   -- connection is handled here. 
                           || (httpVer request < httpVersionSupported     -- so we don't need to write a handler for it.
                               && (Connection "keep-alive" `notElem` reqHeaders request)))
                          $ processRequest conf h sockAddr False
    Nothing -> when first $
                 do hPutStr h $ show requestTimeoutResponse
                    hFlush h
                    processRequest conf h sockAddr False

getRequest :: Handle -> IO [String]
getRequest h = do
  l <- hGetLine h        -- when the connection is losed abruptly, we will get a 'end of file' exception.
  if emptyLine l         -- but it will be catched by the top 'finally' function, and it will just exit the thread.
     then getRequest h   -- so I don't address it here.
     else getRequest' l h

getRequest' :: String -> Handle -> IO [String]
getRequest' l h = 
    if emptyLine l
       then return []
       else do l' <- hGetLine h
               ls <- getRequest' l' h
               return (l:ls)
emptyLine :: String -> Bool
emptyLine "\r" = True
emptyLine _ = False

