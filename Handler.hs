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
module Handler where

import HTTP
import Request
import Response
import Config

import Prelude hiding (lookup)
import Data.Map (lookup)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((</>), takeExtension, hasTrailingPathSeparator)
import System.IO
import System.Time (ClockTime)
import Control.Monad.Error

handleRequest conf req = handleRequestMethod conf req $ reqMethod req

---------------- a new handler framework, still in work. -------------
type IOResultResponse = ErrorT Response IO

type Handler = Config -> Request -> Response -> IOResultResponse Response

instance Error Response where
    noMsg = defaultResultResponse
    strMsg s = defaultResultResponse { respBody = Just s}

-- if you want to stop handling the request and send the response directly in your handler,
-- then use this one.
sendThisResponse :: Response -> IOResultResponse Response
sendThisResponse = throwError

extractResponse :: Either Response Response -> Response
extractResponse (Right resp) = resp

-- used together with the handlerReducer function.
getTheResultResponseAndSend :: IOResultResponse Response -> IO Response
getTheResultResponseAndSend ioResultResp = liftM extractResponse $ runErrorT $ catchError ioResultResp return

handlerReducer :: Config -> Request -> Response -> [Handler] -> IOResultResponse Response
handlerReducer conf req resp handlerList = foldl applyHandler (return resp) handlerList
    where applyHandler :: IOResultResponse Response -> Handler -> IOResultResponse Response
          applyHandler resp handler = resp >>= handler conf req

handleRequestMethod :: Config -> Request -> Method -> IO Response
handleRequestMethod conf req GET = getTheResultResponseAndSend $ handlerReducer conf req defaultGETResponse getHandlerList
    where defaultGETResponse = defaultResultResponse
          getHandlerList =[versionChecker,dateHeaderAdder,fileRetriever]
handleRequestMethod conf req HEAD = handleRequestMethod conf req GET >>= removeBody
handleRequestMethod conf req _ = return $ addHeader defaultResultResponse { statusCode = SC405 } (Allow "GET, HEAD")

------------ handlers -------------------

addHeader :: Response -> Header -> Response
addHeader resp header = resp { respHeaders = rh ++ [header] }
    where rh = respHeaders resp
addHeaders :: Response -> [Header] -> Response
addHeaders = foldl addHeader

versionChecker :: Handler
versionChecker conf req resp = if httpVer req > httpVersionSupported
                                 then sendThisResponse $ resp { statusCode = SC505}
                                 else return resp
dateHeaderAdder :: Handler
dateHeaderAdder conf req resp = fmap (addHeader resp) (liftIO getDateHeader)

fileRetriever :: Handler
fileRetriever conf req resp = do
  let filePath = handleReqURI conf $ reqURI req
  fExists <- liftIO $ doesFileExist filePath
  case fExists of
    True -> do (fLength, modTime, body) <- liftIO $ getFile filePath
               let cType = getFileType conf filePath
               return $ addHeaders resp{ statusCode = SC200, respBody = Just body} 
                          [contentLengthHeader fLength,
                           contentTypeHeader cType,
                           lastModHeader modTime]
    False -> sendThisResponse $ addHeaders resp{ statusCode = SC404 , respBody = Just body }
                                  [contentLengthHeader fLength,
                                   contentTypeHeader cType]
               where (body, cType, fLength) = fileNotFoundPage

-------- helper functions for file retrieve ----------------
handleReqURI :: Config -> RequestURI -> FilePath
handleReqURI conf (Abs_path s) = if s == "/"
                                 then documentRoot conf </> directoryIndex conf
                                 else documentRoot conf </> checkPath s conf  -- check the validity of the request path. 
                                                                              -- if the request want to access some unauthorized file,
                                                                              -- then change it to the default file.
handleReqURI conf _ = undefined

checkPath :: FilePath -> Config -> FilePath
checkPath fp conf = if ".." `isInfixOf` fp 
                     then directoryIndex conf
                     else if hasTrailingPathSeparator fp
                           then tail fp </> directoryIndex conf
                           else tail fp

-- on ghc 6.12 we have to use openBinaryFile to read pictures etc.
-- This function can read text files as well.
getFile :: FilePath -> IO (Integer, ClockTime, String)
getFile filep = do
  tm <- getModificationTime filep
  hd <- openBinaryFile filep ReadMode
  sz <- hFileSize hd
  content <- hGetContents hd   -- semi-closed, will be closed when all read in.
  return (sz, tm, content)

getFileType :: Config -> FilePath -> String
getFileType conf fp = fromMaybe (defaultType conf) (lookup (tail $ takeExtension fp) (mimeTable conf))

removeBody :: Response -> IO Response
removeBody resp = return resp { respBody = Nothing }
