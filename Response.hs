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
module Response where

import HTTP
import Config

import Data.List (intercalate)
import System.Time (ClockTime, formatCalendarTime, toUTCTime)
import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime)
import Data.Maybe (fromMaybe)

crlf = "\r\n"

data Response = Response { httpVersion :: HTTPVersion,
                           statusCode :: StatusCode,
                           respHeaders :: [Header],
                           respBody :: Maybe String
                         }
instance Show Response where
    show Response { 
               httpVersion = hv, 
               statusCode = sc,
               respHeaders = rhdrs, 
               respBody = rb
             } = show hv ++ " " ++ fst (statusAndReason sc) ++ " " ++ snd (statusAndReason sc) ++ crlf 
                       ++ intercalate crlf (map show rhdrs) ++ crlf ++ crlf ++ respBd
        where respBd = fromMaybe "" rb

data StatusCode = SC100       -- Continue
                | SC101       -- Switching Protocols
                | SC200       -- OK
                | SC201       -- Created
                | SC202       -- Accepted
                | SC203       -- Non-Authoritative Information
                | SC204       -- No Content
                | SC205       -- Reset Content
                | SC206       -- Partial Content
                | SC300       -- Multiple Choices
                | SC301       -- Moved Permanently
                | SC302       -- Found
                | SC303       -- See Other
                | SC304       -- Not Modified
                | SC305       -- Use Proxy
                | SC307       -- Temporary Redirect
                | SC400       -- Bad Request
                | SC401       -- Unauthorized
                | SC402       -- Payment Required
                | SC403       -- Forbiden
                | SC404       -- Not Found
                | SC405       -- Method Not Allowed
                | SC406       -- Not Acceptable
                | SC407       -- Proxy Authentication Required
                | SC408       -- Request Time-out
                | SC409       -- Conflict
                | SC410       -- Gone
                | SC411       -- Length Required
                | SC412       -- Precondition Failed
                | SC413       -- Request Entity Too Large
                | SC414       -- Request-URI Too Large
                | SC415       -- Unsupported Media Type
                | SC416       -- Requested range not satisfiable
                | SC417       -- Expectation Failed
                | SC500       -- Internal Server Error
                | SC501       -- Not Implemented
                | SC502       -- Bad Gateway
                | SC503       -- Service Unavailable
                | SC504       -- Gateway Time-out
                | SC505       -- HTTP Version not supported

statusAndReason :: StatusCode -> (String,String)
statusAndReason s = case s of
                      SC100  -> ("100","Continue")
                      SC101  -> ("101","Switching Protocols")
                      SC200  -> ("200","OK")
                      SC201  -> ("201","Created")
                      SC202  -> ("202","Accepted")
                      SC203  -> ("203","Non-Authoritative Information")
                      SC204  -> ("204","No Content")
                      SC205  -> ("205","Reset Content")
                      SC206  -> ("206","Partial Content")
                      SC300  -> ("300","Multiple Choices")
                      SC301  -> ("301","Moved Permanently")
                      SC302  -> ("302","Found")
                      SC303  -> ("303","See Other")
                      SC304  -> ("304","Not Modified")
                      SC305  -> ("305","Use Proxy")
                      SC307  -> ("307","Temporary Redirect")
                      SC400  -> ("400","Bad Request")
                      SC401  -> ("401","Unauthorized")
                      SC402  -> ("402","Payment Required")
                      SC403  -> ("403","Forbiden")
                      SC404  -> ("404","Not Found")
                      SC405  -> ("405","Method Not Allowed")
                      SC406  -> ("406","Not Acceptable")
                      SC407  -> ("407","Proxy Authentication Required")
                      SC408  -> ("408","Request Time-out")
                      SC409  -> ("409","Conflict")
                      SC410  -> ("410","Gone")
                      SC411  -> ("411","Length Required")
                      SC412  -> ("412","Precondition Failed")
                      SC413  -> ("413","Request Entity Too Large")
                      SC414  -> ("414","Request-URI Too Large")
                      SC415  -> ("415","Unsupported Media Type")
                      SC416  -> ("416","Requested range not satisfiable")
                      SC417  -> ("417","Expectation Failed")
                      SC500  -> ("500","Internal Server Error")
                      SC501  -> ("501","Not Implemented")
                      SC502  -> ("502","Bad Gateway")
                      SC503  -> ("503","Service Unavailable")
                      SC504  -> ("504","Gateway Time-out")
                      SC505  -> ("505","HTTP Version not supported")

defaultResultResponse = Response { httpVersion = httpVersionSupported,
                                   statusCode = SC500,
                                   respHeaders = [serverHeader],
                                   respBody = Nothing
                                 }

requestTimeoutResponse = Response { httpVersion = httpVersionSupported,
                                    statusCode = SC408,
                                    respHeaders = [serverHeader],
                                    respBody = Nothing
                                  }
badRequestResponse = Response { httpVersion = httpVersionSupported,
                                statusCode = SC400,
                                respHeaders = [serverHeader],
                                respBody = Nothing
                              }
------- build headers ------
getDateHeader :: IO Header
getDateHeader = fmap (Date . buildRFC1123) getCurrentTime

serverHeader :: Header
serverHeader = Server $ serverSoftware ++ "/" ++ serverVersion

contentLengthHeader :: Integer -> Header
contentLengthHeader l = Content_Length $ show l

contentEncodingHeader :: String -> Header
contentEncodingHeader = Content_Encoding

contentTypeHeader :: String -> Header
contentTypeHeader = Content_Type

lastModHeader :: ClockTime -> Header
lastModHeader = Last_Modified . formatCalendarTime defaultTimeLocale rfc1123FormatString . toUTCTime

----------- html page to return ---------
fileNotFoundPage :: (String, String, Integer)
fileNotFoundPage = (body, "text/html", toInteger $ length body)
    where body = "<html><title>File Not Found</title><body><h1>File Not Found</h1></body></html>"
