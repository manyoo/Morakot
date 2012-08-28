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
module Request where

import HTTP 

import Text.ParserCombinators.Parsec
import Network.URI (URI,parseAbsoluteURI)
import Data.Maybe (fromJust)
import Data.Char (toLower)

data Method = GET
            | HEAD
            | POST
            | PUT
            | DELETE
            | TRACE
            | OPTIONS
            | CONNECT
            | Custom String
              deriving Show

parseMethod :: String -> Method
parseMethod "GET" = GET
parseMethod "HEAD" = HEAD
parseMethod "POST" = POST
parseMethod "PUT" = PUT
parseMethod "DELETE" = DELETE
parseMethod "TRACE" = TRACE
parseMethod "OPTIONS" = OPTIONS
parseMethod "CONNECT" = CONNECT
parseMethod s = Custom s

-- Request-Line = Method SP Request-URI SP HTTP-Version CRLF (defined in rfc2616)
data Request = Request {
                        reqMethod :: Method, 
                        reqURI :: RequestURI, 
                        httpVer :: HTTPVersion, 
                        reqHeaders :: [Header],
                        reqBody :: String
                       }
               deriving Show

data RequestURI = Asterisk
                | AbsoluteURI URI   -- used on proxy, but rfc said all servers MUST accept this form.
                | Abs_path String
                | Authority String  -- used only by CONNECT method

instance Show RequestURI where
    show Asterisk = "*"
    show (AbsoluteURI u) = show u
    show (Abs_path s) = s
    show (Authority s) = s

parseURI :: String -> Maybe RequestURI
parseURI "*" = Just Asterisk
parseURI (uri@('/':_)) = Just $ Abs_path uri
parseURI uri = Just $ AbsoluteURI $ fromJust $ parseAbsoluteURI uri

parseVersion :: String -> Maybe HTTPVersion
parseVersion s =
    case parse versionParser "Version Parser" s of
      Right result -> Just result
      Left error -> Nothing

-- a very simple version parser, used only in the parseVersion function.
versionParser :: Parser HTTPVersion
versionParser = do
  string "HTTP/"
  major <- many1 digit
  char '.'
  minor <- many1 digit
  return $ Ver (read major :: Int) (read minor :: Int)

parseRequest :: [String] -> Request
parseRequest reqStrList = Request {reqMethod = rMethod,
                                   reqURI = rURI,
                                   httpVer = hVersion,
                                   reqHeaders = hd,
                                   reqBody = ""
                                  }
    where (reqLine, headers) | length reqStrList == 1 = (init $ head reqStrList, [])
                             | length reqStrList > 1 = (init $ head reqStrList, map init $ tail reqStrList)
          mthdStr:uriStr:verStr:xs = words reqLine
          rMethod = parseMethod mthdStr
          rURI = fromJust $ parseURI uriStr
          hVersion = fromJust $ parseVersion verStr
          hd = parseHeaders headers

parseHeaders :: [String] -> [Header]
parseHeaders = map parseHeader
    where parseHeader :: String -> Header
          parseHeader s = fromJust $ buildHeader hname $ dropHeadSpace $ tail hval
              where (hname, hval) = break (== ':') s
                    dropHeadSpace [] = []
                    dropHeadSpace (' ':xs) = dropHeadSpace xs
                    dropHeadSpace s = s

buildHeader :: String -> String -> Maybe Header
buildHeader hName hVal =
    case (map toLower hName) of
      "cache-control" -> valStr Cache_Control
      "connection" -> valStr Connection
      "date" -> valStr Date
      "pragma" -> valStr Pragma
      "trailer" -> valStr Trailer
      "transfer-encoding" -> valStr Transfer_Encoding
      "upgrade" -> valStr Upgrade
      "via" -> valStr Via
      "warning" -> valStr Warning
      "accept" -> valStr Accept
      "accept-charset" -> valStr Accept_Charset
      "accept-encoding" -> valStr Accept_Encoding
      "accept-language" -> valStr Accept_Language
      "authorization" -> valStr Authorization
      "expect" -> valStr Expect
      "from" -> valStr From
      "host" -> valStr Host
      "if-match" -> valStr If_Match
      "if-modified-since" -> valStr If_Modified_Since
      "if-non-match" -> valStr If_Non_Match
      "if-range" -> valStr If_Range
      "if-unmodified-since" -> valStr If_Unmodified_Since
      "max-forwords" -> valStr Max_Forwards
      "proxy-authorization" -> valStr Proxy_Authorization
      "range" -> valStr Range
      "referer" -> valStr Referer
      "te" -> valStr TE
      "user-agent" -> valStr User_Agent
      "accept-ranges" -> valStr Accept_Ranges
      "age" -> valStr Age
      "etag" -> valStr ETag
      "location" -> valStr Location
      "proxy-authenticate" -> valStr Proxy_Authenticate
      "retry-after" -> valStr Retry_After
      "server" -> valStr Server
      "vary" -> valStr Vary
      "www-authenticate" -> valStr WWW_Authenticate
      "allow" -> valStr Allow
      "content-encoding" -> valStr Content_Encoding
      "content-language" -> valStr Content_Language
      "content-length" -> valStr Content_Length
      "content-location" -> valStr Content_Location
      "content-md5" -> valStr Content_MD5
      "content-range" -> valStr Content_Range
      "content-type" -> valStr Content_Type
      "expires" -> valStr Expires
      "last-modified" -> valStr Last_Modified
      s -> valStr $ Extension_Header s
    where valStr constr = Just $ constr $ map toLower hVal

