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
module HTTP where

{- Define the HTTP data structures according to RFC 2616 -}
import System.Locale (defaultTimeLocale)
import Data.Time (UTCTime, parseTime, formatTime, getCurrentTime)

data HTTPVersion = Ver Int Int
                   deriving Eq

instance Show HTTPVersion where
    show (Ver major minor) = "HTTP/" ++ show major ++ "." ++ show minor

instance Ord HTTPVersion where
    Ver ma1 mi1 <= Ver ma2 mi2 = (ma1 < ma2) || (mi1 <= mi2)

-- the Http version we support.
httpVersionSupported :: HTTPVersion
httpVersionSupported = Ver 1 1

-- at this early stage, we don't dive into the header mess too much,
-- just make them strings, and don't care about them!
data Header = Cache_Control String  -- general headers --
            | Connection String
            | Date String
            | Pragma String
            | Trailer String
            | Transfer_Encoding String
            | Upgrade String
            | Via String
            | Warning String
            -- request headers --
            | Accept String
            | Accept_Charset String
            | Accept_Encoding String
            | Accept_Language String
            | Authorization String
            | Expect String
            | From String
            | Host String
            | If_Match String
            | If_Modified_Since String
            | If_Non_Match String
            | If_Range String
            | If_Unmodified_Since String
            | Max_Forwards String
            | Proxy_Authorization String
            | Range String
            | Referer String
            | TE String
            | User_Agent String
            -- response headers --
            | Accept_Ranges String
            | Age String
            | ETag String
            | Location String
            | Proxy_Authenticate String
            | Retry_After String
            | Server String
            | Vary String
            | WWW_Authenticate String
            -- entity headers -- 
            | Allow String
            | Content_Encoding String
            | Content_Language String
            | Content_Length String
            | Content_Location String
            | Content_MD5 String
            | Content_Range String
            | Content_Type String
            | Expires String
            | Last_Modified String
            | Extension_Header String String
              deriving Eq

instance Show Header where
    show (Cache_Control s) = "Cache-Control: " ++ s
    show (Connection s) = "Connection: " ++ s
    show (Date s) = "Date: " ++ s
    show (Pragma s) = "Pragma: " ++ s
    show (Trailer s) = "Trailer: " ++ s
    show (Transfer_Encoding s) = "Transfer-Encoding: " ++ s
    show (Upgrade s) = "Upgrade: " ++ s
    show (Via s) = "Via: " ++ s
    show (Warning s) = "Warning: " ++ s
    show (Accept s) = "Accept: " ++ s
    show (Accept_Charset s) = "Accept-Charset: " ++ s
    show (Accept_Encoding s) = "Accept-Encoding: " ++ s
    show (Accept_Language s) = "Accept-Language: " ++ s
    show (Authorization s) = "Authorization: " ++ s
    show (Expect s) = "Expect: " ++ s
    show (From s) = "From: " ++ s
    show (Host s) = "Host: " ++ s
    show (If_Match s) = "If-Match: " ++ s
    show (If_Modified_Since s) = "If-Modified-Since: " ++ s
    show (If_Non_Match s) = "If-Non-Match: " ++ s
    show (If_Range s) = "If-Range: " ++ s
    show (If_Unmodified_Since s) = "If-Unmodified-Since: " ++ s
    show (Max_Forwards s) = "Max-Forwards: " ++ s
    show (Proxy_Authorization s) = "Proxy-Authorization: " ++ s
    show (Range s) = "Range: " ++ s
    show (Referer s) = "Referer: " ++ s
    show (TE s) = "TE: " ++ s
    show (User_Agent s) = "User-Agent: " ++ s
    show (Accept_Ranges s) = "Accept-Ranges: " ++ s
    show (Age s) = "Age: " ++ s
    show (ETag s) = "ETag: " ++ s
    show (Location s) = "Location: " ++ s
    show (Proxy_Authenticate s) = "Proxy-Authenticate: " ++ s
    show (Retry_After s) = "Retry-After: " ++ s
    show (Server s) = "Server: " ++ s
    show (Vary s) = "Vary: " ++ s
    show (WWW_Authenticate s) = "WWW-Authenticate: " ++ s
    show (Allow s) = "Allow: " ++ s
    show (Content_Encoding s) = "Content-Encoding: " ++ s
    show (Content_Language s) = "Content-Language: " ++ s
    show (Content_Length s) = "Content-Length: " ++ s
    show (Content_Location s) = "Content-Location: " ++ s
    show (Content_MD5 s) = "Content-MD5: " ++ s
    show (Content_Range s) = "Content-Range: " ++ s
    show (Content_Type s) = "Content-Type: " ++ s
    show (Expires s) = "Expires: " ++ s
    show (Last_Modified s) = "Last-Modified: " ++ s
    show (Extension_Header s v) = "Extension-Header: " ++ s ++ ": " ++ v


-- TIME PARSER
rfc1123FormatString = "%a, %d %b %Y %T GMT"
rfc850FormatString = "%A, %d-%b-%y %T GMT"
asctimeFormatString = "%a %b %d %T %Y"

parseRFC1123 :: String -> Maybe UTCTime
parseRFC1123 = parseTime defaultTimeLocale rfc1123FormatString
parseRFC850 :: String -> Maybe UTCTime
parseRFC850 = parseTime defaultTimeLocale rfc850FormatString
parseASCTime :: String -> Maybe UTCTime
parseASCTime = parseTime defaultTimeLocale asctimeFormatString

-- we MUST only generate RFC1123 format time.
buildRFC1123 :: UTCTime -> String
buildRFC1123 = formatTime defaultTimeLocale rfc1123FormatString
