-- -----------------------------------------------------------------------------
-- Copyright 2010, Eric Wong.
-- Copyright 2002, Simon Marlow.
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
module Config where

import Data.Map (Map,empty)
import Control.Exception (throw)

-- Thanks to Simon Marlow's HWS, I know what should be configurable for a server.
-- But I don't support all of these right now. 
data Config = Config {
      user                  :: String,
      group                 :: String,
      
      port                  :: Int,
  
      requestTimeout        :: Int,
      keepAliveTimeout      :: Int,
      maxClients            :: Int,
                               
      serverAdmin           :: String,      -- "" indicates no admin
      serverName            :: String,      -- "" indicates no canon name
      serverAlias           :: [String],
      useCanonicalName      :: Bool,
      hostnameLookups       :: Bool,
  
      documentRoot          :: String,
      userDir               :: String,
      directoryIndex        :: String,
      accessFileName        :: String,
      indexes               :: Bool,
      followSymLinks        :: Bool,
                               
      typesConfig           :: String,
      defaultType           :: String,
      mimeTable             :: Map String String,  -- the MIME table generated from 'typesConfig' file on-the-fly. 
                                                   -- Not used for configuration. I use this to make the table available
                                                   -- to all threads.
      addLanguage           :: [(String,String)],
      languagePriority      :: [String],
                               
      logFile               :: String,
      accessLogFormat       :: String,
                               
      errorLogFile          :: String,
      logLevel              :: Int
    }
              deriving Show

-- terms we can configure in the configuration file.
data ConfigTerm = CTPort Int
                | CTReqTimeout Int
                | CTKeepAlive Int
                | CTMaxClients Int
                | CTServerName String
                | CTHostnameLookups Bool
                | CTDocRoot String
                | CTDirIndex String
                | CTTypesConfig String
                | CTDefaultType String
                | CTLogFile String

defaultConfig :: Config
defaultConfig = Config{
                  user = "nobody",
                  group = "nobody",
                          
                  port = 8000,
                           
                  requestTimeout        = 300,
                  keepAliveTimeout      = 300,
                  maxClients            = 150,
  
                  serverAdmin           = "",
                  serverName            = "",
                  serverAlias           = [],
                  useCanonicalName      = False,
                  hostnameLookups       = False,
  
                  documentRoot          = "/home/eric/learning/docs.huihoo.com/homepage/shredderyin",
                  userDir               = "",
                  directoryIndex        = "index.html",
                  accessFileName        = ".htaccess",
                  indexes               = False,
                  followSymLinks        = False,
  
                  typesConfig           = "/etc/mime.types",
                  defaultType           = "text/plain",
                  mimeTable             = empty,
  
                  addLanguage           = [],
                  languagePriority      = [],

                  logFile               = "morakot.log",
                  accessLogFormat       = "%h %l %u %t \"%r\" %s %b \"%{Referer}i\" \"%{User-Agent}i\"",

                  errorLogFile          = "httpd-error.log",
                  logLevel              = 1
                }

-- not user-definable...
serverSoftware       = "Morakot"
serverVersion        = "0.1"

-------------- Configuration file parser ---------------------
parseConfigFile :: String -> Config -> Config
parseConfigFile inString conf = let noNullStrs = filter ("" /=) $ lines inString
                                    usefulStrs = filter (('#' /=) . head) noNullStrs
                                    termList = map (parseTerm . break ('=' ==)) usefulStrs
                                in foldl changeTerm conf termList

parseTerm :: (String, String) -> ConfigTerm
parseTerm (termStr, valStr) = case stripSpace termStr of
                                "Port" -> CTPort $ read val
                                "Request-Timeout" -> CTReqTimeout $ read val
                                "Keep-Alive-Timeout" -> CTKeepAlive $ read val
                                "Max-Clients" -> CTMaxClients $ read val
                                "Server-Name" -> CTServerName val
                                "Hostname-Lookups" -> CTHostnameLookups $ strToBool val
                                "Document-Root" -> CTDocRoot val
                                "Directory-Index" -> CTDirIndex val
                                "Types-Config" -> CTTypesConfig val
                                "Default-Type" -> CTDefaultType val
                                "Log-File" -> CTLogFile val
                                _ -> error "Configuration Term Unsupported"
    where stripSpace [] = []
          stripSpace (' ':xs) = stripSpace xs
          stripSpace (a:xs) = a:stripSpace xs
          val = stripSpace $ tail valStr
          strToBool "0" = False
          strToBool "1" = True

changeTerm :: Config -> ConfigTerm -> Config
changeTerm conf (CTPort v) = conf { port = v }
changeTerm conf (CTReqTimeout v) = conf { requestTimeout = v }
changeTerm conf (CTKeepAlive v) = conf { keepAliveTimeout = v }
changeTerm conf (CTMaxClients v) = conf { maxClients = v }
changeTerm conf (CTServerName v) = conf { serverName = v }
changeTerm conf (CTHostnameLookups v) = conf { hostnameLookups = v }
changeTerm conf (CTDocRoot v) = conf { documentRoot = v }
changeTerm conf (CTDirIndex v) = conf { directoryIndex = v }
changeTerm conf (CTTypesConfig v) = conf { typesConfig = v }
changeTerm conf (CTDefaultType v) = conf { defaultType = v }
changeTerm conf (CTLogFile v) = conf { logFile = v }
