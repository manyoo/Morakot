module Parser where

import qualified Text.ParserCombinators.Parsec as P
import HTTP
import System.Locale (defaultTimeLocale)
import Data.Time (UTCTime, parseTime, formatTime, getCurrentTime)
import Numeric (readHex)

char :: P.Parser Char
char = P.anyChar

upalpha = P.upper
loalpha = P.lower

alpha = upalpha <!> loalpha

digit = P.digit

cr = P.char '\r'
lf = P.char '\n'
sp = P.space
ht = P.tab
quote = P.char '*'

crlf :: Parser ()
crlf = do cr
          lf

ctlString = "\0\SOH\STX\ETX\EOT\ENQ\ACK\BEL\BS\HT\LF\VT\FF\CR\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US\DEL"

ctl = P.oneOf ctlString

lws :: Parser Char  -- replace linear white space with a single space
lws = do 
  P.skipMany crlf
  P.skipMany1 (sp <|> ht)
  return ' '

text = P.many (P.noneOf ctlString)

hex = P.hexdigit

separators = P.oneOf "()<>@,;:\\\"/[]?={} \t"

token = P.many1 (P.noneOf ctlString <|> separators)

comment = do
  P.char '('
  res <- P.many (ctext <|> quoted_pair <|> comment)
  P.char ')'
  return res

ctext = P.many (P.noneOf "()")

quoted_string = do
  P.char '"'
  res <- P.many (qdtext <|> quoted_pair)
  P.char '"'
  return res

qdtext = P.many (P.noneOf "\"")

quoted_pair = do
  P.char '\\'
  c <- char
  return c


-- TIME PARSER
rfc1123FormatString = "%a, %d %b %Y %T GMT"
rfc850FormatString = "%A, %d-%b-%y %T GMT"
asctimeFormatString = "%a %b %d %T %Y"

parseRFC1123 :: String -> Maybe UTCTime
parseRFC1123 = parseTime defaultTimeLocale rfc1123FormatString

parseRFC850 = parseTime defaultTimeLocale rfc850FormatString

parseASCTime = parseTime defaultTimeLocale asctimeFormatString

-- we MUST only generate RFC1123 format time.
buildRFC1123 :: UTCTime -> String
buildRFC1123 = formatTime defaultTimeLocale rfc1123FormatString


-- charactor set
charset = token

content_coding = token

transfer_coding = P.string "chunked" <|> transfer_extension

transfer_extension = do
  t <- token
  p <- P.many parameter
  return (t,p)

parameter = do
  att <- token
  P.char '='
  val <- token <|> quoted_string
  return (att,val)

chunked_body = do
  chk <- P.many chunk
  lch <- last_chunk
  trl <- trailer
  crlf
  return $ ChunkedBody {chunk=chk, lastChunk=lch, trailer=trl}

chunk = do
  size <- chunk_size
  chkExt <- P.many chunk_extension
  crlf
  dat <- chunk_data
  crlf
  return $ Chunk {chunkSize=size, chunkExtension=chkExt, chunkData=dat}

chunk_size :: Parser Int
chunk_size =do
  h<- P.many1 hex
  let res = readHex h
  return $ fst $ head res

last_chunk = do
  zero <- P.many1 (P.char '0')
  ext <- P.many chunk_extension
  crlf
  return ext              --------------- undefined here

chunk_extension = do
  P.char ';'
  name <- token
  P.option (name,"") (do
                       P.char '='
                       val <- token <|> quoted_string
                       return (name,val))
                    
chunk_data size = P.count size char

trailer = P.many entity_header

media_type = do
  tp <- token
  P.char '/'
  subtp <- token
  para <- P.many parameter
  return $ MediaType{mType = tp, subtype = subtp, parameter = para}

product = do
  p <- token
  P.option (Product {name = p, version = ""} (do
                                               P.char '/'
                                               ver <- token
                                               return $ Product {name = p, version = ver}
                                             )
