module Web.SimpleServer.HTTPRequest (
      HTTPRequest(..)
    , getHttpRequest
) where

import Control.Applicative
import Control.Monad                 (liftM4)
import Data.Map.Strict               hiding (map)
import Numeric                       (readHex)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import System.IO                     (Handle)

data Method = Get | Post
          deriving (Eq, Ord, Show)

data HTTPRequest = HTTPRequest {
      reqMethod :: Method
    , reqURL :: String
    , reqHeaders :: Map String String
    , reqBody :: Maybe String
    } deriving (Eq, Show)


-- from the Haskell O'Reilly book. Huh!
p_request :: CharParser () HTTPRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
  where q name ctor body = liftM4 HTTPRequest req url p_headers body
            where req = ctor <$ string name <* char ' '
        url = optional (char '/') *>
              manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
              <* crlf

p_headers :: CharParser st (Map String String)
p_headers = fromList <$> (header `manyTill` crlf)
  where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        contents = liftA2 (++) (many1 notEOL <* crlf)
                               (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
        fieldName = (:) <$> letter <*> many fieldChar
        fieldChar = letter <|> digit <|> oneOf "-_"

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

getHttpRequest :: String -> Either ParseError HTTPRequest
getHttpRequest = parse p_request ""
