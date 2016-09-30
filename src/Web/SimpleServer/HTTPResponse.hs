module Web.SimpleServer.HTTPResponse (
      HTTPResponse(..)

    -- Response codes
    , http_OK, http_REDIRECT, http_BADREQUEST
    , http_NOTFOUND, http_SERVERERROR

    -- Simple generators
    , emptyResponse
    , okMessage
    , redirect
    , badRequest

    , setHeader

    , format
) where

import Data.List       (intercalate)
import Data.Map.Strict hiding (map)

data HTTPResponse = HTTPResponse
    { code :: Int
    , headers :: Map String String
    , message :: String
    } deriving (Eq, Show)

autoHeaders :: [(String, HTTPResponse -> String)]
autoHeaders = [ ("Content-Length", contentLength)
              ]

contentLength :: HTTPResponse -> String
contentLength = show . length . message

http_OK, http_REDIRECT, http_BADREQUEST, http_NOTFOUND, http_SERVERERROR :: Int
http_OK = 200
http_REDIRECT = 308
http_BADREQUEST = 400
http_NOTFOUND = 404
http_SERVERERROR = 500

emptyResponse :: HTTPResponse
emptyResponse = okMessage ""

okMessage, redirect, badRequest :: String -> HTTPResponse
okMessage = HTTPResponse http_OK empty
redirect = HTTPResponse http_REDIRECT empty
badRequest = HTTPResponse http_BADREQUEST empty

setAutoHeaders :: HTTPResponse -> HTTPResponse
setAutoHeaders hr@(HTTPResponse c headers m) = HTTPResponse c headers' m
    where
        autoHeaderMap = fromList [(k, f hr) | (k, f) <- autoHeaders]
        headers'      = union autoHeaderMap headers

setHeader :: HTTPResponse -> String -> String -> HTTPResponse
setHeader (HTTPResponse c headers m) = \h val -> HTTPResponse c (insert h val headers) m

format :: HTTPResponse -> String
format hr = firstLine ++
            crlf ++
            headerLines ++
            crlf ++
            crlf ++
            message hr
    where
        crlf        = "\r\n"
        firstLine   = "HTTP/1.0 " ++ (show . code) hr
        headers'    = map ((intercalate ": ") . \(x, y) -> [x, y]) $ toList (headers hr)
        headerLines = intercalate crlf headers'

