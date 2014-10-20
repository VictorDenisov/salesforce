{-# LANGUAGE OverloadedStrings #-}

module Salesforce.HTTP
  ( showRequest
  , showResponse
  ) where

import Control.Monad (when, unless)
import Control.Monad.Writer (MonadWriter(..), execWriter)
import Data.ByteString.Lazy (ByteString)
import Data.CaseInsensitive (original)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import Network.HTTP.Client (getUri)
import Network.HTTP.Conduit as HTTP
  ( Request(..), method, requestHeaders, requestBody
  , RequestBody(..)
  , Response(..), responseBody
  )
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (Status(..))


showRequest :: Request -> Text
showRequest x = T.concat . execWriter $ do
  tell [ "Request {", TE.decodeUtf8 $ method x, " ", T.pack $ show (getUri x) ]

  let headers = requestHeaders x
  when (headers /= []) $
    tell [ " headers=[", T.intercalate ", " $ map showHeader headers, "]" ]

  let body = requestBody x
  unless (requestBodyIsEmpty body) $
    tell [ " body=", showRequestBody body ]

  tell [ "}" ]

  where requestBodyIsEmpty :: RequestBody -> Bool
        requestBodyIsEmpty (RequestBodyLBS b) = LB8.length b == 0
        requestBodyIsEmpty (RequestBodyBS b) = B8.length b == 0
        requestBodyIsEmpty (RequestBodyBuilder b _) = b == 0
        requestBodyIsEmpty (RequestBodyStream b _) = b == 0
        requestBodyIsEmpty _ = False

        showRequestBody :: RequestBody -> Text
        showRequestBody (RequestBodyLBS b) = LT.toStrict $ LTE.decodeUtf8 b
        showRequestBody (RequestBodyBS b) = TE.decodeUtf8 b
        showRequestBody (RequestBodyBuilder _ _) = "<builder>"
        showRequestBody (RequestBodyStream _ _) = "<stream>"
        showRequestBody (RequestBodyStreamChunked _) = "<chunked>"


showResponse :: Response ByteString -> Text
showResponse x = T.concat . execWriter $ do
  tell [ "Response {", T.pack . show . statusCode $ responseStatus x ]

  let headers = responseHeaders x
  when (headers /= []) $
    tell [ " headers=[", T.intercalate ", " $ map showHeader headers, "]" ]

  let body = responseBody x
  when (body /= "") $
    tell [ " body=", LT.toStrict $ LTE.decodeUtf8 body ]

  tell [ "}" ]

showHeader :: Header -> Text
showHeader h = T.concat [ TE.decodeUtf8 $ original $ fst h, ": ", TE.decodeUtf8 $ snd h ]
