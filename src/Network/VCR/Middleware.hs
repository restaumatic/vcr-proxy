{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.VCR.Middleware where

import Control.Monad (when)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Text.Encoding as TE

import Data.Yaml
  ( decodeFileEither
  , encode
  , encodeFile
  )
import qualified Network.HTTP.Types as HT
import Network.VCR.Types
  ( ApiCall (..)
  , Body (..)
  , Cassette (..)
  , Mode (..)
  , ReplayError (..)
  , SavedRequest (..)
  , SavedResponse (..)
  , VCRResponse (..)
  , bodyToLBS
  , emptyCassette
  , lbsToBody
  )
import qualified Network.Wai as Wai

import qualified Data.Text.Encoding as BE (encodeUtf8)

import qualified Codec.Compression.GZip as GZip
import qualified Data.Aeson as A
import qualified Data.Aeson.Diff as A
import qualified Data.Aeson.Encode.Pretty as A

import Data.CaseInsensitive (CI, mk)
import Data.IORef (atomicModifyIORef')
import qualified System.Exit as XIO
import System.IO (stderr)
import qualified System.IO (hPutStrLn)
import qualified URI.ByteString as URI

middleware :: Mode -> IORef Cassette -> Wai.Middleware
middleware Replay = replayingMiddleware findAnyResponse
middleware ReplayStrict = replayingMiddleware consumeRequestsInOrder
middleware Record {endpoint} = recordingMiddleware endpoint

{- | Middleware which only records API calls, the requests are proxied to the `endpoint` and recorded in the
 `filePath` cassette file
-}
recordingMiddleware :: String -> IORef Cassette -> Wai.Middleware
recordingMiddleware endpoint cassetteIORef app req respond = do
  Cassette {ignoredHeaders} <- readIORef cassetteIORef
  (req', body) <- getRequestBody req
  -- Construct a request that can be sent to the actual remote API, by replacing the host in the request with the endpoint
  -- passed as an argument to the middleware
  let newRequest = modifyEndpoint (T.pack endpoint) req'
  -- delegate to http-proxy app
  app newRequest $ \response -> do
    -- Save the request that we have received from the remote API
    let savedRequest = buildRequest ignoredHeaders req' (LBS.fromChunks body)
    -- Since reading the response body consumes it, we can't just reuse the response
    (status, headers, reBody) <- getResponseBody response
    savedResponse <- buildResponse reBody response
    -- Store the request, response pair
    atomicModifyIORef' cassetteIORef $ \cassette -> (cassette { apiCalls = apiCalls cassette <> [ApiCall {request = savedRequest, response = savedResponse}] }, ())
    respond $ Wai.responseLBS status headers (gzipIfNeeded headers reBody)

-- | A policy for obtaining responses given a request.
type FindResponse = IORef Cassette -> SavedRequest -> IO (Either ReplayError SavedResponse)

findAnyResponse :: FindResponse
findAnyResponse cassetteIORef savedRequest = do
  Cassette {apiCalls, ignoredHeaders} <- readIORef cassetteIORef
  pure $
    fmap response $
      note RequestNotRecorded $
        find (\c -> request c == savedRequest) apiCalls

-- | A policy for obtaining response which expects the request to be issued in the order they were recorded.
consumeRequestsInOrder :: FindResponse
consumeRequestsInOrder cassetteIORef savedRequest = do
  cassette@Cassette {apiCalls, ignoredHeaders} <- readIORef cassetteIORef
  case apiCalls of
    c : rest -> do
      if request c == savedRequest
        then do
          writeIORef cassetteIORef $ cassette {apiCalls = rest}
          pure $ Right $ response c
        else pure $ Left $ ExpectedDifferentRequest (request c)
    [] ->
      pure $ Left RequestNotRecorded

{- | Middleware which only replays API calls, if a request is not found in the filePath provided cassette file,
 a 500 error will be thrown
-}
replayingMiddleware :: FindResponse -> IORef Cassette -> Wai.Middleware
replayingMiddleware findResponse cassetteIORef app req respond = do
  cassette@Cassette {apiCalls, ignoredHeaders} <- readIORef cassetteIORef
  b <- Wai.strictRequestBody req
  let receivedRequest = buildRequest ignoredHeaders req b
  -- Find an existing ApiCall according to the FindResponse policy
  findResponse cassetteIORef receivedRequest >>= \case
    -- if a request is found, respond with the saved response
    Right res -> do
      let gzippedBody = gzipIfNeeded (headers (res :: SavedResponse)) (bodyToLBS $ body (res :: SavedResponse))
      respond $ Wai.responseLBS (status res) (headers (res :: SavedResponse)) gzippedBody
    -- if the request was not recorded, return an error
    Left err -> do
      let (msg, savedRequest) = case err of
            RequestNotRecorded -> ("Request not recorded", Nothing)
            ExpectedDifferentRequest expected -> ("Expected a different request", Just expected)
          diff = A.diff (A.toJSON receivedRequest) . A.toJSON <$> savedRequest
          response = VCRResponse {_error = msg, _receivedRequest = receivedRequest, _expectedRequest = savedRequest, _diff = diff}
      putTextStdErrLn $
        "\ESC[31mVCRProxy error: "
          <> msg
          <> "\n\ESC[0mExpected request (only in strict replay mode): \n\ESC[34m"
          <> jsonToText (A.toJSON savedRequest)
          <> "\n\ESC[0mReceived request: \n"
          <> jsonToText (A.toJSON receivedRequest)
          <> "\n\ESC[33mDiff: \n"
          <> jsonToText (A.toJSON diff)
      respond $ Wai.responseLBS HT.status500 [] (A.encode response)

-- | Modify parts of the request so that it can be sent to the remote API while being received on a localhost server
modifyEndpoint :: Text -> Wai.Request -> Wai.Request
modifyEndpoint endpoint req =
  req
    { Wai.requestHeaderHost = Just host
    , Wai.rawPathInfo = modifyPath $ Wai.rawPathInfo req
    , Wai.requestHeaders = mapMaybe modifyHeader (Wai.requestHeaders req) <> [("accept-encoding", "identity")]
    }
  where
    endpoint' = TE.encodeUtf8 endpoint
    uri = either endpointError id $ URI.parseURI URI.strictURIParserOptions endpoint'
    (host', port) = maybe noHostError (\a -> (URI.hostBS $ URI.authorityHost a, URI.portNumber <$> URI.authorityPort a)) (URI.uriAuthority uri)
    host = host' <> maybe "" (\p -> TE.encodeUtf8 $ ":" <> tshow p) port
    scheme = URI.schemeBS . URI.uriScheme $ uri
    modifyHeader h@(key, value)
      | key == mk "host" = Just (key, host)
      | key == mk "accept-encoding" = Nothing
      | otherwise = Just h
    modifyPath = BS.append scheme . BS.append "://" . BS.append host
    endpointError e = error $ "Error parsing endpoint as URI, " <> show e
    noHostError = error "No host could be extracted from the endpoint"

buildRequest :: [Text] -> Wai.Request -> LBS.ByteString -> SavedRequest
buildRequest ignoredHeaders r body =
  SavedRequest
    { methodName = TE.decodeUtf8 $ Wai.requestMethod r
    , headers = reqHeaders
    , url = TE.decodeUtf8 $ Wai.rawPathInfo r
    , params = Wai.queryString r
    , body = lbsToBody body
    }
  where
    reqHeaders = filter (\(key, value) -> key `elem` ignoredHeaders') (Wai.requestHeaders r)
    ignoredHeaders' = mk . BE.encodeUtf8 <$> ignoredHeaders

buildResponse :: LBS.ByteString -> Wai.Response -> IO SavedResponse
buildResponse body response = do
  pure $
    SavedResponse
      { body = lbsToBody body
      , headers = Wai.responseHeaders response
      , status = Wai.responseStatus response
      }

getResponseBody :: Wai.Response -> IO (HT.Status, HT.ResponseHeaders, LBS.ByteString)
getResponseBody res =
  let (status, headers, body) = Wai.responseToStream res
   in body $ \f -> do
        content <- newIORef mempty
        f (\chunk -> modifyIORef' content (<> chunk)) (return ())
        b <- toLazyByteString <$> readIORef content
        pure (status, headers, b)

-- Taken from: https://github.com/yesodweb/wai/blob/master/wai-extra/Network/Wai/Middleware/RequestLogger.hs#L220
getRequestBody :: Wai.Request -> IO (Wai.Request, [BS.ByteString])
getRequestBody req = do
  let loop front = do
        bs <- Wai.requestBody req
        if BS.null bs
          then return $ front []
          else loop $ front . (bs :)
  body <- loop id
  -- Since reading the body consumes it, we need to refill it.
  -- This implementation ensures that each chunk is only returned
  -- once.
  ichunks <- newIORef body
  let rbody = atomicModifyIORef' ichunks $ \chunks ->
        case chunks of
          [] -> ([], BS.empty)
          x : y -> (y, x)
  -- This triggers a deprecation warning, that makes no sense in this case, since there is no other way to reset the
  -- request body
  let req' = req {Wai.requestBody = rbody}
  return (req', body)

die :: String -> IO a
die err = (System.IO.hPutStrLn stderr err) >> XIO.exitFailure

gzipIfNeeded :: [HT.Header] -> LBS.ByteString -> LBS.ByteString
gzipIfNeeded headers body | needsCompression headers = GZip.compress body
gzipIfNeeded _ body = body

needsCompression :: [HT.Header] -> Bool
needsCompression headers =
  case lookup ("Content-Encoding" :: CI BS.ByteString) headers of
    Just "gzip" -> True
    _ -> False

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

note :: e -> Maybe a -> Either e a
note err = maybe (Left err) Right

tshow :: Show a => a -> Text
tshow = T.pack . show

putTextStdErrLn :: Text -> IO ()
putTextStdErrLn = System.IO.hPutStrLn stderr . T.unpack

lsbToText :: LBS.ByteString -> T.Text
lsbToText = TE.decodeUtf8 . LBS.toStrict

jsonToText :: A.Value -> T.Text
jsonToText = lsbToText . A.encodePretty
