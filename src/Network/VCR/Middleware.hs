{-# LANGUAGE NamedFieldPuns #-}
module Network.VCR.Middleware where

import           Control.Monad              (when)
import           Data.ByteString.Builder    (toLazyByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.IORef                 (modifyIORef', newIORef, readIORef)
import           Data.List                  (find)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Data.Text.Encoding         as TE

import           Data.Yaml                  (decodeFileEither, encodeFile)
import qualified Network.HTTP.Types         as HT
import           Network.VCR.Types          (ApiCall (..), Cassette (..),
                                             Mode (..), SavedRequest (..),
                                             SavedResponse (..), emptyCassette)
import qualified Network.Wai                as Wai
import qualified Network.Wai.Internal       as Wai (getRequestBodyChunk)

import           Data.CaseInsensitive       (mk)
import qualified Data.Text.Encoding         as BE (encodeUtf8)

import           Data.IORef                 (atomicModifyIORef)
import           System.Directory           (doesFileExist)
import qualified System.Exit                as XIO
import           System.IO                  (stderr)
import qualified System.IO                  (hPutStrLn)
import qualified URI.ByteString             as URI

middleware :: Mode -> FilePath -> Wai.Middleware
middleware Replay              = replayingMiddleware
middleware Record { endpoint } = recordingMiddleware endpoint


-- | Middleware which only records API calls, the requests are proxied to the `endpoint` and recorded in the
-- `filePath` cassette file
recordingMiddleware :: String -> FilePath -> Wai.Middleware
recordingMiddleware endpoint filePath app req respond = do
  exists <- doesFileExist filePath
  when (not exists) $ encodeFile filePath emptyCassette
  cas <- decodeFileEither filePath
  (req', body) <- getRequestBody req
  -- Construct a request that can be sent to the actual remote API, by replacing the host in the request with the endpoint
  -- passed as an argument to the middleware
  let newRequest = (modifyEndpoint (T.pack endpoint) req')
  -- create file if it doesn't exist
  case cas of
    Left  err -> die $ "Cassette: " <> filePath <> " couldn't be decoded or found! " <> (show err)
    Right cassette@Cassette { apiCalls, ignoredHeaders } ->
      -- delegate to http-proxy app
      app newRequest $ \response -> do
        -- Save the request that we have received from the remote API
        let savedRequest = buildRequest ignoredHeaders req' (LBS.fromChunks body)
        -- Since reading the response body consumes it, we can't just reuse the response
        (status, headers, reBody) <- getResponseBody response
        savedResponse <- buildResponse reBody response
        -- Store the request, response pair
        encodeFile filePath $ cassette
          { apiCalls = apiCalls <> [ApiCall { request = savedRequest, response = savedResponse }] }
        respond $ Wai.responseLBS status headers reBody


-- | Middleware which only replays API calls, if a request is not found in the filePath provided cassette file,
-- a 500 error will be thrown
replayingMiddleware :: FilePath -> Wai.Middleware
replayingMiddleware filePath app req respond = do
  cas <- decodeFileEither filePath
  case cas of
    Left  err -> die $ "Cassette: " <> filePath <> " couldn't be decoded or found! " <> (show err)
    Right cassette@Cassette { apiCalls, ignoredHeaders } -> do
      b <- Wai.strictRequestBody req
      let savedRequest = buildRequest ignoredHeaders req b
      -- Find an existing ApiCall with a simmilar request (up to ignored headers)
      case find (\c -> request c == savedRequest) apiCalls of
        -- if a request is found, respond with the saved response
        Just c ->
          let
            res = (response c)
          in
            respond $ Wai.responseLBS (status res) (headers (res :: SavedResponse)) (body (res :: SavedResponse))
        -- if the request was not recorded, return an error
        Nothing -> do
          respond $ Wai.responseLBS HT.status500
              { HT.statusMessage =
                  TE.encodeUtf8 . T.pack $
                    "The request: " <> show savedRequest <> " is not recorded! Ignored headers: " <> show ignoredHeaders } [] ""


-- | Modify parts of the request so that it can be sent to the remote API while being received on a localhost server
modifyEndpoint :: Text -> Wai.Request -> Wai.Request
modifyEndpoint endpoint req = req
  { Wai.requestHeaderHost = Just host
  , Wai.rawPathInfo       = modifyPath $ Wai.rawPathInfo req
  , Wai.requestHeaders    = modifyHeader <$> (Wai.requestHeaders req)
  } where
    endpoint'                   = TE.encodeUtf8 endpoint
    uri                         = either endpointError id $ URI.parseURI URI.strictURIParserOptions endpoint'
    host                        = maybe noHostError (URI.hostBS . URI.authorityHost) (URI.uriAuthority uri)
    scheme                      = TE.decodeUtf8 . URI.schemeBS . URI.uriScheme $ uri
    modifyHeader h@(key, value) = if key == hostHeaderKey then (key, host) else h
    modifyPath                  = TE.encodeUtf8 . T.append scheme . T.append "://" . T.append (TE.decodeUtf8 host) . TE.decodeUtf8
    endpointError e             = error $ "Error parsing endpoint as URI, " <> show e
    noHostError                 = error "No host could be extracted from the endpoint"
    hostHeaderKey               = (mk . TE.encodeUtf8 $ "Host")

buildRequest :: [Text] -> Wai.Request -> LBS.ByteString -> SavedRequest
buildRequest ignoredHeaders r body =
  SavedRequest
    { methodName              = TE.decodeUtf8 $ Wai.requestMethod r
    , headers                 = reqHeaders
    , url                     = TE.decodeUtf8 $ Wai.rawPathInfo r
    , params                  = Wai.queryString r
    , body                    = LBS.toStrict body
    }
  where
    reqHeaders                = filter (\(key, value) -> elem key ignoredHeaders')  (Wai.requestHeaders r)
    ignoredHeaders'           = mk . BE.encodeUtf8 <$> ignoredHeaders

buildResponse :: LBS.ByteString -> Wai.Response -> IO SavedResponse
buildResponse body response = do
  pure $ SavedResponse
    { body                  = body
    , headers               = Wai.responseHeaders response
    , status                = Wai.responseStatus response
    }

getResponseBody :: Wai.Response -> IO (HT.Status, HT.ResponseHeaders, LBS.ByteString)
getResponseBody res =
  let (status,headers,body) = Wai.responseToStream res in
  body $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    b <- toLazyByteString <$> readIORef content
    pure (status, headers, b)



-- Taken from: https://github.com/yesodweb/wai/blob/master/wai-extra/Network/Wai/Middleware/RequestLogger.hs#L220
getRequestBody :: Wai.Request -> IO (Wai.Request, [BS.ByteString])
getRequestBody req = do
  let loop front = do
         bs <- Wai.getRequestBodyChunk req
         if BS.null bs
             then return $ front []
             else loop $ front . (bs:)
  body <- loop id
  -- Since reading the body consumes it, we need to refill it.
  -- This implementation ensures that each chunk is only returned
  -- once.
  ichunks <- newIORef body
  let rbody = atomicModifyIORef ichunks $ \chunks ->
         case chunks of
             []  -> ([], BS.empty)
             x:y -> (y, x)
  -- This triggers a deprecation warning, that makes no sense in this case, since there is no other way to reset the
  -- request body
  let req' = req { Wai.requestBody = rbody }
  return (req', body)


die :: String -> IO a
die err = (System.IO.hPutStrLn stderr err) >> XIO.exitFailure
