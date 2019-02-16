{-# LANGUAGE NamedFieldPuns #-}
module Network.VCR.Middleware where

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

import           Data.CaseInsensitive       (mk)
import qualified Data.Text.Encoding         as BE (encodeUtf8)

import           System.Directory           (doesFileExist)
import qualified System.Exit                as XIO
import           System.IO                  (stderr)
import qualified System.IO                  (hPutStrLn)


middleware :: Mode -> FilePath -> Wai.Middleware
middleware Replay = replayingMiddleware
middleware Record = recordingMiddleware


recordingMiddleware :: FilePath -> Wai.Middleware
recordingMiddleware filePath app req respond = do
  exists <- doesFileExist filePath
  if not exists then
    encodeFile filePath emptyCassette
    else
      pure ()
  cas <- decodeFileEither filePath
  -- create file if it doesn't exist
  case cas of
    Left  err -> die $ "Cassette: " <> filePath <> " couldn't be decoded or found! " <> (show err)
    Right cassette@Cassette { apiCalls, ignoredHeaders } ->
      app req $ \response -> do
        savedRequest <- buildRequest ignoredHeaders req
        -- Since reading the response body consumes it, we can't just reuse the response
        (status, headers, reBody) <- responseBody response
        savedResponse <- buildResponse reBody response
        encodeFile filePath $ cassette
          { apiCalls = apiCalls <> [ApiCall { request = savedRequest, response = savedResponse }] }
        respond $ Wai.responseLBS status headers reBody


replayingMiddleware :: FilePath -> Wai.Middleware
replayingMiddleware filePath app req respond = do
  cas <- decodeFileEither filePath
  case cas of
    Left  err -> die $ "Cassette: " <> filePath <> " couldn't be decoded or found! " <> (show err)
    Right cassette@Cassette { apiCalls, ignoredHeaders } -> do
      savedRequest <- buildRequest ignoredHeaders req
      case find (\c -> request c == savedRequest) apiCalls of
        Just c ->
          let
            res = (response c)
          in
            respond $ Wai.responseLBS (status res) (headers (res :: SavedResponse)) (body (res :: SavedResponse))
        Nothing -> do
          respond $ Wai.responseLBS HT.status500
              { HT.statusMessage =
                  TE.encodeUtf8 . T.pack $ "The request: " <> show savedRequest  <> " is not recorded!" } [] ""


buildRequest :: [Text] -> Wai.Request -> IO SavedRequest
buildRequest ignoredHeaders r = do
  body <- Wai.strictRequestBody r
  pure $ SavedRequest
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

responseBody :: Wai.Response -> IO (HT.Status, HT.ResponseHeaders, LBS.ByteString)
responseBody res =
  let (status,headers,body) = Wai.responseToStream res in
  body $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    b <- toLazyByteString <$> readIORef content
    pure (status, headers, b)

die :: String -> IO a
die err = (System.IO.hPutStrLn stderr err) >> XIO.exitFailure
