{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.VCR
    ( server
    ) where

import           Control.Exception          (SomeException)
import           Data.ByteString.Builder    (toLazyByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.IORef                 (modifyIORef', newIORef, readIORef)

import qualified Data.Text.Encoding         as TE
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Conduit       as HC
import qualified Network.HTTP.Proxy         as HProxy (Request (..),
                                                       Settings (..),
                                                       defaultProxySettings,
                                                       httpProxyApp)
import qualified Network.HTTP.Types         as HT
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Network.Wai.Internal       as Wai (Response (..))

import           Network.VCR.Types          (ApiCall (..), Cassette (..),
                                             SavedRequest (..),
                                             SavedResponse (..))

import           Data.Yaml                  (decodeFileEither, encodeFile)



server :: IO ()
server = do
  mgr <- HC.newManager HC.tlsManagerSettings
  cassette <- decodeFileEither filePath

  case cassette of
    Left err  -> error $ "Cassette: " <> filePath <> " couldn't be decoded or found! " <> (show err)
    Right cas -> Warp.runSettings (warpSettings settings) $ middle filePath cas $ HProxy.httpProxyApp settings mgr
  where
    filePath = "cassette.yaml"
    settings = HProxy.defaultProxySettings { HProxy.proxyPort = 3128, HProxy.proxyRequestModifier = handler }

handler :: HProxy.Request -> IO (Either Wai.Response HProxy.Request)
handler req = pure $ Right req


middle :: FilePath -> Cassette -> Wai.Middleware
middle filePath cassette app req respond =  app req $ \response -> do
  savedRequest  <- buildRequest req
  putStrLn $ show savedRequest
  (status, headers, reBody) <- responseBody response
  savedResponse <- buildResponse reBody response
  putStrLn $ show savedResponse
  saveApiCalls filePath $
    (apiCalls cassette) <> [ApiCall { request = savedRequest, response = savedResponse }]
  respond $ Wai.responseLBS status headers reBody

warpSettings :: HProxy.Settings -> Warp.Settings
warpSettings pset = Warp.setPort (HProxy.proxyPort pset)
    . Warp.setHost (HProxy.proxyHost pset)
    . Warp.setTimeout (HProxy.proxyTimeout pset)
    . Warp.setOnException (\ _ _ -> return ())
    . Warp.setOnExceptionResponse defaultExceptionResponse
    $ Warp.setNoParsePath True Warp.defaultSettings

defaultExceptionResponse :: SomeException -> Wai.Response
defaultExceptionResponse e =
        Wai.responseLBS HT.internalServerError500
                [ (HT.hContentType, "text/plain; charset=utf-8") ]
                $ LBS.fromChunks [BS.pack $ show e]



saveApiCalls :: FilePath -> [ApiCall] -> IO ()
saveApiCalls fname calls = encodeFile fname cas
                         where cas = Cassette calls


buildRequest :: Wai.Request -> IO SavedRequest
buildRequest r = do
  body <- Wai.strictRequestBody r
  pure $ SavedRequest
    { methodName = TE.decodeUtf8 $ Wai.requestMethod r
    , headers = Wai.requestHeaders r
    , url = TE.decodeUtf8 $ Wai.rawPathInfo r
    , params = Wai.queryString r
    , body = LBS.toStrict body
    }

buildResponse :: LBS.ByteString -> Wai.Response -> IO SavedResponse
buildResponse body response = do
  pure $ SavedResponse
    { body = body
    , headers = Wai.responseHeaders response
    , status = Wai.responseStatus response
    }

responseBody :: Wai.Response -> IO (HT.Status, HT.ResponseHeaders, LBS.ByteString)
responseBody res =
  let (status,headers,body) = Wai.responseToStream res in
  body $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    b <- toLazyByteString <$> readIORef content
    pure (status, headers, b)
