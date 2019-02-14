module Network.VCR
    ( server
    ) where

import           Control.Exception          (SomeException)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Network.HTTP.Client        as HC
import qualified Network.HTTP.Conduit       as HC
import qualified Network.HTTP.Proxy         as HProxy (Request (..),
                                                       Settings (..),
                                                       defaultProxySettings,
                                                       httpProxyApp)
import qualified Network.HTTP.Types         as HT
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp


import           Data.Yaml                  (decodeFileEither)

import           Network.VCR.Middleware     (middleware)
import           System.Environment         (getArgs)
import qualified System.Exit                as XIO
import           System.IO                  (stderr)
import qualified System.IO                  (hPutStrLn)


server :: IO ()
server = do
  args <- getArgs
  case args of
    [filePath, port] -> run filePath $ read port
    [filePath]       -> run filePath 3128
    _                -> die $ "Please provide the path to the cassette yaml file"


run :: FilePath -> Int -> IO ()
run filePath port = do
  mgr <- HC.newManager HC.tlsManagerSettings
  cassette <- decodeFileEither filePath
  case cassette of
    Left err  -> die $ "Cassette: " <> filePath <> " couldn't be decoded or found! " <> (show err)
    Right cas -> Warp.runSettings (warpSettings settings) $ middleware filePath cas $ HProxy.httpProxyApp settings mgr

  where
    settings = HProxy.defaultProxySettings { HProxy.proxyPort = port }


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


die :: String -> IO a
die err = (System.IO.hPutStrLn stderr err) >> XIO.exitFailure
