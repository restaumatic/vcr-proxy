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



import           Network.VCR.Middleware     (die, middleware)
import           System.Environment         (getArgs)


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
  Warp.runSettings (warpSettings settings) $ middleware filePath $ HProxy.httpProxyApp settings mgr
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


