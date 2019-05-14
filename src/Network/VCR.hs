{-# LANGUAGE NamedFieldPuns #-}
module Network.VCR
    ( server
    , withServer
    ) where

import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, takeMVar, MVar)
import           Control.Exception          (SomeException, bracket)
import           Control.Monad
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


import           Control.Applicative        ((<**>))
import           Network.VCR.Middleware     (die, middleware)
import           Network.VCR.Types          (Options (..), parseOptions)
import           Options.Applicative        (execParser, fullDesc, header,
                                             helper, info, progDesc)
import           System.Environment         (getArgs)

import           System.IO                  (BufferMode (..), hSetBuffering,
                                             stdout)


server :: IO ()
server = execParser opts >>= run
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "Run the VCR proxy to replay or record API calls. Runs in replay mode by default."
     <> header "VCR Proxy" )

run :: Options -> IO ()
run options@Options { mode, cassettePath, port } = do
  putStrLn $ "Starting VCR proxy, mode: " <> show mode  <> ", cassette file: " <> cassettePath <>  ", listening on port: " <> show port
  withServer options $ do
    putStrLn "VCR proxy started"
    forever $ threadDelay 1000000000

withServer :: Options -> IO a -> IO a
withServer Options { mode, cassettePath, port } action = do
  -- Set line buffering, because if we use it from a parent process, pipes are full buffered by default
  hSetBuffering stdout LineBuffering
  started <- newEmptyMVar
  bracket (start started) killThread $ \_ -> do
    takeMVar started
    action

  where
    start started = forkIO $ do
      mgr <- HC.newManager HC.tlsManagerSettings
      Warp.runSettings (warpSettings started proxySettings) $ middleware mode cassettePath $ HProxy.httpProxyApp proxySettings mgr
    proxySettings = HProxy.defaultProxySettings { HProxy.proxyPort = port }



warpSettings
  :: MVar () -- ^ MVar to put after starting
  -> HProxy.Settings
  -> Warp.Settings
warpSettings started pset = Warp.setPort (HProxy.proxyPort pset)
    . Warp.setHost (HProxy.proxyHost pset)
    . Warp.setTimeout (HProxy.proxyTimeout pset)
    . Warp.setOnExceptionResponse defaultExceptionResponse
    -- This is needed so we know when we start using the proxy if it is run as a child process
    . Warp.setBeforeMainLoop (putMVar started ())
    $ Warp.setNoParsePath True Warp.defaultSettings

defaultExceptionResponse :: SomeException -> Wai.Response
defaultExceptionResponse e =
        Wai.responseLBS HT.badGateway502
                [ (HT.hContentType, "text/plain; charset=utf-8") ]
                $ LBS.fromChunks [BS.pack $ show e]


