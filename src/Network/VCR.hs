{-# LANGUAGE NamedFieldPuns #-}

module Network.VCR (
    server,
    withServer,
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (
    MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
 )
import Control.Exception (SomeException, bracket)

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as HC
import qualified Network.HTTP.Proxy as HProxy (
    Request (..),
    Settings (..),
    defaultProxySettings,
    httpProxyApp,
 )
import qualified Network.HTTP.Types as HT
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Control.Applicative ((<**>))
import Network.VCR.Middleware (die, middleware)
import Network.VCR.Types (
    Cassette,
    Mode (..),
    Options (..),
    emptyCassette,
    parseOptions,
 )
import Options.Applicative (
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
 )
import System.Environment (getArgs)

import Data.Yaml (decodeFileEither, encodeFile, ToJSON, ParseException, FromJSON)
import System.Directory (doesFileExist)
import System.IO (
    BufferMode (..),
    hSetBuffering,
    stdout,
 )
import qualified Network.VCR.Compression as Compression

server :: IO ()
server = execParser opts >>= run
  where
    opts =
        info
            (parseOptions <**> helper)
            ( fullDesc
                <> progDesc "Run the VCR proxy to replay or record API calls. Runs in replay mode by default."
                <> header "VCR Proxy"
            )

run :: Options -> IO ()
run options = withServer options $ do
    forever $ threadDelay 1000000000

withServer :: Options -> IO a -> IO a
withServer options@Options{mode, cassettePath, port, compression} action = do
    putStrLn $ "Starting VCR proxy, mode: " <> show mode <> ", cassette file: " <> cassettePath <> ", listening on port: " <> show port
    case mode of
        Record endpoint -> do
            exists <- doesFileExist cassettePath
            when (not exists) $ saveCassette compression cassettePath (emptyCassette $ T.pack endpoint)
        _ -> pure ()
    cas <- loadCassette compression cassettePath
    case cas of
        Left err -> die $ "Cassette: " <> cassettePath <> " couldn't be decoded or found! " <> (show err)
        Right cassette -> do
            cassetteIORef <- newIORef cassette
            runInternal options cassetteIORef $ do
                putStrLn "VCR proxy started"
                action

runInternal :: Options -> IORef Cassette -> IO a -> IO a
runInternal Options{mode, cassettePath, port, compression} cassetteIORef action = do
    -- Set line buffering, because if we use it from a parent process, pipes are full buffered by default
    hSetBuffering stdout LineBuffering
    started <- newEmptyMVar
    bracket (start started) (stop compression cassettePath cassetteIORef) $ \_ -> do
        takeMVar started
        action
  where
    start started = forkIO $ do
        mgr <- HC.newManager HC.tlsManagerSettings
        Warp.runSettings (warpSettings started proxySettings) $ middleware mode cassetteIORef $ HProxy.httpProxyApp proxySettings mgr
    stop compression cassettePath casetteIORef threadId = do
        case mode of
            Record _ -> do
                cassette <- readIORef casetteIORef
                saveCassette compression cassettePath cassette
            _ -> pure ()
        killThread threadId
    proxySettings = HProxy.defaultProxySettings{HProxy.proxyPort = port}

warpSettings ::
    -- | MVar to put after starting
    MVar () ->
    HProxy.Settings ->
    Warp.Settings
warpSettings started pset =
    Warp.setPort (HProxy.proxyPort pset)
        . Warp.setHost (HProxy.proxyHost pset)
        . Warp.setTimeout (HProxy.proxyTimeout pset)
        . Warp.setOnExceptionResponse defaultExceptionResponse
        -- This is needed so we know when we start using the proxy if it is run as a child process
        . Warp.setBeforeMainLoop (putMVar started ())
        $ Warp.setNoParsePath True Warp.defaultSettings

defaultExceptionResponse :: SomeException -> Wai.Response
defaultExceptionResponse e =
    Wai.responseLBS
        HT.badGateway502
        [(HT.hContentType, "text/plain; charset=utf-8")]
        $ LBS.fromChunks [BS.pack $ show e]

saveCassette :: ToJSON a => Bool -> FilePath -> a -> IO ()
saveCassette compression path v =
    if compression
    then Compression.save 3 path v
    else encodeFile path v

loadCassette :: FromJSON a => Bool -> FilePath -> IO (Either ParseException a)
loadCassette compression path =
    if compression
    then Compression.load path
    else decodeFileEither path
