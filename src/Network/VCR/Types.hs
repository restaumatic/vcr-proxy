{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.VCR.Types where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text.Encoding as BE (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding as BEL (decodeUtf8, encodeUtf8)

import Control.Monad (mzero)
import Data.Aeson (
  FromJSON,
  ToJSON,
  Value (..),
  decode,
  decodeStrict,
  encode,
  object,
  parseJSON,
  toJSON,
  (.:),
  (.=),
 )
import Data.Aeson.Diff (Patch)
import Data.CaseInsensitive (foldedCase, mk)
import GHC.Generics (Generic)

import Network.HTTP.Types (Header, Query, Status (..))

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Options.Applicative (
  Parser,
  auto,
  help,
  long,
  metavar,
  option,
  short,
  showDefault,
  strOption,
  switch,
  value,
 )

data Mode = Record {endpoint :: String} | Replay | ReplayStrict
  deriving (Show, Eq)

data ReplayError = RequestNotRecorded | ExpectedDifferentRequest SavedRequest

data VCRResponse = VCRResponse {_error :: Text, _receivedRequest :: SavedRequest, _expectedRequest :: Maybe SavedRequest, _diff :: Maybe Patch}
  deriving (Show, Eq, Generic)

instance ToJSON VCRResponse where
  toJSON VCRResponse {_error, _receivedRequest, _expectedRequest} =
    object
      [ "error" .= _error
      , "receivedRequest" .= toJSON _receivedRequest
      , "expectedRequest" .= toJSON _expectedRequest
      , "tag" .= ("VCRResponse" :: Text)
      ]

parseMode :: Parser Mode
parseMode =
  ( \mode endpoint ->
      case mode of
        "Record" -> Record endpoint
        "Replay" -> Replay
        "ReplayStrict" -> ReplayStrict
        m -> error $ "Uknown mode: " <> m
  )
    <$> strOption (long "mode" <> short 'm' <> metavar "MODE" <> help "Run vcr proxy in the specified mode: Record | Replay | ReplayStrict")
    <*> strOption (long "endpoint" <> short 'e' <> metavar "REMOTE_API_ENDPOINT" <> help "Forward requests to the specified API endpoint")

data Options = Options
  { cassettePath :: FilePath
  , mode :: Mode
  , port :: Int
  , compression :: Bool
  }
  deriving (Eq, Show)

pattern DEFAULT_PORT :: Int
pattern DEFAULT_PORT = 3128

parseOptions :: Parser Options
parseOptions =
  Options
    <$> strOption (long "cassette" <> short 'c' <> metavar "CASSETTE_FILE" <> help "Cassette yaml file for recording/replaying the API interactions")
    <*> parseMode
    <*> option auto (long "port" <> help "Port to listen on" <> showDefault <> value DEFAULT_PORT <> metavar "INT")
    <*> switch (long "compression" <> help "Should cassette yaml be (de)compressed")

data Body = JSONBody Value | RawBody B.ByteString
  deriving (Show, Eq)

bodyToLBS :: Body -> L.ByteString
bodyToLBS (RawBody bs) = L.fromStrict bs
bodyToLBS (JSONBody value) = encode value

lbsToBody :: L.ByteString -> Body
lbsToBody = bsToBody . L.toStrict

bsToBody :: B.ByteString -> Body
bsToBody body = maybe (RawBody body) JSONBody (decodeStrict body)

instance FromJSON Body where
  parseJSON v@(Object _) = pure $ JSONBody v
  parseJSON (String s) =
    -- Try extra hard to parse the body as JSON (for backwards compatibility with "stringified" JSON requests)
    pure $ bsToBody (BE.encodeUtf8 s)

instance ToJSON Body where
  toJSON (JSONBody b) = b
  toJSON (RawBody b) = toJSON (BE.decodeUtf8 b)

data SavedRequest = SavedRequest
  { methodName :: Text
  , headers :: [Header]
  , url :: Text
  , params :: Query
  , body :: Body
  }
  deriving (Show, Eq)

data ApiCall = ApiCall
  { request :: SavedRequest
  , response :: SavedResponse
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Cassette = Cassette
  { endpoint :: Text
  , apiCalls :: [ApiCall]
  , ignoredHeaders :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToJSON SavedRequest where
  toJSON (SavedRequest {methodName, headers, url, params, body}) =
    object
      [ "methodName" .= methodName
      , "headers" .= toJSON (fromHeader <$> headers)
      , "url" .= url
      , "params" .= toJSON (fromQuery <$> params)
      , "body" .= toJSON body
      ]

instance FromJSON SavedRequest where
  parseJSON (Object v) =
    SavedRequest
      <$> v
      .: "methodName"
      <*> ((\headers -> toHeader <$> headers) <$> (v .: "headers"))
      <*> v
      .: "url"
      <*> ((\params -> toQuery <$> params) <$> v .: "params")
      <*> (v .: "body")
  parseJSON _ = mzero

data SavedResponse = SavedResponse
  { body :: Body
  , headers :: [Header]
  , status :: Status
  }
  deriving (Show, Eq)

instance ToJSON SavedResponse where
  toJSON (SavedResponse {body, headers, status}) =
    object
      [ "body" .= toJSON body
      , "headers" .= toJSON (fromHeader <$> headers)
      , "status"
          .= object
            [ "code" .= statusCode status
            , "message" .= BE.decodeUtf8 (statusMessage status)
            ]
      ]

instance FromJSON SavedResponse where
  parseJSON (Object v) =
    SavedResponse
      <$> (v .: "body")
      <*> ((\headers -> toHeader <$> headers) <$> (v .: "headers"))
      <*> (v .: "status" >>= parseStatus)
    where
      parseStatus (Object s) = Status <$> (s .: "code") <*> (BE.encodeUtf8 <$> s .: "message")
      parseStatus _ = mzero
  parseJSON _ = mzero

fromQuery :: (B.ByteString, Maybe B.ByteString) -> (Text, Maybe Text)
fromQuery (name, value) = (BE.decodeUtf8 name, BE.decodeUtf8 <$> value)

toQuery :: (Text, Maybe Text) -> (B.ByteString, Maybe B.ByteString)
toQuery (name, value) = (BE.encodeUtf8 name, BE.encodeUtf8 <$> value)

fromHeader :: Header -> (Text, Text)
fromHeader (name, value) = (BE.decodeUtf8 $ foldedCase name, BE.decodeUtf8 value)

toHeader :: (Text, Text) -> Header
toHeader (name, value) = (mk $ BE.encodeUtf8 name, BE.encodeUtf8 value)

emptyCassette :: Text -> Cassette
emptyCassette endpoint = Cassette {endpoint = endpoint, apiCalls = [], ignoredHeaders = []}
