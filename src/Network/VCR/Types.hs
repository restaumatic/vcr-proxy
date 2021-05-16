{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.VCR.Types where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Maybe              (fromJust)
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as BE (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding as BEL (decodeUtf8, encodeUtf8)
import           Data.Yaml               (ParseException, decodeFileEither)


import           Control.Monad           (mzero)
import qualified Data.Aeson.Types        as JSON
import           Data.Aeson              (FromJSON, ToJSON, Value (..), object,
                                          decode, encode,
                                          parseJSON, toJSON, (.:), (.=))
import           Data.CaseInsensitive    (foldedCase, mk)
import           GHC.Generics            (Generic)

import           Network.HTTP.Types      (Header, Query, Status (..))

import           Control.Applicative     ((<|>))
import           Data.Semigroup          ((<>))
import           Options.Applicative     (Parser, auto, help, long, metavar,
                                          option, short, showDefault, strOption,
                                          value)


data Mode = Record { endpoint :: String } | Replay | ReplayStrict
  deriving (Show, Eq)


parseMode :: Parser Mode
parseMode =
  (\mode endpoint ->
    case mode of
      "Record"       -> Record endpoint
      "Replay"       -> Replay
      "ReplayStrict" -> ReplayStrict
      m              -> error $ "Uknown mode: " <> m
  ) <$> strOption ( long "mode" <> short 'm' <> metavar "MODE" <> help "Run vcr proxy in the specified mode: Record | Replay | ReplayStrict")
    <*> strOption ( long "endpoint" <> short 'e' <> metavar "REMOTE_API_ENDPOINT" <> help "Forward requests to the specified API endpoint")

data Options = Options
  { cassettePath :: FilePath
  , mode         :: Mode
  , port         :: Int
  } deriving (Eq, Show)

pattern DEFAULT_PORT :: Int
pattern DEFAULT_PORT = 3128

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption (long "cassette" <> short 'c' <> metavar "CASSETTE_FILE" <> help "Cassette yaml file for recording/replaying the API interactions")
  <*> parseMode
  <*> option auto (long "port" <> help "Port to listen on" <> showDefault <> value DEFAULT_PORT <> metavar "INT")


data SavedRequest = SavedRequest
  { methodName :: Text
  , headers    :: [Header]
  , url        :: Text
  , params     :: Query
  , body       :: B.ByteString
  } deriving (Show, Eq)


data ApiCall = ApiCall
  { request  :: SavedRequest
  , response :: SavedResponse
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Cassette = Cassette
  { endpoint          :: Text
  , apiCalls          :: [ApiCall]
  , ignoredHeaders    :: [Text]
  , ignoredBodyFields :: [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


instance ToJSON SavedRequest where
  toJSON (SavedRequest{ methodName, headers, url, params , body}) =
    object
      [ "methodName" .= methodName
      , "headers"    .= toJSON (fromHeader <$> headers)
      , "url"        .= url
      , "params"     .= toJSON (fromQuery <$> params)
      , "body"       .= BE.decodeUtf8 body
      ]

instance FromJSON SavedRequest where
  parseJSON (Object v) =
    SavedRequest <$>
      v .: "methodName" <*>
      ((\headers -> toHeader <$> headers) <$> (v .: "headers")) <*>
      v .: "url"    <*>
      ((\params -> toQuery <$> params) <$> v .: "params" ) <*>
      (BE.encodeUtf8 <$> v .: "body")
  parseJSON _          = mzero

data SavedResponse = SavedResponse
  { body    :: L.ByteString
  , headers :: [Header]
  , status  :: Status
  } deriving (Show, Eq)

instance ToJSON SavedResponse where
  toJSON (SavedResponse { body, headers, status }) =
    object
      [ "body"     .= (BEL.decodeUtf8 body)
      , "headers"  .= toJSON (fromHeader <$> headers)
      , "status"   .= object
        [ "code"   .= statusCode status
        , "message".= BE.decodeUtf8 (statusMessage status)
        ]
      ]

instance FromJSON SavedResponse where
  parseJSON (Object v)         =
    SavedResponse <$>
      (BEL.encodeUtf8 <$> v .: "body") <*>
      ((\headers -> toHeader <$> headers) <$> (v .: "headers")) <*>
      ( v.: "status"         >>= parseStatus)
    where
      parseStatus (Object s)   = Status <$> (s .: "code") <*> (BE.encodeUtf8 <$> s .: "message")
      parseStatus _            = mzero
  parseJSON _                  = mzero

fromQuery :: (B.ByteString, Maybe B.ByteString) -> (Text, Maybe Text)
fromQuery (name, value) = (BE.decodeUtf8 name, BE.decodeUtf8 <$> value)

toQuery :: (Text, Maybe Text) -> (B.ByteString, Maybe B.ByteString)
toQuery (name, value) = (BE.encodeUtf8 name, BE.encodeUtf8 <$> value)

fromHeader :: Header -> (Text, Text)
fromHeader (name, value) = (BE.decodeUtf8 $ foldedCase name, BE.decodeUtf8 value)

toHeader :: (Text, Text) -> Header
toHeader (name, value)  =  (mk $ BE.encodeUtf8 name, BE.encodeUtf8 value)


emptyCassette :: Text -> Cassette
emptyCassette endpoint = Cassette { endpoint = endpoint, apiCalls = [], ignoredHeaders = [], ignoredBodyFields = [] }

-- utility methods for matching with ignored fields in the body

-- | Remove the ignored fields from the request's body
modifyBody :: [Text] -> SavedRequest -> SavedRequest
modifyBody ignoredBodyFields (SavedRequest mn hs url ps body) = SavedRequest mn hs url ps body'
  where ignoredBodyFieldsMap = HashMap.fromList $ zip ignoredBodyFields (repeat Null)
        bodyMap :: HashMap Text Value = HashMap.difference (fromJust . decode $ L.fromStrict body) ignoredBodyFieldsMap
        body' = L.toStrict $ encode bodyMap

-- | Remove the ignored fields from all of the saved requests
--   this allows us to later match the requests without the ignored fields taken into account.
modifyCassette :: Cassette -> Cassette
modifyCassette (Cassette endpoint apiCalls ignoredHeaders ignoredBodyFields) =
  (Cassette endpoint apiCalls' ignoredHeaders ignoredBodyFields)
  where
    modifyApiCall ignoredBodyFields (ApiCall savedRequest savedResponse) = ApiCall (modifyBody ignoredBodyFields savedRequest) savedResponse
    apiCalls' = (modifyApiCall ignoredBodyFields) <$> apiCalls

-- | Read the cassette, removing the ignored fields from all saved requests' bodies
readCassette :: String -> IO (Either ParseException Cassette)
readCassette filePath = do
  cassette <- decodeFileEither filePath
  pure (modifyCassette <$> cassette)
