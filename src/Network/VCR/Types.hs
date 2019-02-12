{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.VCR.Types where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as BE (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.Encoding as BEL (decodeUtf8, encodeUtf8)


import           Control.Monad           (mzero)
import           Data.Aeson              (FromJSON, ToJSON, Value (..), object,
                                          parseJSON, toJSON, (.:), (.=))
import           Data.CaseInsensitive    (foldedCase, mk)
import           GHC.Generics            (Generic)

import           Network.HTTP.Types      (Header, Query, Status (..))


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

data Cassette = Cassette { apiCalls :: [ApiCall] }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
  parseJSON _ = mzero

data SavedResponse = SavedResponse
  { body    :: L.ByteString
  , headers :: [Header]
  , status  :: Status
  } deriving (Show, Eq)

instance ToJSON SavedResponse where
  toJSON (SavedResponse { body, headers, status }) =
    object
      [ "body" .= (BEL.decodeUtf8 body)
      , "headers" .= toJSON (fromHeader <$> headers)
      , "status" .= object
        [ "code" .= statusCode status
        , "message" .= BE.decodeUtf8 (statusMessage status)
        ]
      ]

instance FromJSON SavedResponse where
  parseJSON (Object v) =
    SavedResponse <$>
      (BEL.encodeUtf8 <$> v .: "body") <*>
      ((\headers -> toHeader <$> headers) <$> (v .: "headers")) <*>
      ( v.: "status" >>= parseStatus)
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
toHeader (name, value)  =  (mk $ BE.encodeUtf8 name, BE.encodeUtf8 value)


