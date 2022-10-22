{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.VCR.Compression where

import qualified Codec.Compression.Zstd as Zstd
import qualified Data.ByteString as BS
import Data.Yaml

{- | Magic number at the start of zstd frame, see <https://github.com/facebook/zstd/blob/dev/doc/zstd_compression_format.md#zstandard-frames>
 Used to detect compressed payloads.
-}
zstdMagicNumber :: BS.ByteString
zstdMagicNumber = "\x28\xb5\x2f\xfd"

save :: ToJSON a => Int -> FilePath -> a -> IO ()
save compressLevel path v = do
  let compressed = Zstd.compress compressLevel $ encode v
  BS.writeFile path compressed

load :: FromJSON a => FilePath -> IO (Either ParseException a)
load path = do
  compressed <- BS.readFile path
  if zstdMagicNumber `BS.isPrefixOf` compressed
  then do
    case Zstd.decompress compressed of
      Zstd.Error err -> error $ "Invalid zstd compressed value: " <> show err
      Zstd.Skip -> error "Invalid zstd compressed value (Skip frame)"
      Zstd.Decompress v -> pure $ decodeEither' v
  else
    error $ "Expected " <> show path <> " to start with zstd magic number"
