{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Network.HTTP.Types as HT
import Network.VCR.Types
  ( ApiCall (..)
  , Body (..)
  , SavedRequest (..)
  , SavedResponse (..)
  , buildReplayIndex
  , requestKey
  )
import System.Exit (exitFailure)

mkReq :: Int -> SavedRequest
mkReq n =
  SavedRequest
    { methodName = "GET"
    , headers = []
    , url = "/items/" <> T.pack (show n)
    , params = []
    , body = RawBody ""
    }

mkResp :: Int -> SavedResponse
mkResp n = mkRespWith n ""

mkRespWith :: Int -> String -> SavedResponse
mkRespWith n suffix =
  SavedResponse
    { body = RawBody (BS.pack ("response " <> show n <> suffix))
    , headers = []
    , status = HT.status200
    }

mkCall :: Int -> ApiCall
mkCall n = ApiCall {request = mkReq n, response = mkResp n}

assert :: String -> Bool -> IO ()
assert msg True = putStrLn $ "  ok - " <> msg
assert msg False = do
  putStrLn $ "  FAIL - " <> msg
  exitFailure

main :: IO ()
main = do
  putStrLn "vcr-proxy tests"

  -- Test 1: basic lookup.
  let calls = [mkCall 1, mkCall 2, mkCall 3]
      idx = buildReplayIndex calls
  assert "index size matches unique requests"
    (HM.size idx == 3)
  assert "lookup returns the right ApiCall"
    ((response <$> HM.lookup (requestKey (mkReq 2)) idx)
     == Just (mkResp 2))

  -- Test 2: duplicate-key handling preserves first occurrence (matches
  -- the previous 'find' semantics).
  let dupCalls =
        [ ApiCall {request = mkReq 7, response = mkRespWith 7 "-A"}
        , ApiCall {request = mkReq 7, response = mkRespWith 7 "-B"}
        , ApiCall {request = mkReq 7, response = mkRespWith 7 "-C"}
        ]
      dupIdx = buildReplayIndex dupCalls
  assert "duplicate keys collapse to one entry"
    (HM.size dupIdx == 1)
  assert "duplicate-key lookup returns the FIRST recording"
    ((response <$> HM.lookup (requestKey (mkReq 7)) dupIdx)
     == Just (mkRespWith 7 "-A"))

  -- Test 3: timing sanity. 3000 entries, 3000 lookups should be far under a second.
  let n = 3000
      bigCalls = [mkCall i | i <- [1 .. n]]
      bigIdx = buildReplayIndex bigCalls
      countHit :: Maybe ApiCall -> Int
      countHit = maybe 0 (const 1)
  t0 <- getCurrentTime
  let hits = sum [countHit (HM.lookup (requestKey (mkReq i)) bigIdx) | i <- [1 .. n]]
  hits `seq` pure ()
  t1 <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
  putStrLn $ "  timing - " <> show n <> " lookups against " <> show n
    <> " entry index: " <> show (elapsed * 1000) <> " ms (hits=" <> show hits <> ")"
  assert "all 3000 lookups hit" (hits == n)
  assert "3000 lookups complete in under 1 second" (elapsed < 1.0)

  putStrLn "all tests passed"
