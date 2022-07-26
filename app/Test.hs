module Test where

import           Conduit
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.Cereal  as CerealC
import           Data.Digest.Pure.SHA (sha256)
import           Data.Serialize
import           Test.Tasty
import           Test.Tasty.HUnit

import           LuaJIT.Script

hs :: IO ()
hs = defaultMain isoTest


isoTest :: TestTree
isoTest =
    testGroup "Main"
        [ testIsoOn "luac/test.luac"
        , testIsoOn "luac/SP.luac"
        , testIsoOn "luac/Control_HP.luac"
        , testIsoOn "luac/ICKicker.luac"
        , testIsoOn "luac/moli.test-prot.luac"
        , testIsoOn "luac/hardcrypted.luac"
        , testIsoOn "luac/moli.test_protected.luac"
        , testIsoOn "luac/moonl.luac"
        , testIsoOn "luac/VolentProject_3.luac"
        -- testIsoOnСonduit "luac/VolentProject_3.luac"
        ]


testIsoOn :: FilePath -> TestTree
testIsoOn file =
    let
        steps step = do
          step $ "reading and parsing " ++ file
          raw <- BS.readFile file

          case runGet decodeScript raw of
              Left err -> do
                  assertFailure err

              Right script -> do
                  let encoded = runPut $ encodeScript script

                  step "checking hashes"
                  assertEqual "hashes" (sha256 $ LBS.fromStrict raw) (sha256 $ LBS.fromStrict encoded)
  in
  testCaseSteps ("The isomorphism check on " ++ file) steps


testIsoOnСonduit :: FilePath -> TestTree
testIsoOnСonduit file =
    let
        steps step = do
          step $ "reading and parsing " ++ file
          raw <- BS.readFile file

          script <- runConduitRes (sourceFile file .| CerealC.sinkGet decodeScript)

          let encoded = runPut $ encodeScript script

          step "checking hashes"
          assertEqual "hashes" (sha256 $ LBS.fromStrict raw) (sha256 $ LBS.fromStrict encoded)
  in
  testCaseSteps ("The isomorphism check on " ++ file) steps
