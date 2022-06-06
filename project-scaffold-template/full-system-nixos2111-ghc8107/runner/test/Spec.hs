{-# LANGUAGE OverloadedStrings #-}
module TestMain (main) where

import Test.Hspec
import Test.Hspec.Hedgehog

--import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Core.Types
import Env
import Core.JavaAnalyzerRunner

-- | Following is only for example
-- | you must adapt it accordingly
main :: IO ()
main = hspec $
  describe "test java-analyzer-runner properties" $ do
    it "test property with hedgehog - property 1" $ hedgehog $ do
      theSwitchName <- forAll $
        Gen.element
        [VerboseGC, VerboseClass, VerboseModule, VerboseJNI, Xint, Xrs
        , Xnoclassgc]
      case toJVMCmdLine (JVMCmdLineSwitch $ Switch theSwitchName) of
        "-verbose:gc" -> success
        "-verbose:class" -> success
        "-verbose:module" -> success
        "-verbose:jni" -> success
        "-Xint" -> success
        "-Xrs" -> success
        "-Xnoclassgc" -> success
        _ -> failure
