{-# LANGUAGE OverloadedStrings #-}
module TestMain (main) where

import Test.Hspec
import Test.Hspec.Hedgehog

--import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Core.Types
import Env
import Core.{{ name | toPascal }}

main :: IO ()
main = hspec $
  describe "test {{ name }} properties" $ do
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
    it "test property with hedgehog - property 2" $ hedgehog $ do
      theOptionName <- forAll $ Gen.element [Xms, Xmx, Xloggc]
      theOptionValue <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
      let optionCmd = toJVMCmdLine (JVMCmdLineOption $
                                    Option theOptionName theOptionValue)
      case theOptionValue of
        "" -> optionCmd === ""
        _ -> case theOptionName of
              Xms -> optionCmd === "-Xms" <> theOptionValue
              Xmx -> optionCmd === "-Xmx" <> theOptionValue
              Xloggc -> optionCmd === "-Xloggc:" <> theOptionValue
    it "test property with hedgehog - property 3" $ hedgehog $ do
      propertyName <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
      propertyValue <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
      let propertyCmd = toJVMCmdLine (JVMCmdLineProperty $
                                      Property propertyName propertyValue)
      case (propertyName, propertyValue) of
        ("", _) -> propertyCmd === ""
        (_, "") -> propertyCmd === ""
        _ -> propertyCmd === "-D" <> propertyName <> "=" <> propertyValue
