{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec (spec) where

import RIO
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Bool" $ do
    it "works with True" $
      True `shouldBe` True

    it "works with False" $
      False `shouldBe` False

    it "works with True and False" $
      True `shouldBe` not False
