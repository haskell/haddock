{-# LANGUAGE OverloadedStrings #-}
module Documentation.Haddock.Parser.UtilSpec (main, spec) where

import Documentation.Haddock.Parser.Monad
import Documentation.Haddock.Parser.Util
import Data.Either.Compat (isLeft)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "takeUntil" $ do
    it "takes everything until a specified byte sequence" $ do
      parseOnly (takeUntil "end") "someend" `shouldBe` Right "some"

    it "requires the end sequence" $ do
      parseOnly (takeUntil "end") "someen" `shouldSatisfy` isLeft

    it "takes escaped bytes unconditionally" $ do
      parseOnly (takeUntil "end") "some\\endend" `shouldBe` Right "some\\end"
