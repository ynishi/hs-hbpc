module PartsSpec (spec) where

import Test.Hspec
import qualified Parts as P

spec :: Spec
spec = do
    describe "parts" $ do
      it "sum of price" $ do
        P.sum [P.Part P.MotherBoard 9000, P.Part P.CPU 10000] `shouldBe` 19000
    describe "pc" $ do
      let pc = P.Computer P.PC [P.Part P.MotherBoard 9000] [P.Store "amazon.co.jp"]
      it "sum of price" $ do
        P.sum pc `shouldBe` 9000
      it "uniq stores" $ do
        P.uniqStores pc `shouldBe` [P.Store "amazon.co.jp"] 


