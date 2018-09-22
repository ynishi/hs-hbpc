module Domain.PartsSpec
  ( spec
  ) where

import qualified Domain.Parts as P
import           Test.Hspec

spec :: Spec
spec = do
  describe "parts" $
    it "sum of price" $
    P.sum
      [P.Part P.MotherBoard 9000 $ P.Store "", P.Part P.CPU 10000 $ P.Store ""] `shouldBe`
    19000
  describe "pc" $ do
    let pc =
          P.Computer
            P.PC
            [P.Part P.MotherBoard 9000 $ P.Store "example.com"]
            [P.Store "amazon.co.jp"]
    it "sum of price" $ P.sum pc `shouldBe` 9000
    it "uniq stores" $
      P.uniqStores pc `shouldBe` [P.Store "amazon.co.jp", P.Store "example.com"]
