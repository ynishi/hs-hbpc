module Domain.BuyingSpec
  ( spec
  ) where

import           Domain.Buying
import           Test.Hspec

import           Domain.Device

import qualified Algebra.Graph as AG
import Control.Lens ((.~), (&))

spec :: Spec
spec = do
  describe "buying" $
    it "init Buying" $
    Buying
      { _buyingPrice = 1000
      , _buyingDevice = defaultDevice
      , _buyingShop = "shop1"
      , _buyingAt = "2018/01/01"
      } `shouldBe`
    Buying 1000 defaultDevice "shop1" "2018/01/01"
  describe "estimate" $ do
    context "when with []" $ it "return 0" $ estimate [] `shouldBe` 0
    context "when with buying::[]" $
      it "return buyingPrice" $
      estimate [defaultBuying & buyingPrice .~ 1000] `shouldBe` 1000
    context "when with buyings" $
      it "sum buyingPrice" $ do
        let bs =
              map
                (defaultBuying &)
                [buyingPrice .~ 1000, buyingPrice .~ 2000]
        estimate bs `shouldBe` 3000
