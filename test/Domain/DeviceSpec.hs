module Domain.DeviceSpec
  ( spec
  ) where

import           Domain.Device
import           Test.Hspec

import qualified Algebra.Graph as AG
import           Control.Lens     ((.~))

spec :: Spec
spec =
  describe "device" $ do
    it "init Device" $
      Device
        { _deviceId = "productId"
        , _devicePrice = 10000
        , _deviceProductName = "productName"
        , _deviceDesc = "Desc"
        , _deviceContains = [DefaultDevice]
        , _deviceIfaces = ["tcpip"]
        } `shouldBe`
      Device "productId" 10000 "productName" "Desc" [DefaultDevice] ["tcpip"]
    it "init DefaultDevice" $ DefaultDevice `shouldBe` DefaultDevice
    it "defaultDevice return Blueprint with default value" $
      defaultDevice `shouldBe` Device "" 0 "" "" [] []
    it "sumPrice return sum of all price" $ do
      let inner1 = devicePrice .~ 10000 $ defaultDevice
      let inner2 = (devicePrice .~ 20000) . (deviceContains .~ [inner1]) $ defaultDevice
      let device = (devicePrice .~ 30000) . (deviceContains .~ [inner2]) $ defaultDevice
      sumPrice device `shouldBe` 60000
    it "sumPrice with DefaultDevice return 0" $
      sumPrice DefaultDevice `shouldBe` 0