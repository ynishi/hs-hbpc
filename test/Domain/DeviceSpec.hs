module Domain.DeviceSpec
  ( spec
  ) where

import           Domain.Device
import           Test.Hspec

import qualified Algebra.Graph as AG

spec :: Spec
spec = do
  describe "device" $ do
    it "init Device" $ do
      Device
        { _deviceId = "productId"
        , _devicePrice = 10000
        , _deviceProductName = "productName"
        , _deviceDesc = "Desc"
        , _deviceContains = [DefaultDevice]
        , _deviceIfaces = ["tcpip"]
        } `shouldBe`
        Device "productId" 10000 "productName" "Desc" [DefaultDevice] ["tcpip"]
    it "init DefaultDevice" $ do DefaultDevice `shouldBe` DefaultDevice
    it "defaultDevice return Blueprint with default value" $ do
      defaultDevice `shouldBe` Device "" 0 "" "" [] []
    it "sumPrice return sum of all price" $ do
      let inner1 = Device {_devicePrice = 10000, _deviceContains = []}
      let inner2 = Device {_devicePrice = 20000, _deviceContains = [inner1]}
      let device = Device {_devicePrice = 30000, _deviceContains = [inner2]}
      sumPrice device `shouldBe` 60000
    it "sumPrice with DefaultDevice return 0" $ do
      sumPrice DefaultDevice `shouldBe` 0
