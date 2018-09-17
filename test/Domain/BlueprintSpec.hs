module Domain.BlueprintSpec
  ( spec
  ) where

import           Domain.Blueprint
import           Test.Hspec

spec :: Spec
spec = do
  describe "blueprint" $ do
    it "init Blueprint" $ do
      Blueprint
        { _bpName = "blueprint"
        , _bpTitle = "bp title"
        , _bpDesc = "bp desc"
        , _bpComponents = []
        } `shouldBe`
        Blueprint "blueprint" "bp title" "bp desc" []
  describe "component" $ do
    it "init Hub" $ do Hub "hub1" [End] `shouldBe` Hub "hub1" [End]
    it "init End" $ do End `shouldBe` End
    it "init Hub with Device" $ do
      HubDevice "hub1" DefaultDevice [End] `shouldBe`
        HubDevice "hub1" DefaultDevice [End]
    it "link 2Hub each other" $ do
      let hub1 = Hub "hub1" []
      let hub2 = Hub "hub2" []
      link hub1 hub2 `shouldBe` (Hub "hub1" [hub2], Hub "hub2" [hub1])
    it "near return Hubs" $ do
      let hub1 = Hub "hub1" []
      let hub2 = Hub "hub2" []
      let (hub1', _) = link hub1 hub2
      near hub1' `shouldBe` [Hub "hub2" []]
  describe "device" $ do
    it "init Device" $ do
      Device
        { _deviceId = "productId"
        , _devicePrice = 10000
        , _deviceProductName = "productName"
        , _deviceDesc = "Desc"
        , _deviceContains = [DefaultDevice]
        } `shouldBe`
        Device "productId" 10000 "productName" "Desc" [DefaultDevice]
    it "init DefaultDevice" $ do DefaultDevice `shouldBe` DefaultDevice
    it "makeDevice return Device with field val" $ do
      makeDevice `shouldBe` Device "" 0 "" "" []
    it "sumPrice return sum of all price" $ do
      let inner1 = Device {_devicePrice = 10000, _deviceContains = []}
      let inner2 = Device {_devicePrice = 20000, _deviceContains = [inner1]}
      let device = Device {_devicePrice = 30000, _deviceContains = [inner2]}
      sumPrice device `shouldBe` 60000
    it "sumPrice with DefaultDevice return 0" $ do
      sumPrice DefaultDevice `shouldBe` 0
