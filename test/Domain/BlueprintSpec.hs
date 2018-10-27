module Domain.BlueprintSpec
  ( spec
  ) where

import           Domain.Blueprint
import           Test.Hspec

import           Domain.Device

import qualified Algebra.Graph    as AG
import           Control.Lens     ((^.), (^?))
import qualified Data.Map.Strict  as Map

spec :: Spec
spec = do
  describe "Blueprint" $
    it "is be able to initialize" $
    Blueprint
      { _bpName = "blueprint"
      , _bpTitle = "bp title"
      , _bpDesc = "bp desc"
      , _bpDevices = Map.empty
      , _bpGraphs = Map.empty
      } `shouldBe`
    Blueprint "blueprint" "bp title" "bp desc" Map.empty Map.empty
  describe "defaultBlueprint" $
    it "returns default value" $
    defaultBlueprint `shouldBe` Blueprint "" "" "" Map.empty Map.empty
  describe "addDevice" $
    context "when default value" $ do
      let testBp = addDevice defaultDevice defaultBlueprint
      it "add device to _bpDevices" $
        (testBp ^. bpDevices) `shouldBe`
        Map.fromList
          [(1, Device {_deviceName = "", _deviceDesc = "", _deviceIfaces = []})]
      it "add device to _bpGraphs" $ (testBp ^. bpGraphs) `shouldBe` Map.empty
  describe "lens getter" $ do
    describe "bpName" $
      it "view _bpName" $ defaultBlueprint ^. bpName `shouldBe` ""
    describe "bpTitle" $
      it "view _bpTitle" $ defaultBlueprint ^. bpTitle `shouldBe` ""
    describe "bpDesc" $
      it "view _bpDesc" $ defaultBlueprint ^. bpDesc `shouldBe` ""
    describe "bpDevices" $
      it "view _bpDevices" $ defaultBlueprint ^. bpDevices `shouldBe` Map.empty
    describe "bpGraphs" $
      it "preview _bpGraphs" $ defaultBlueprint ^. bpGraphs `shouldBe` Map.empty
  describe "link" $
    it "link 2 device in Blueprint" $ do
      let iface = "tcpip"
      let device1 = defaultDevice {_deviceName = "d1", _deviceIfaces = [iface]}
      let device2 = defaultDevice {_deviceName = "d2", _deviceIfaces = [iface]}
      let bp = addDevice device2 . addDevice device1 $ defaultBlueprint
      let d1Id = head . Map.keys . _bpDevices $ bp
      let d2Id = Map.keys (_bpDevices bp) !! 1
      link d1Id d2Id iface bp `shouldBe`
        Blueprint
          { _bpName = ""
          , _bpTitle = ""
          , _bpDesc = ""
          , _bpDevices =
              Map.fromList
                [ ( 1
                  , Device
                      { _deviceName = "d1"
                      , _deviceDesc = ""
                      , _deviceIfaces = ["tcpip"]
                      })
                , ( 2
                  , Device
                      { _deviceName = "d2"
                      , _deviceDesc = ""
                      , _deviceIfaces = ["tcpip"]
                      })
                ]
          , _bpGraphs =
              Map.fromList [("tcpip", AG.Connect (AG.Vertex 1) (AG.Vertex 2))]
          }
