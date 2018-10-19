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
      , _bpDevices = []
      , _bpGraphs = Map.empty
      } `shouldBe`
    Blueprint "blueprint" "bp title" "bp desc" [] Map.empty
  describe "defaultBlueprint" $
    it "returns default value" $
    defaultBlueprint `shouldBe` Blueprint "" "" "" [] Map.empty
  describe "addDevice" $
    context "when default value" $ do
      let testBp = addDevice defaultDevice defaultBlueprint
      it "add device to _bpDevices" $
        (testBp ^. bpDevices) `shouldBe`
        [Device {_deviceName = "", _deviceDesc = "", _deviceIfaces = []}]
      it "add device to _bpGraphs" $ (testBp ^. bpGraphs) `shouldBe` Map.empty
  describe "lens getter" $ do
    describe "bpName" $
      it "view _bpName" $ defaultBlueprint ^. bpName `shouldBe` ""
    describe "bpTitle" $
      it "view _bpTitle" $ defaultBlueprint ^. bpTitle `shouldBe` ""
    describe "bpDesc" $
      it "view _bpDesc" $ defaultBlueprint ^. bpDesc `shouldBe` ""
    describe "bpDevices" $
      it "view _bpDevices" $ defaultBlueprint ^. bpDevices `shouldBe` []
    describe "bpGraphs" $
      it "preview _bpGraphs" $ defaultBlueprint ^. bpGraphs `shouldBe` Map.empty
  describe "link" $ do
    it "link 2 device in Blueprint" $ do
      let iface = "tcpip"
      let device1 = defaultDevice {_deviceName = "d1", _deviceIfaces = [iface]}
      let device2 = defaultDevice {_deviceName = "d2", _deviceIfaces = [iface]}
      let bp = addDevice device2 . addDevice device1 $ defaultBlueprint
      link device1 device2 iface bp `shouldBe`
        Blueprint
          { _bpName = ""
          , _bpTitle = ""
          , _bpDesc = ""
          , _bpDevices =
              [ Device
                  { _deviceName = "d2"
                  , _deviceDesc = ""
                  , _deviceIfaces = ["tcpip"]
                  }
              , Device
                  { _deviceName = "d1"
                  , _deviceDesc = ""
                  , _deviceIfaces = ["tcpip"]
                  }
              ]
          , _bpGraphs =
              Map.fromList
                [ ( "tcpip"
                  , AG.Overlay
                      (AG.Connect
                         (AG.Vertex
                            (Device
                               { _deviceName = "d1"
                               , _deviceDesc = ""
                               , _deviceIfaces = ["tcpip"]
                               }))
                         (AG.Vertex
                            (Device
                               { _deviceName = "d2"
                               , _deviceDesc = ""
                               , _deviceIfaces = ["tcpip"]
                               })))
                      (AG.Overlay
                         (AG.Vertex
                            (Device
                               { _deviceName = "d2"
                               , _deviceDesc = ""
                               , _deviceIfaces = ["tcpip"]
                               }))
                         (AG.Vertex
                            (Device
                               { _deviceName = "d1"
                               , _deviceDesc = ""
                               , _deviceIfaces = ["tcpip"]
                               }))))
                ]
          }
