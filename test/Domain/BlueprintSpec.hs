module Domain.BlueprintSpec
  ( spec
  ) where

import           Domain.Blueprint
import           Test.Hspec

import           Domain.Device

import qualified Algebra.Graph    as AG
import           Control.Lens     ((^.), (^?))
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set

spec :: Spec
spec = do
  describe "Blueprint" $ do
    it "is be able to initialize" $ do
      Blueprint
        { _bpName = "blueprint"
        , _bpTitle = "bp title"
        , _bpDesc = "bp desc"
        , _bpDevices = Set.empty
        , _bpComponents = []
        , _bpGraph = AG.empty
        , _bpTCPIP = AG.empty
        , _bpProtocols = Map.empty
        } `shouldBe`
        Blueprint
          "blueprint"
          "bp title"
          "bp desc"
          Set.empty
          []
          AG.empty
          AG.empty
          Map.empty
  describe "defaultBlueprint" $ do
    it "returns default value" $ do
      defaultBlueprint `shouldBe`
        Blueprint "" "" "" Set.empty [] AG.empty AG.empty Map.empty
  describe "addDeviceToBp" $ do
    it "add device to _bpGraph" $ do
      let bp = defaultBlueprint
      let device = defaultDevice
      _bpGraph (addDeviceToBp device bp) `shouldBe` (AG.vertex device)
  describe "addDevice" $ do
    context "when default value" $ do
      it "add device to _bpDevices" $ do
        let testBp = addDevice defaultDevice defaultBlueprint
        (testBp ^. bpDevices) `shouldBe` Set.singleton defaultDevice
      it "add device to _bpGraph" $ do
        let testBp = addDevice defaultDevice defaultBlueprint
        (testBp ^? bpGraph) `shouldBe` Just (AG.vertex defaultDevice)
    context "when not hasTCPIP" $ do
      it "not add device to _bpTCPIP" $ do
        let testBp = addDevice defaultDevice defaultBlueprint
        (testBp ^? bpTCPIP) `shouldBe` Just AG.empty
  describe "lens getter" $ do
    describe "bpName" $ do
      it "view _bpName" $ do defaultBlueprint ^. bpName `shouldBe` ""
    describe "bpTitle" $ do
      it "view _bpTitle" $ do defaultBlueprint ^. bpTitle `shouldBe` ""
    describe "bpDesc" $ do
      it "view _bpDesc" $ do defaultBlueprint ^. bpDesc `shouldBe` ""
    describe "bpDevices" $ do
      it "view _bpDevices" $ do
        defaultBlueprint ^. bpDevices `shouldBe` Set.empty
    describe "bpComponents" $ do
      it "view _bpComponents" $ do
        defaultBlueprint ^. bpComponents `shouldBe` []
    describe "bpGraph" $ do
      it "preview _bpGraph" $ do
        defaultBlueprint ^? bpGraph `shouldBe` Just AG.empty
    describe "bpTCPIP" $ do
      it "view _bpTCPIP" $ do
        defaultBlueprint ^? bpTCPIP `shouldBe` Just AG.empty
    describe "bpProtocols" $ do
      it "view _bpProtocols" $ do
        defaultBlueprint ^. bpProtocols `shouldBe` Map.empty
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
