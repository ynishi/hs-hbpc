module Domain.UsecaseSpec
  ( spec
  ) where

import           Domain.Usecase
import           Test.Hspec

import qualified DataAdapter.TVar       as DT

import qualified Algebra.Graph          as AG
import           Control.Lens           ((&), (.~))
import           Control.Monad.IO.Class

spec :: Spec
spec = do
  describe "usecase" $ do
    describe "createNewBlueprint" $ do
      context "when name not exists" $ do
        it "return name" $ do
          db <- DT.defaultTVarStore
          (createNewBlueprint db CreateNewBlueprintReq) `shouldReturn`
            (CreateNewBlueprintRes "untitled-1")
      context "when name exists" $ do
        it "return another name" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
          (createNewBlueprint db CreateNewBlueprintReq) `shouldReturn`
            (CreateNewBlueprintRes "untitled-2")
    describe "addBlueprint" $ do
      context "when name not exists" $ do
        it "return OK" $ do
          db <- DT.defaultTVarStore
          let abr =
                AddBlueprintReq
                  { _adrName = "untitled-1"
                  , _adrTitle = "title"
                  , _adrDesc = "desc"
                  }
          (addBlueprint db $ abr) `shouldReturn` (AddBlueprintRes (Right "OK"))
      context "when name exists" $ do
        it "return Left with message" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
          let abr =
                AddBlueprintReq
                  { _adrName = "untitled-1"
                  , _adrTitle = "title"
                  , _adrDesc = "desc"
                  }
          (addBlueprint db abr) `shouldReturn`
            (AddBlueprintRes (Left "insert:untitled-1"))
    describe "loadBlueprint" $ do
      context "when name exists" $ do
        it "return Blueprint" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
          loadBlueprint db (LoadBlueprintReq "untitled-1") `shouldReturn`
            LoadBlueprintRes
              (Right (LoadBlueprintResData "untitled-1" "title" "desc"))
      context "when name exists in" $ do
        it "return Blueprint" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title1" "desc1"
          addBlueprint db $ AddBlueprintReq "untitled-2" "title2" "desc2"
          loadBlueprint db (LoadBlueprintReq "untitled-2") `shouldReturn`
            LoadBlueprintRes
              (Right (LoadBlueprintResData "untitled-2" "title2" "desc2"))
    describe "loadBlueprintMaybe" $ do
      context "when name exists" $ do
        it "return Blueprint" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
          loadBlueprintMaybe db (LoadBlueprintReq "untitled-1") `shouldReturn`
            Just (LoadBlueprintResData "untitled-1" "title" "desc")
      context "when name exists in" $ do
        it "return Blueprint" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title1" "desc1"
          addBlueprint db $ AddBlueprintReq "untitled-2" "title2" "desc2"
          loadBlueprintMaybe db (LoadBlueprintReq "untitled-2") `shouldReturn`
            Just (LoadBlueprintResData "untitled-2" "title2" "desc2")
      context "when name not exists" $ do
        it "return Nothing" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
          loadBlueprintMaybe db (LoadBlueprintReq "untitled-2") `shouldReturn`
            Nothing
    describe "saveBlueprint" $ do
      it "save Blueprint" $ do
        db <- DT.defaultTVarStore
        let sbr =
              SaveBlueprintReq
                { _sbrName = "untitled-1"
                , _sbrTitle = "title"
                , _sbrDesc = "desc"
                }
        addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
        (saveBlueprint db sbr) `shouldReturn`
          SaveBlueprintRes (Right "untitled-1")
      it "update Blueprint" $ do
        db <- DT.defaultTVarStore
        addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
        (saveBlueprint db $ SaveBlueprintReq "untitled-1" "title1" "desc1") `shouldReturn`
          (SaveBlueprintRes (Right "untitled-1"))
        loadBlueprint db (LoadBlueprintReq "untitled-1") `shouldReturn`
          LoadBlueprintRes
            (Right (LoadBlueprintResData "untitled-1" "title1" "desc1"))
    describe "loadDevice" $
      it "return Device" $ do
        db <- DT.defaultTVarStore
        registDevice db $ RegistDeviceReq "device-1" "title1" "desc1"
        registDevice db $ RegistDeviceReq "device-2" "title2" "desc2"
        loadDevice db (LoadDeviceReq "device-1") `shouldReturn`
          LoadDeviceRes (Right (LoadDeviceResData "device-1" "title1" "desc1"))
    describe "addDeviceToBlueprint" $
      -- or return data and not save
     do
      it "add device to Blueprint" $ do
        db <- DT.defaultTVarStore
        registDevice db $ RegistDeviceReq "device-1" "title1" "desc1"
        addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
        addDeviceToBlueprint
          db
          (AddDeviceToBlueprintReq "device-1" "untitled-1") `shouldReturn`
          AddDeviceToBlueprintRes (Right "device-1:untitled-1")
      it "add 2 device to Blueprint" $ do
        db2 <- DT.defaultTVarStore
        registDevice db2 $ RegistDeviceReq "device-1" "title1" "desc1"
        registDevice db2 $ RegistDeviceReq "device-2" "title2" "desc2"
        addBlueprint db2 $ AddBlueprintReq "untitled-1" "title" "desc"
        addDeviceToBlueprint db2 $
          AddDeviceToBlueprintReq "device-1" "untitled-1"
        addDeviceToBlueprint
          db2
          (AddDeviceToBlueprintReq "device-2" "untitled-1") `shouldReturn`
          AddDeviceToBlueprintRes (Right "device-2:untitled-1")
        loadBlueprint db2 (LoadBlueprintReq "untitled-1") `shouldReturn`
          LoadBlueprintRes
            (Right (LoadBlueprintResData "untitled-1" "title" "desc"))
      it "add 2 same device to Blueprint" $ do
        db <- DT.defaultTVarStore
        registDevice db $ RegistDeviceReq "device-1" "title1" "desc1"
        addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
        addDeviceToBlueprint db $
          AddDeviceToBlueprintReq "device-1" "untitled-1"
        addDeviceToBlueprint
          db
          (AddDeviceToBlueprintReq "device-1" "untitled-1") `shouldReturn`
          AddDeviceToBlueprintRes (Right "device-1:untitled-1")
        loadBlueprint db (LoadBlueprintReq "untitled-1") `shouldReturn`
          LoadBlueprintRes
            (Right (LoadBlueprintResData "untitled-1" "title" "desc"))
    describe "loadDeviceByBlueprint"
      it "return Devices in Blueprint" $ do
        db <- DT.defaultTVarStore
        registDevice db $ RegistDeviceReq "device-1" "title1" "desc1"
        addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
        addDeviceToBlueprint db $
          AddDeviceToBlueprintReq "device-1" "untitled-1"
        loadDeviceByBlueprint db (LoadDeviceByBlueprintReq "untitled-1") `shouldReturn`
          LoadDeviceByBlueprintRes
            (Right (LoadDeviceResData "device-1" "title1" "desc1"))
