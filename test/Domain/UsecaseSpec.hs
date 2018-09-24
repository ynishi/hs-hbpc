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
          (loadBlueprint db $ LoadBlueprintReq "untitled-1") `shouldReturn`
            (LoadBlueprintRes
               (Right (LoadBlueprintResData "untitled-1" "title" "desc")))
      context "when name exists in" $ do
        it "return Blueprint" $ do
          db <- DT.defaultTVarStore
          addBlueprint db $ AddBlueprintReq "untitled-1" "title1" "desc1"
          addBlueprint db $ AddBlueprintReq "untitled-2" "title2" "desc2"
          (loadBlueprint db $ LoadBlueprintReq "untitled-2") `shouldReturn`
            (LoadBlueprintRes
               (Right (LoadBlueprintResData "untitled-2" "title2" "desc2")))
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
          (SaveBlueprintRes (Right "untitled-1"))
      it "update Blueprint" $ do
        db <- DT.defaultTVarStore
        addBlueprint db $ AddBlueprintReq "untitled-1" "title" "desc"
        (saveBlueprint db $ SaveBlueprintReq "untitled-1" "title1" "desc1") `shouldReturn`
          (SaveBlueprintRes (Right "untitled-1"))
        (loadBlueprint db $ LoadBlueprintReq "untitled-1") `shouldReturn`
          (LoadBlueprintRes
             (Right (LoadBlueprintResData "untitled-1" "title1" "desc1")))
