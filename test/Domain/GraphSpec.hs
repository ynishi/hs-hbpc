module Domain.GraphSpec
  ( spec
  ) where

import           Domain.Graph
import           Test.Hspec

import qualified Algebra.Graph as G

spec :: Spec
spec = do
  describe "empty" $ do
    it "is 1 graph" $ do G.size e `shouldBe` 1
    it "contains 0 node" $ do G.vertexCount e `shouldBe` 0
    it "contains 0 edge" $ do G.edgeCount e `shouldBe` 0
  describe "single node graph" $ do
    it "is 1 graph" $ do G.size v1 `shouldBe` 1
    it "contains 1 node" $ do G.vertexCount v1 `shouldBe` 1
    it "contains 0 edge" $ do G.edgeCount v1 `shouldBe` 0
  describe "2 single node graph" $ do
    it "is 2 graphs" $ do G.size v12 `shouldBe` 2
    it "contains 2 nodes" $ do G.vertexCount v12 `shouldBe` 2
    it "contains 0 edge" $ do G.edgeCount v12 `shouldBe` 0
  describe "2 single node graph connected" $ do
    it "is 2 graphs" $ do G.size v12e1 `shouldBe` 2
    it "contains 2 nodes" $ do G.vertexCount v12e1 `shouldBe` 2
    it "countains 1 edges" $ do G.edgeCount v12e1 `shouldBe` 1
