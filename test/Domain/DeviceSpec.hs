module Domain.DeviceSpec
  ( spec
  ) where

import           Domain.Device
import           Test.Hspec

import qualified Algebra.Graph as AG
import           Control.Lens  ((&), (.~))

spec :: Spec
spec =
  describe "device" $ do
    describe "init" $
      it "init Device" $
      Device
        {_deviceName = "Name", _deviceDesc = "Desc", _deviceIfaces = ["tcpip"]} `shouldBe`
      Device "Name" "Desc" ["tcpip"]
    describe "defaultDevice" $
      it "return Device with default value" $
      defaultDevice `shouldBe` Device "" "" []
    describe "hasTCPIP" $ do
      context "when TCPIP in Ifaces" $ do
        let device = defaultDevice & deviceIfaces .~ ["TCPIP"]
        it "is True" $ hasTCPIP device `shouldBe` True
      context "when TCPIP not in Ifaces" $ do
        let device = defaultDevice & deviceIfaces .~ ["NoTCPIP"]
        it "is False" $ hasTCPIP device `shouldBe` False
    describe "hasProtocol" $ do
      let protocol = "Aproto"
      context "when a protocol in Ifaces" $ do
        let device = defaultDevice & deviceIfaces .~ ["Aproto"]
        it "is True" $ hasProtocol protocol device `shouldBe` True
      context "when a protocol not in Ifaces" $ do
        let device = defaultDevice & deviceIfaces .~ ["Bproto"]
        it "is False" $ hasProtocol protocol device `shouldBe` False
