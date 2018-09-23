{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS
  -Wall -Werror -fno-warn-unused-binds -fno-warn-orphans #-}

module Web.Servant
  ( startApp
  , startDefaultApp
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text                as Text
import qualified DataAdapter.TVar         as DT
import qualified Domain.Usecase           as U
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

newtype Req =
  Req String

$(deriveJSON defaultOptions ''U.Res)

$(deriveJSON defaultOptions ''U.Req)

$(deriveJSON defaultOptions ''Req)

fromReq :: Req -> U.Req
fromReq _ = U.Req

instance FromHttpApiData Req where
  parseUrlPiece = Right . Req . Text.unpack

type API
   = Get '[ PlainText] String :<|> "blueprints" :> "all" :> Get '[ JSON] U.Res :<|> "blueprints" :> ReqBody '[ JSON] Req :> Post '[ JSON] U.Res

startDefaultApp :: IO ()
startDefaultApp = startApp 8080

startApp :: Int -> IO ()
startApp port = do
  tVar <- atomically $ newTVar []
  let db = DT.TVarStore tVar
  run port $ app db

app :: (U.Store a) => a -> Application
app db = serve api (server db)

api :: Proxy API
api = Proxy

server :: (U.Store a) => a -> Server API
server db = hello :<|> todoAll :<|> todoPost
  where
    hello = return "Hello"
    todoAll = liftIO $ U.createBlueprint db U.Req
    todoPost req = liftIO $ U.createBlueprint db (fromReq req)
