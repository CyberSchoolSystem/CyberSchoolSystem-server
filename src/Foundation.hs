{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation where

import Yesod
import Database.Persist.MongoDB
import Settings

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App -- TODO: Do more stuff

instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        y <- getYesod
        runMongoDBPool master action (appConnPool y) -- TODO: Don't use master
