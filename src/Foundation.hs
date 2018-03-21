{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation where

import Yesod
import Yesod.Form.Types (FormMessage)
import Database.Persist.MongoDB
import Text.Shakespeare.I18N ()
import Settings

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App -- TODO: Do more stuff

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage -- No Translation TODO: Maybe add translation

instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        y <- getYesod
        runMongoDBPool master action (appConnPool y) -- TODO: Don't use master
