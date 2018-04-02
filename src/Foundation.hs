{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Foundation where

import Data.Default.Class (def)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist ((==.))
import Database.Persist.Class (selectFirst)
import Database.Persist.MongoDB
import Database.Persist.Types (Entity)
import Model (UserId, EntityField (..), Unique(..), User(..))
import Network.HTTP.Client.Conduit (Manager)
import Settings
import Text.Shakespeare.I18N ()
import Yesod
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB, HashDBUser(..))
import Yesod.Core (defaultClientSessionBackend)
import Yesod.Form.Types (FormMessage)
import Yesod.Persist.Core (runDB)

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where-- TODO: SSL
    -- approot = ApprootStatic
    -- yesodMiddleware = (sslOnleMiddleware 20) . defaultYesodMiddleware
    -- makeSessionBackend _ = sslOnlySessions $ fmap Just $
    makeSessionBackend _ = fmap Just $
        defaultClientSessionBackend 1 "client_session_key.aes"

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage -- No Translation TODO: Maybe add translation

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = RootR
    logoutDest _ = RootR

    authPlugins _ = [ authHashDB (Just . UniqueUser) ]

    getAuthId creds = runDB $ do
        req <- selectFirst [UserUsername ==. (credsIdent creds)] []
        case req of
            Just (Entity eid _) -> return . Just $ eid
            Nothing -> undefined

    authHttpManager = error "W T F"

instance HashDBUser User where
    userPasswordHash = userPassword
    setPasswordHash h u = u{ userPassword = Just h}

instance YesodAuthPersist App

instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        y <- getYesod
        runMongoDBPool master action (appConnPool y) -- TODO: Don't use master
