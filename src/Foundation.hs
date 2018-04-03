{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP                   #-}
module Foundation where

import           Data.Text (Text)
import           Database.Persist ((==.))
import           Database.Persist.Class (selectFirst)
import           Database.Persist.MongoDB
import           Database.Persist.Types (Entity(..))
import           Model (UserId, EntityField (..), Unique(..), User(..), Role(..))
import           Settings
import           Text.Shakespeare.I18N ()
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.HashDB (authHashDB)
import           Yesod.Auth.Dummy (authDummy)
import           Yesod.Core (defaultClientSessionBackend)
import           Yesod.Form.Types (FormMessage)
import           Yesod.Persist.Core (runDB)

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
#ifdef DEVELOPMENT
        defaultClientSessionBackend 5 "client_session_key.aes"
#else
        defaultClientSessionBackend 20 "client_session_key.aes" --TODO: Use Config file
#endif

#ifndef NO_AUTH
    isAuthorized VoteActR _ = isCitizen
    isAuthorized VoteAddR _ = isRepresentative
    isAuthorized VoteRemoveR _ = isRepresentative
    isAuthorized UserAddR _ = isAdmin
    isAuthorized UserRemoveR _ = isAdmin
    isAuthorized UserInfoR _ = isAdmin
    isAuthorized UserUpdateR _ = isAdmin
#endif
    isAuthorized _ _ = return Authorized

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage -- No Translation TODO: Maybe add translation

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = RootR
    logoutDest _ = RootR

    authPlugins app = [ authHashDB (Just . UniqueUser)] ++ extra
        where extra = [authDummy | appDummyLogin $ appSettings app]

    getAuthId creds = runDB $ do
        req <- selectFirst [UserUsername ==. (credsIdent creds)] []
        case req of
            Just (Entity eid _) -> return . Just $ eid
            Nothing -> return Nothing

    authHttpManager = error "W T F"

instance YesodAuthPersist App

instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        y <- getYesod
        runMongoDBPool master action (appConnPool y) -- TODO: Don't use master

isCitizen :: Handler AuthResult
isCitizen = maybeAuthId >>= checkAuth roleCitizen "You are not a citizen"

isRepresentative :: Handler AuthResult
isRepresentative = maybeAuthId >>= checkAuth roleRepresentative "You are not a representative"

isAdmin :: Handler AuthResult
isAdmin = maybeAuthId >>= checkAuth roleAdmin "You are not an admin"

isTeacher :: Handler AuthResult
isTeacher = undefined

{-| Get the roles of a user -}
getRole :: UserId -> Handler Role
getRole u = do
    user <- runDB $ selectFirst [UserId ==. u] []
    case user of
        Just e -> return $ userRoles . entityVal $ e
        Nothing -> undefined

{-| Perform auth checkAuth based on passed function and user id -}
checkAuth :: (Role -> Bool) -> Text -> Maybe UserId -> Handler AuthResult
checkAuth  role failText (Just u) = do
    r <- getRole u
    if role r
        then return Authorized
        else return $ Unauthorized failText
checkAuth _ _ Nothing = return AuthenticationRequired
