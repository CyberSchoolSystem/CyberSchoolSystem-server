{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE CPP                   #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Foundation where

import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as TE
import           Data.Version              (showVersion)
import           Database.Persist          ((==.))
import           Database.Persist.Class    (selectFirst)
import           Database.Persist.MongoDB
import           Database.Persist.Types    (Entity(..))
import qualified Error                     as E
import           Model                     (UserId, EntityField (..), Unique(..), User(..), Role(..))
import           Paths_CyberSchoolSystem_Server (version)
import           Settings
import           Settings.Static
import           Text.Shakespeare.I18N     ()
import           Text.Hamlet               (hamletFile)
import           Text.Jasmine              (minifym)
import           Text.Julius               (juliusFile)
import           Text.Lucius               (luciusFileReload, luciusFile)
import           Yesod
import           Yesod.Auth
import           Yesod.Auth.HashDB         (authHashDBWithForm)
import           Yesod.Auth.Dummy          (authDummy)
import           Yesod.Core                (defaultClientSessionBackend)
import           Yesod.Core.Handler        (withUrlRenderer)
import           Yesod.Core.Widget         (toWidget, whamletFile)
import           Yesod.Default.Util        (addStaticContentExternal)
import           Yesod.Form.Types          (FormMessage)
import           Yesod.Persist.Core        (runDB)
import           Yesod.Static

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    , appStatic   :: Static
    }

mkYesodData "App" $(parseRoutesFile "config/routes")
staticFiles (appStaticDir $ compileTimeAppSettings)

instance Yesod App where -- TODO: SSL
    -- approot = ApprootStatic
    -- yesodMiddleware = (sslOnleMiddleware 20) . defaultYesodMiddleware
    -- makeSessionBackend _ = sslOnlySessions $ fmap Just $
    errorHandler = customErrorHandler
    addStaticContent ext mime content = do
        app <- getYesod
        let staticDir = appStaticDir $ appSettings app
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
        where
            genFileName l = "autogen-" ++ base64md5 l
    makeSessionBackend _ = fmap Just $
#ifdef DEVELOPMENT
        defaultClientSessionBackend 5 "client_session_key.aes"
#else
        defaultClientSessionBackend 20 "client_session_key.aes" --TODO: Use Config file
#endif

#ifndef NO_AUTH
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized

    isAuthorized ApiVoteActR _ = isCitizen
    isAuthorized ApiVoteAddR _ = isRepresentative
    isAuthorized ApiVoteRemoveR _ = isRepresentative
    isAuthorized UiVoteAddR _ = isRepresentative
    isAuthorized UiVoteInfoR _ = isRepresentative

    isAuthorized ApiUserAddR _ = isAdmin
    isAuthorized ApiUserRemoveR _ = isAdmin
    isAuthorized ApiUserInfoR _ = isAdmin
    isAuthorized ApiUserUpdateR _ = isAdmin
    isAuthorized UiUserAddR _ = isAdmin
    isAuthorized UiUserInfoR _ = isAdmin

    isAuthorized ApiGradeAddR _ = isTech
    isAuthorized ApiGradeRemoveR _ = isTech
    isAuthorized UiGradeAddR _ = isTech
    isAuthorized UiGradeInfoR _ = isTech

    isAuthorized UiAccessInR _ = isCustoms
    isAuthorized UiAccessOutR _ = isCustoms
    isAuthorized ApiAccessInR _ = isCustoms
    isAuthorized ApiAccessOutR _ = isCustoms

    isAuthorized _ _ = isAuthenticated
#elif
    isAuthorized _ _ = return Authorized
#endif

    defaultLayout contents = do
        maid <- maybeAuthId
        app <- getYesod
        let css =
                if (appReload . appSettings $ app)
                    then toWidget $(luciusFileReload "templates/defaultLayout.lucius")
                    else toWidget $(luciusFile "templates/defaultLayout.lucius")
            widget = do
                        addStylesheet (StaticR css_bootstrap_min_css)
                        addStylesheet (StaticR css_font_awesome_min_css)
                        addStylesheet (StaticR css_ionicons_min_css)
                        addStylesheet (StaticR css_all_skins_min_css)
                        addStylesheet (StaticR css_AdminLTE_min_css)
                        addStylesheet (StaticR css_morris_css) --Pie Chart
                        addScript (StaticR js_jquery_min_js)
                        addScript (StaticR js_bootstrap_min_js)
                        addScript (StaticR js_jquery_slimscroll_min_js)
                        addScript (StaticR js_fastclick_js)
                        addScript (StaticR js_adminlte_min_js)
                        addScript (StaticR js_demo_js)
                        addScript (StaticR js_morris_min_js) --Pie Chart
                        addScript (StaticR js_raphael_min_js) --Pie Chart
                        addScript (StaticR js_moment_with_locales_js) --Locales in frontend
                        css
                        toWidget $(juliusFile "templates/defaultLayout.julius")
                        contents
        PageContent title headTags bodyTags <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/defaultLayout.hamlet")

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage -- No Translation TODO: Maybe add translation

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = RootR
    logoutDest _ = RootR

    authLayout widget = do
        let cont = do
                addStylesheet (StaticR css_bootstrap_min_css)
                addStylesheet (StaticR css_font_awesome_min_css)
                addStylesheet (StaticR css_ionicons_min_css)
                addStylesheet (StaticR css_AdminLTE_min_css)
                addScript (StaticR js_jquery_min_js)
                addScript (StaticR js_bootstrap_min_js)
                addScript (StaticR js_adminlte_min_js)
                widget
        PageContent title headTags bodyTags <- widgetToPageContent cont
        withUrlRenderer $(hamletFile "templates/loginLayout.hamlet")

    authPlugins app = [ authHashDBWithForm loginForm (Just . UniqueUser)] ++ extra
        where extra = [authDummy | appDummyLogin $ appSettings app]
              loginForm action = $(whamletFile "templates/loginForm.hamlet")

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

isTech :: Handler AuthResult
isTech = maybeAuthId >>= checkAuth roleTech "You are not a tech"

isCustoms :: Handler AuthResult
isCustoms = maybeAuthId >>= checkAuth roleCustoms "You are not a customs"

isAuthenticated :: Handler AuthResult
isAuthenticated = do
    m <- maybeAuthId
    case m of
        Just _ -> return Authorized
        Nothing -> return AuthenticationRequired

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

customErrorHandler :: ErrorResponse -> Handler TypedContent
customErrorHandler NotFound = selectRep $ do
    provideRep $ errorLayout $ do
        toWidget $(hamletFile "templates/404.hamlet")
    provideRep . return . toJSON $ (E.NotFound "NotFound" :: E.Error Value)

customErrorHandler (InternalError e) = selectRep $ do
    provideRep $ errorLayout $ do
        toWidget $(hamletFile "templates/500.hamlet")
    provideRep . return . toJSON $ (E.InternalError "Internal Error" e :: E.Error Value)

customErrorHandler (InvalidArgs e) = selectRep $ do
    provideRep $ errorLayout $ do
        toWidget $(hamletFile "templates/400.hamlet")
    provideRep . return . toJSON $ (E.InvalidArgs "InvalidArgs" e :: E.Error Value)

customErrorHandler NotAuthenticated = selectRep $ do
    provideRep $ errorLayout $ do
        toWidget $(hamletFile "templates/401.hamlet")
    provideRep . return . toJSON $ (E.NotAuthenticated "Not Authenticated" :: E.Error Value)

customErrorHandler (PermissionDenied e) = selectRep $ do
    provideRep $ errorLayout $ do
        toWidget $(hamletFile "templates/403.hamlet")
    provideRep . return . toJSON $ (E.PermissionDenied "Permission Denied" e :: E.Error Value)

customErrorHandler (BadMethod m) = selectRep $ do
    provideRep $ errorLayout $ do
        toWidget $(hamletFile "templates/405.hamlet")
    provideRep . return . toJSON $ (E.BadMethod "Bad Method" (TE.decodeUtf8 m) :: E.Error Value)

errorLayout :: Widget -> Handler Html
errorLayout contents = do
    let widget = do
            addStylesheet (StaticR css_bootstrap_min_css)
            addStylesheet (StaticR css_font_awesome_min_css)
            addStylesheet (StaticR css_ionicons_min_css)
            addStylesheet (StaticR css_AdminLTE_min_css)
            addScript (StaticR js_jquery_min_js)
            addScript (StaticR js_bootstrap_min_js)
            addScript (StaticR js_adminlte_min_js)
            contents

    PageContent title headTags bodyTags <- widgetToPageContent widget
    withUrlRenderer $(hamletFile "templates/errorLayout.hamlet")
