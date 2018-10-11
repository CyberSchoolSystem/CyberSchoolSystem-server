{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Foundation where

import           Control.Monad          (forM_)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Time.Clock           (getCurrentTime)
import           Data.Version              (showVersion)
import           Database.Persist          ((==.))
import           Database.Persist.Class    (selectFirst)
import           Database.Persist.MongoDB
import           Database.Persist.Types    (Entity(..))
import qualified Error                     as E
import qualified Message                   as M
import           Model
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
import           Yesod.Auth.Dummy          (authDummy)
import           Yesod.Auth.HashDB         (authHashDBWithForm)
import           Yesod.Auth.Message        (germanMessage, AuthMessage(..))
import           Yesod.Core                (defaultClientSessionBackend)
import           Yesod.Core.Handler        (withUrlRenderer, setUltDestCurrent, redirect, getMessage)
import           Yesod.Core.Widget         (toWidget, whamletFile)
import           Yesod.Default.Util        (addStaticContentExternal)
import           Yesod.Form.Types          (FormMessage)
import           Yesod.Persist.Core        (runDB)
import           Yesod.Static

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    , appStatic   :: Static
    , appHostname :: String
    }

mkYesodData "App" $(parseRoutesFile "config/routes")
staticFiles (appStaticDir $ compileTimeAppSettings)

instance Yesod App where -- TODO: SSL
    -- approot = ApprootStatic
    -- yesodMiddleware = (sslOnleMiddleware 20) . defaultYesodMiddleware
    -- makeSessionBackend _ = sslOnlySessions $ fmap Just $
    yesodMiddleware = fileRemover  . defaultYesodMiddleware
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
    makeSessionBackend app = fmap Just $
        defaultClientSessionBackend
            (appLoginMinutes settings)
            (appKeyFile settings)
        where settings = appSettings app

#ifndef NO_AUTH
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized

    isAuthorized ApiVoteActR _ = isCitizen
    isAuthorized ApiVoteAddR _ = isRepresentative
    isAuthorized ApiVoteRemoveR _ = isRepresentative
    isAuthorized UiVoteAddR _ = isRepresentative
    isAuthorized UiVoteTerminatedR _ = isCitizen
    isAuthorized UiVoteRunningR _ = isCitizen

    isAuthorized ApiUserAddR _ = isTech
    isAuthorized ApiUserRemoveR _ = isTech
    isAuthorized ApiUserInfoR _ = isAdmin
    isAuthorized ApiUserUpdateR _ = isAdmin
    isAuthorized UiUserAddR _ = isTech
    isAuthorized UiUserInfoR _ = isAdmin

    isAuthorized ApiGradeAddR _ = isTech
    isAuthorized ApiGradeRemoveR _ = isTech
    isAuthorized UiGradeAddR _ = isTech
    isAuthorized UiGradeInfoR _ = isTech

    isAuthorized UiAccessR _ = isCustoms
    isAuthorized ApiAccessInR _ = isCustoms
    isAuthorized ApiAccessOutR _ = isCustoms
    isAuthorized ApiAccessExportR _ = isTeacher
    isAuthorized UiAccessExportR _ = isTeacher

    isAuthorized _ _ = isAuthenticated
#else
    isAuthorized _ _ = return Authorized -- TODO Use config file
#endif

    defaultLayout contents = do
        maid <- maybeAuthId
        app <- getYesod
        tech <- authResultToBool <$> isTech
        admin <- authResultToBool <$> isAdmin
        teach <- authResultToBool <$> isTeacher
        citizen <- authResultToBool <$> isCitizen
        customs <- authResultToBool <$> isCustoms
        representative <- authResultToBool <$> isRepresentative
        mmsg <- getMessage
        let motd = appMOTD . appSettings $ app
            hasMotd = motd /= ""
            hostname = appHostname app
            css =
                if (appReload . appSettings $ app)
                    then toWidget $(luciusFileReload "templates/defaultLayout.lucius")
                    else toWidget $(luciusFile "templates/defaultLayout.lucius")
            widget = do
                        addStylesheet (StaticR css_bootstrap_min_css)
                        addStylesheet (StaticR css_font_awesome_min_css)
                        addStylesheet (StaticR css_ionicons_min_css)
                        addStylesheet (StaticR css_all_skins_min_css)
                        addStylesheet (StaticR css_AdminLTE_min_css)
                        addStylesheet (StaticR css_morris_css) -- Pie Chart
                        addStylesheet (StaticR css_bootstrap_datetimepicker_min_css) -- required for DateTimePicker
                        addScript (StaticR js_jquery_min_js)
                        addScript (StaticR js_moment_js) -- required for DateTimePicker
                        addScript (StaticR js_locale_de_js) -- German format for DateTimePicker
                        addScript (StaticR js_bootstrap_min_js)
                        addScript (StaticR js_bootstrap_datetimepicker_js) -- required for DateTimePicker
                        addScript (StaticR js_jquery_slimscroll_min_js)
                        addScript (StaticR js_fastclick_js)
                        addScript (StaticR js_adminlte_min_js)
                        addScript (StaticR js_demo_js)
                        addScript (StaticR js_raphael_min_js) -- Pie Chart
                        addScript (StaticR js_chart_min_js)
                        css
                        toWidget $(juliusFile "templates/defaultLayout.julius")
                        contents
        PageContent title headTags bodyTags <- widgetToPageContent widget
        withUrlRenderer $(hamletFile "templates/defaultLayout.hamlet")

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage App M.Message where
    renderMessage _ _ = M.fromMessage

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = RootR
    logoutDest _ = RootR
    renderAuthMessage _ _ = germanMessage

    authLayout widget = do
        maid <- maybeAuthId
        mmsg <- getMessage
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
        case maid of
            Just _ -> redirect RootR
            Nothing -> withUrlRenderer $(hamletFile "templates/loginLayout.hamlet")

    authPlugins app = [authHashDBWithForm loginForm (Just . UniqueUser . T.toLower)] ++ extra
        where extra = [authDummy | appDummyLogin $ appSettings app]
              loginForm action = $(whamletFile "templates/loginForm.hamlet")

    authenticate creds = do
        req <- runDB $ selectFirst [UserUsername ==. (T.toLower . credsIdent $ creds)] []
        case req of
            Just (Entity eid _) -> return $ Authenticated eid
            Nothing             -> return . UserError $ IdentifierNotFound (credsIdent creds)

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
isTeacher = maybeAuthId >>= checkAuth isTeach "You are not a teach"
    where
        isTeach Role{roleTeacher = t} =
            case t of
                Nothing -> False
                Just _ -> True

authResultToBool :: AuthResult -> Bool
authResultToBool a = case a of
                         Authorized -> True
                         _          -> False
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

{-| Remove files if TTL is over -}
fileRemover :: HandlerT App IO res -> HandlerT App IO res
fileRemover handler = do
    now <- liftIO $ getCurrentTime
    files <- runDB $ selectList [FileFilePath !=. ""] []
    let toRemove = entityKey <$> filter destroy files
        destroy a = (fileDestroy . entityVal $ a) < now
    runDB $ forM_ toRemove delete
    handler

-- TODO: Convert to Message
customErrorHandler :: ErrorResponse -> Handler TypedContent
customErrorHandler NotFound = selectRep $ do
    provideRep $ defaultLayout $ do
        toWidget $(hamletFile "templates/404.hamlet")
    provideRep . return . toJSON $ (E.NotFound "NotFound" :: E.Error Value)

customErrorHandler (InternalError e) = selectRep $ do
    provideRep $ defaultLayout $ do
        toWidget $(hamletFile "templates/500.hamlet")
    provideRep . return . toJSON $ (E.InternalError "Internal Error" e :: E.Error Value)

customErrorHandler (InvalidArgs e) = selectRep $ do
    provideRep $ defaultLayout $ do
        toWidget $(hamletFile "templates/400.hamlet")
    provideRep . return . toJSON $ (E.InvalidArgs "InvalidArgs" e :: E.Error Value)

customErrorHandler NotAuthenticated = selectRep $ do
    provideRep $ defaultLayout $ do
        setUltDestCurrent
        redirect (AuthR LoginR)
    provideRep . return . toJSON $ (E.NotAuthenticated "Not Authenticated" :: E.Error Value)

customErrorHandler (PermissionDenied e) = selectRep $ do
    provideRep $ defaultLayout $ do
        toWidget $(hamletFile "templates/403.hamlet")
    provideRep . return . toJSON $ (E.PermissionDenied "Permission Denied" e :: E.Error Value)

customErrorHandler (BadMethod m) = selectRep $ do
    provideRep $ defaultLayout $ do
        toWidget $(hamletFile "templates/405.hamlet")
    provideRep . return . toJSON $ (E.BadMethod "Bad Method" (TE.decodeUtf8 m) :: E.Error Value)
