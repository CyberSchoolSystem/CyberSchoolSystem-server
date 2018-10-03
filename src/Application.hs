{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE CPP               #-}
{-# OPTIONS_GHC -Wno-orphans   #-} -- Do not fail at -Werror

module Application
    ( appMain
    , mkFoundation
    ) where

import           Control.Monad (forM)
import           Database.Persist.MongoDB
import           Foundation
import           Handler
import           Migration
import           Network.HostName (getHostName)
import           Settings
import           Model
import           Yesod.Auth
import           Yesod.Core
import           Yesod.Default.Config2 (loadYamlSettings, useEnv)
import           Yesod.Static          (staticDevel, static)

mkYesodDispatch "App" resourcesApp

mkFoundation :: AppSettings -> IO App
mkFoundation appSettings = do
    appConnPool <- createPoolConfig $ appDatabaseConf appSettings
    appHostname <- getHostName
    appStatic <- (if appReload appSettings then staticDevel else static)
                 (appStaticDir appSettings)

    return App {..}

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app query = do
    liftIO $ runMongoDBPool (mgAccessMode $ appDatabaseConf $ appSettings app) query (appConnPool app)

appMain :: IO ()
appMain = do
    settings <- loadYamlSettings [configSettingsYml] [] useEnv
    foundation <- mkFoundation settings
    -- (runMigrator $ runDBwithApp foundation) <$> migrationData
    _ <- migrationData `forM` (runMigrator $ runDBWithApp foundation)
    runDBWithApp foundation $ do
        r <- selectFirst [UserUsername !=. ""] []
        case r of
            Just _ -> return ()
            Nothing -> do
                g <- insert $ Grade "Dark Tower"
                _ <- insert $ User "Crimson" "King" g "jesus"
                        (Just "sha256|17|pZUkwz0cnbtcVxSJBD3qeQ==|D3KVx2A/tAhcy44z2QjcG/FvoV2jEyvKm1nH+dgv0bw=")
                        0 [] Role{roleCitizen = True, roleRepresentative = True,
                                    roleTeacher = Nothing, roleAdmin = True,
                                    roleCustoms = True, roleTech = True}
                return ()

    warp 3000 foundation
