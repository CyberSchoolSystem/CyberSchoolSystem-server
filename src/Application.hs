{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Application
    ( appMain
    ) where

import Foundation
import Yesod.Core
import Yesod.Default.Config2 (loadYamlSettings, useEnv, configSettingsYml)
import Handler
import Database.Persist
import Database.Persist.MongoDB
import Settings
import Yesod.Auth

mkYesodDispatch "App" resourcesApp

mkFoundation :: AppSettings -> IO App
mkFoundation appSettings = do
    appConnPool <- createPoolConfig $ appDatabaseConf appSettings

    return App {..}

-- develMain = undefined

appMain :: IO ()
appMain = do
    settings <- loadYamlSettings [configSettingsYml] [] useEnv -- TODO: Maybe do at compile time. Speed?
    foundation <- mkFoundation settings

    warp 3000 foundation
