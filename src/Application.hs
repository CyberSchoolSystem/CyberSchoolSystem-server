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

import           Database.Persist.MongoDB
import           Foundation
import           Handler
import           Settings
import           Yesod.Auth
import           Yesod.Core
import           Yesod.Default.Config2 (loadYamlSettings, useEnv, configSettingsYml)

mkYesodDispatch "App" resourcesApp

mkFoundation :: AppSettings -> IO App
mkFoundation appSettings = do
    appConnPool <- createPoolConfig $ appDatabaseConf appSettings

    return App {..}

-- develMain = undefined

appMain :: IO ()
appMain = do
    settings <- loadYamlSettings [
#ifdef DEVELOPMENT
                                 "config/settings-devel.yml" ,
#endif
                                 configSettingsYml
                                 ] [] useEnv -- TODO: Maybe do at compile time. Speed?
    foundation <- mkFoundation settings

    warp 3000 foundation
