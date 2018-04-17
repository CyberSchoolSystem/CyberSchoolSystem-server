{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Settings
    ( AppSettings (..)
    , configSettingsYml
    ) where

import          Data.Aeson
import          Database.Persist.MongoDB (MongoConf)

data AppSettings = AppSettings -- TODO: Add useful settings from Scaffold
    { appDatabaseConf :: MongoConf
    , appDevelopment :: Bool
    , appDummyLogin :: Bool
    , appReload :: Bool
    , appStaticDir :: String
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf <- o .: "database"
        appDevelopment <- o .:? "development" .!= False
        appDummyLogin <- o .:? "dummyLogin" .!= appDevelopment
        appReload <- o .:? "reloadeMode" .!= appDevelopment
        appStaticDir <- o .:? "staticDir" .!= "static/"

        return AppSettings {..}

configSettingsYml :: FilePath
configSettingsYml =
#ifdef DEVELOPMENT
    "config/settings-devel.yml"
#elif TESTING
    "config/settings-test.yml"
#else
    "config/settings.yml"
#endif
