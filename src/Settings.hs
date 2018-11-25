{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}
module Settings
    ( AppSettings (..)
    , configSettingsYml
    ) where

import          Data.Aeson
import          Data.Text                (Text)
import          Database.Persist.MongoDB (MongoConf)

data AppSettings = AppSettings -- TODO: Add useful settings from Scaffold
    { appDatabaseConf :: MongoConf
    , appDevelopment :: Bool
    , appDummyLogin :: Bool
    , appReload :: Bool
    , appStaticDir :: String
    , appLoginMinutes :: Int
    , appMOTD :: Text
    , appKeyFile :: FilePath
    , appTmpFilePath :: FilePath
    , appPort :: Int
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf <- o .: "database"
        appDevelopment <- o .:? "development" .!= False
        appDummyLogin <- o .:? "dummyLogin" .!= appDevelopment
        appReload <- o .:? "reloadeMode" .!= appDevelopment
        appStaticDir <- o .:? "staticDir" .!= "static/"
        appLoginMinutes <- o .:? "loginMinutes" .!= 120
        appMOTD <- o .:? "motd" .!= ""
        appKeyFile <- o .: "keyFile" .!= "client_session_key.aes"
        appTmpFilePath <- o .:? "tmpFilePath" .!= "/tmp"
        appPort <- o .:? "port" .!= 3000

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
