{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Settings
    ( AppSettings (..)
    ) where

import          Data.Aeson
import          Database.Persist.MongoDB (MongoConf)

data AppSettings = AppSettings -- TODO: Add useful settings from Scaffold
    { appDatabaseConf :: MongoConf
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf <- o .: "database"

        return AppSettings {..}
