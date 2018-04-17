{-# LANGUAGE TemplateHaskell #-}
module Settings.Static
    ( compileTimeAppSettings
    ) where

import qualified Control.Exception       as E
import           Data.Aeson               (Result(..), Value, fromJSON)
import           Data.ByteString          (ByteString)
import           Data.FileEmbed           (embedFile)
import           Data.Monoid              (mempty)
import           Data.Yaml                (decodeEither')
import           Data.Yaml.Config         (applyEnvValue)
import           Settings

{-| Raw bytes at compile time of @config/settings.yml@ -}
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

{-| @config/settings.yml@, parsed to a @Value@. -}
configSettingsYmlValue :: Value
configSettingsYmlValue = either E.throw id
                        $ decodeEither' configSettingsYmlBS

{- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@. -}
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

