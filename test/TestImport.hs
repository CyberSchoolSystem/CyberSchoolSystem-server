{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module X
    ) where

import           Application
import           Control.Monad.IO.Class (liftIO)
-- import           Database.Persist as X hiding (get)
import           Database.Persist.MongoDB as X hiding (get)
import           Foundation
import           Model
import           Settings (AppSettings(..))
import           Test.Hspec as X
import           Test.QuickCheck as X
import           Yesod.Auth as X
import           Yesod.Core.Dispatch (defaultMiddlewaresNoLogging)
import           Yesod.Default.Config2 (useEnv, loadYamlSettings)
import           Yesod.Test as X

runDB :: Action IO a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userUsername u
        setUrl $ AuthR $ PluginR "dummy" []

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings ["config/settings-test.yml", "config/settings.yml"] [] useEnv
    foundation <- mkFoundation settings
    return (foundation, defaultMiddlewaresNoLogging)

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app query = do
    liftIO $ runMongoDBPool (mgAccessMode $ appDatabaseConf $ appSettings app) query (appConnPool app)
