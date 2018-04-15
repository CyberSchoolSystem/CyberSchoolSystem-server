{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TestImport
    ( module X
    , runDB
    , authenticateAs
    , withApp
    , addUser
    , addUser'
    , citizen
    , nobody
    , everything
    , adm
    , modRep
    , modAdmin
    , modUsername
    , modCit
    , testAuth
    ) where

import           Application
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad
import           Data.Aeson                     as X hiding (Result, Success)
import           Data.ByteString.Lazy.UTF8      as X hiding (decode)
import           Data.Monoid                    as X
import           Data.Text                      as X (Text, isPrefixOf)
import           Data.Time                      (UTCTime)
import           Database.MongoDB.Admin         (dropCollection)
import           Database.MongoDB.Query         (allCollections)
import           Database.Persist.MongoDB       as X hiding (get)
import           Foundation                     as X
import           Model                          as X
import           Network.Wai.Test               as X hiding (request, assertHeader, assertNoHeader)
import           Settings                       (AppSettings(..))
import           Test.Hspec                     as X
import           Test.QuickCheck                as X
import           Test.QuickCheck.Instances()
import           Yesod.Auth                     as X
import           Yesod.Core                     as X
import           Yesod.Core.Dispatch            (defaultMiddlewaresNoLogging)
import           Yesod.Default.Config2          (useEnv, loadYamlSettings)
import           Yesod.Test                     as X

instance Arbitrary User where
    arbitrary = do
        fn     <- arbitrary :: Gen Text
        ln     <- arbitrary :: Gen Text
        grade  <- arbitrary :: Gen Text
        user   <- arbitrary :: Gen Text
        access <- arbitrary :: Gen [Access]
        role   <- arbitrary :: Gen Role
        return User{userFirstName = fn, userLastName = ln, userUsername = user,
                    userPassword = Nothing, userFails = 0, userAccess = access, userRoles = role,
                    userGrade = grade}

instance Arbitrary Access where
    arbitrary = do
        t <- arbitrary :: Gen UTCTime
        b <- arbitrary :: Gen Bool
        return Access{accessTime = t, accessInside = b}

instance Arbitrary Role where
    arbitrary = do
        cit   <- arbitrary :: Gen Bool
        rep   <- arbitrary :: Gen Bool
        admin   <- arbitrary :: Gen Bool
        teach <- arbitrary :: Gen (Maybe Text)
        return Role{roleCitizen = cit, roleRepresentative = rep, roleAdmin = admin, roleTeacher = teach}

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
    void $ wipeDB foundation
    void $ runDBWithApp foundation $ insert_ adm
    return (foundation, defaultMiddlewaresNoLogging)

runDBWithApp :: App -> Action IO a -> IO a
runDBWithApp app query = do
    liftIO $ runMongoDBPool (mgAccessMode $ appDatabaseConf $ appSettings app) query (appConnPool app)

adm :: User
adm = User "" "" "" "adm" Nothing 0 [] everything

wipeDB :: App -> IO ()
wipeDB app = void $ runDBWithApp app dropAllCollections

dropAllCollections :: (MonadIO m, MonadBaseControl IO m) => Action m [Bool]
dropAllCollections = allCollections >>=
    return . filter (not . isSystemCollection) >>= mapM dropCollection
        where                           
          isSystemCollection = isPrefixOf "system."


addUser :: Text -> Text -> Text -> Text -> Maybe Text -> Role -> YesodExample App (Key User)
addUser fn ln grade user pw role = runDB . insert $ User fn ln grade user pw 0 [] role

addUser' :: User -> YesodExample App (Key User)
addUser' = runDB . insert

citizen :: Role
citizen = Role { roleCitizen = True
               , roleRepresentative = False
               , roleAdmin = False
               , roleTeacher = Nothing
               }

everything :: Role
everything = Role { roleCitizen = True
                  , roleRepresentative = True
                  , roleAdmin = True
                  , roleTeacher = Just ""
                  }

nobody :: Role
nobody = Role { roleCitizen = False
              , roleRepresentative = False
              , roleAdmin = False
              , roleTeacher = Nothing
              }


testAuth :: (Bool -> User -> User) -> Route App -> YesodExample App ()
testAuth m route = do
    u <- fmap (m True) .  liftIO $ generate (arbitrary :: Gen User)
    u'  <- fmap (m False) . liftIO $ generate (arbitrary :: Gen User)
    runDB $ insert_ u
    runDB $ insert_ u'

    authenticateAs (Entity undefined u)
    post route
    statusIs 400 -- malformed JSON yields 400

    authenticateAs (Entity undefined u')
    post route
    statusIs 403

modRep :: Bool -> User -> User
modRep b u = u{userRoles = modRep' b $ userRoles u}

modRep' :: Bool -> Role -> Role
modRep' b r = r{roleRepresentative = b}

modCit :: Bool -> User -> User
modCit b u = u{userRoles = modCit' b $ userRoles u}

modCit' :: Bool -> Role -> Role
modCit' b r = r{roleCitizen = b}

modAdmin :: Bool -> User -> User
modAdmin b u = u{userRoles = modAdmin' b $ userRoles u}

modUsername :: Text -> User -> User
modUsername n u = u{userUsername = n}

modAdmin' :: Bool ->  Role -> Role
modAdmin' b r = r{roleAdmin = b}
