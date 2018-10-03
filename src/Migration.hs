{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Migration
    ( Migrator()
    , migrationData
    , runMigrator
    ) where

import Control.Monad (forM)
import Data.Text (Text(), toLower)
import Database.Persist.MongoDB (Action())
import Database.Persist ((==.), (!=.), (=.))
import Database.Persist.Class (insert_, selectFirst, selectList, update)
import Database.Persist.Types (Entity(..))
import Model

data Migrator = Migrator
              { migName   :: Text
              , migAction :: Action IO ()
              }

{-| All migrations run at startup |-}
migrationData :: [Migrator]
migrationData =
    [Migrator { migName = "lowercaseUserData", migAction = lowercaseUserDataM }
    ]

{-| Run a migrator with a given runDb action |-}
runMigrator :: (Action IO () -> IO ()) -> Migrator -> IO ()
runMigrator runDB migration = runDB $ do
    r <- selectFirst [MigrationName ==. (migName migration)] []
    case r of
        Nothing -> do
            migAction migration
            insert_ (Migration { migrationName = migName migration })
        Just _ -> return ()

{-| Make saved first and last name all lowercase |-}
lowercaseUserDataM :: Action IO ()
lowercaseUserDataM = do
    users <- selectList [UserUsername !=. ""] []
    users `forM` lowercase
    return ()
    where
        lowercase user =
            update (entityKey user)
                   [ UserFirstName =. (toLower <$> userFirstName . entityVal $ user)
                   , UserLastName =. (toLower <$> userLastName . entityVal $ user)
                   ]
