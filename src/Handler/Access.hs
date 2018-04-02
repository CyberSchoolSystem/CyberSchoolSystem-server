{-# LANGUAGE OverloadedStrings #-}
module Handler.Access 
    ( postAccessInR
    , postAccessOutR
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Bits                (xor)
import qualified Data.Text as T
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime(..), getCurrentTime)
import           Database.Persist         ((==.))
import           Database.Persist.Class   (selectFirst, update)
import           Database.Persist.Types   (Entity(..))
import           Database.Persist.MongoDB (push)
import           Foundation
import           Model
import           Yesod.Core.Handler       (invalidArgs, permissionDenied)
import           Yesod.Core.Json          (requireJsonBody)
import           Yesod.Persist.Core       (runDB)

data AccessUser = AccessUser
    { chip :: T.Text
    }

instance FromJSON AccessUser where
    parseJSON (Object v) = AccessUser 
        <$> v .: "chip"
    parseJSON invalid = typeMismatch "AccessUser" invalid

{-| Access -}
postAccessInR :: Handler ()
postAccessInR = do
    req <- requireJsonBody :: Handler AccessUser
    user <- runDB $ selectFirst [UserChipId ==. chip req] []
    addToDB True req user

{-| Check wether use is inside -}
isInside :: User -> Bool
isInside u = accessInside . maximum $ dummy : userAccess u
    where dummy = Access (UTCTime (fromGregorian 1970 1 1) 0) False

{-| Create a new Access with the current time -}
newAccess :: Bool -> IO Access
newAccess dir = Access <$> getCurrentTime <*> return dir

{-| Add a new Access to the db if the user changes state (i.e from inside to outside -}
addToDB :: Bool -> AccessUser -> Maybe (Entity User) -> Handler ()
addToDB direction req user =
    case user of
        Just u -> if direction `xor` (isInside . entityVal $ u)
                      then do
                          access <- liftIO $ newAccess direction
                          runDB $ update (entityKey u) [push UserAccess access]
                      else permissionDenied $ T.concat ["Chip ID ", chip req,
                                " already registered with that state"]
        Nothing -> invalidArgs [T.concat ["Chip ID ", chip req, " does not exist"]]

{-| Leave -}
postAccessOutR :: Handler ()
postAccessOutR = do
    req <- requireJsonBody :: Handler AccessUser
    user <- runDB $ selectFirst [UserChipId ==. chip req] []
    addToDB False req user
