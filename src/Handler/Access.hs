{-# LANGUAGE OverloadedStrings #-}
module Handler.Access 
    ( postApiAccessInR
    , postApiAccessOutR
    , postApiAccessExportR
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
import           Error
import           Foundation
import           Model
import           Yesod.Core.Json          (requireJsonBody)
import           Yesod.Persist.Core       (runDB)

data AccessUser = AccessUser
    { idUser :: T.Text
    }

instance FromJSON AccessUser where
    parseJSON (Object v) = AccessUser 
        <$> v .: "username"
    parseJSON invalid = typeMismatch "AccessUser" invalid

{-| Access -}
postApiAccessInR :: Handler Value
postApiAccessInR = do
    req <- requireJsonBody :: Handler AccessUser
    user <- runDB $ selectFirst [UserUsername ==. idUser req] []
    addToDB True req user

{-| Check wether use is inside -}
isInside :: User -> Bool
isInside u = accessInside . maximum $ dummy : userAccess u
    where dummy = Access (UTCTime (fromGregorian 1970 1 1) 0) False

{-| Create a new Access with the current time -}
newAccess :: Bool -> IO Access
newAccess dir = Access <$> getCurrentTime <*> return dir

{-| Add a new Access to the db if the user changes state (i.e from inside to outside -}
addToDB :: Bool -> AccessUser -> Maybe (Entity User) -> Handler Value
addToDB direction req user =
    case user of
        Just u -> if direction `xor` (isInside . entityVal $ u)
                      then do
                          access <- liftIO $ newAccess direction
                          runDB $ update (entityKey u) [push UserAccess access]
                          return . toJSON $ (ENull :: Error Value)
                      else return . toJSON $ AlreadyDone "You already are on this side"
                                                        [("username", toJSON $ idUser req)
                                                        ,("inside", toJSON $ direction)]
        Nothing -> return . toJSON $ Unknown "Username not found" [("username", idUser req)]

{-| Leave -}
postApiAccessOutR :: Handler Value
postApiAccessOutR = do
    req <- requireJsonBody :: Handler AccessUser
    user <- runDB $ selectFirst [UserUsername ==. idUser req] []
    addToDB False req user

postApiAccessExportR :: Handler Value
postApiAccessExportR = undefined
