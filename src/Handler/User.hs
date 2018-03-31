{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.User
    ( postUserAddR
    , postUserRemoveR
    , getUserInfoR
    , postUserUpdateR
    ) where

import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text as T
import           Database.Persist       ((==.), (=.))
import           Database.Persist.Class (insert_, selectFirst, delete, update, selectList)
import           Database.Persist.Types (Filter, Entity(..), Update)
import           Foundation
import           Model
import           Text.Blaze.Html        (Html)
import           TextShow               (showt)
import           Yesod.Core.Handler     (invalidArgs)
import           Yesod.Core.Json        (requireJsonBody)
import           Yesod.Persist.Core

data AddUserReq = AddUserReq
    { addFirstName :: Text
    , addLastName :: Text
    , addGrade :: Text
    , addChipId :: Text
    , addUsername :: Maybe Text
    , addPassword :: Maybe Text
    }

data RmUserReq = RmUserReq
    { rmChipId :: Maybe Text
    , rmUsername :: Maybe Text
    }

data InfoUserReq = InfoUserReq
    { infoFirstName :: Maybe Text
    , infoLastName :: Maybe Text
    , infoGrade :: Maybe Text
    , infoChipId :: Maybe Text
    , infoUsername :: Maybe Text
    }

-- TODO Do not give people full access on this
-- TODO How to handle resting fails & managing access
type ChipId = Text
type Username = Text
data UpdateUserReq = UpdateUserReq
    { idUser :: Either ChipId Username
    , updateFirstName :: Maybe Text
    , updateLastName :: Maybe Text
    , updateGrade :: Maybe Text
    , updateChipId :: Maybe Text
    , updateUsername :: Maybe Text
    , updatePassword :: Maybe Text
    }

instance FromJSON AddUserReq where
    parseJSON (Object v) = AddUserReq
        <$> v .: "firstName"
        <*> v .: "lastName"
        <*> v .: "grade"
        <*> v .: "chip"
        <*> v .:? "username"
        <*> v .:? "password"
    parseJSON invalid = typeMismatch "AddUserReq" invalid

instance FromJSON RmUserReq where
    parseJSON (Object v) = RmUserReq
        <$> v .:? "chip"
        <*> v .:? "username"
    parseJSON invalid = typeMismatch "RmUserReq" invalid

instance FromJSON InfoUserReq where
    parseJSON (Object v) = InfoUserReq
        <$> v .:? "firstName"
        <*> v .:? "lastName"
        <*> v .:? "grade"
        <*> v .:? "chip"
        <*> v .:? "username"
    parseJSON invalid = typeMismatch "InfoUserReq" invalid

instance FromJSON UpdateUserReq where
    parseJSON (Object v) = UpdateUserReq
        <$> v .: "id"
        <*> v .:? "firstName"
        <*> v .:? "lastName"
        <*> v .:? "grade"
        <*> v .:? "chip"
        <*> v .:? "username"
        <*> v .:? "password"
    parseJSON invalid = typeMismatch "UpdateUserReq" invalid

postUserAddR :: Handler ()
postUserAddR = do
    user <- addToUser <$> requireJsonBody
    runDB $ insert_ user

{-| Transform a request to a User -}
addToUser :: AddUserReq -> User
addToUser AddUserReq{..} = User
    { userFirstName = addFirstName
    , userLastName = addLastName
    , userGrade = addGrade
    , userChipId = addChipId
    , userUsername = addUsername
    , userPassword = addPassword
    , userFails = 0
    , userAccess = []
    }

postUserRemoveR :: Handler ()
postUserRemoveR = do
    req <- requireJsonBody :: Handler RmUserReq
    del <- runDB $ selectFirst (rmToFilter req) []
    case del of
        Just e -> runDB $ delete (entityKey e)
        Nothing -> invalidArgs $ rmInvalidArgs req

{-| Construct invalid args error message, according to a RmUserReq -}
rmInvalidArgs :: RmUserReq -> [Text]
rmInvalidArgs req = catMaybes [ (\x -> T.concat ["Chip ", showt x, " nonexistent"]) <$> rmChipId req
                              , (\x -> T.concat ["Username ", showt x, " nonexistent"]) 
                                <$> rmUsername req ]

{-| Construct DB Filters according to a RmUserReq -}
rmToFilter :: RmUserReq -> [Filter User]
rmToFilter req = catMaybes [ (UserUsername ==.) <$> Just (rmUsername req) -- FIXME
                           , (UserChipId ==.) <$> rmChipId req ]

postUserUpdateR :: Handler ()
postUserUpdateR = do
    req <- requireJsonBody :: Handler UpdateUserReq
    user <- runDB $ selectFirst (updateToFilter req) []
    case user of
        Just e -> runDB $ update (entityKey e) (updateToUpdate req)
        Nothing -> invalidArgs $ updateInvalidArgs req

updateToFilter :: UpdateUserReq -> [Filter User]
updateToFilter u = case idUser u of
                       Left chip -> [UserChipId ==. chip]
                       Right user -> [UserUsername ==. Just user]

updateInvalidArgs :: UpdateUserReq -> [Text]
updateInvalidArgs u = case idUser u of
                          Left chip -> [T.concat ["Unknown chip: ", showt chip]]
                          Right user -> [T.concat ["Unknown username: ", showt user]]

-- TODO Use ListT
updateToUpdate :: UpdateUserReq -> [Update User]
updateToUpdate UpdateUserReq{..} = catMaybes [ (UserFirstName =.) <$> updateFirstName
                                             , (UserLastName =.) <$> updateLastName
                                             , (UserGrade =.) <$> updateGrade
                                             , (UserChipId =.) <$> updateChipId
                                             , (UserUsername =.) <$> Just <$> updateUsername
                                             , (UserPassword =.) <$> Just <$> updatePassword ]

getUserInfoR :: Handler Value
getUserInfoR = do
    req <- requireJsonBody :: Handler InfoUserReq
    users <- runDB $ selectList (infoToFilter req) []
    return $ toJSON users

infoToFilter :: InfoUserReq -> [Filter User]
infoToFilter InfoUserReq{..} = catMaybes [ (UserFirstName ==.) <$> infoFirstName
                                         , (UserLastName ==.) <$> infoLastName
                                         , (UserGrade ==.) <$> infoGrade
                                         , (UserChipId ==.) <$> infoChipId
                                         , (UserUsername ==.) <$> Just <$> infoUsername ]
