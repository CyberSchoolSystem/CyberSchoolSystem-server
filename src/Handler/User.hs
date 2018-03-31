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
import           Data.HashMap.Lazy (member)
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

{-| Uniquely identifies a user -}
data IdUser = IdChip Text | IdUsername Text -- TODO: Elliminate case statements

data AddUserReq = AddUserReq
    { addFirstName :: Text
    , addLastName :: Text
    , addGrade :: Text
    , addChipId :: Text
    , addUsername :: Maybe Text
    , addPassword :: Maybe Text
    }

data RmUserReq = RmUserReq { rmId :: IdUser } 

data InfoUserReq = InfoUserReq
    { infoFirstName :: Maybe Text
    , infoLastName :: Maybe Text
    , infoGrade :: Maybe Text
    , infoChipId :: Maybe Text
    , infoUsername :: Maybe Text
    }

-- TODO Do not give people full access on this
-- TODO How to handle reseting fails & managing access
data UpdateUserReq = UpdateUserReq
    { idUser :: IdUser
    , updateFirstName :: Maybe Text
    , updateLastName :: Maybe Text
    , updateGrade :: Maybe Text
    , updateChipId :: Maybe Text
    , updateUsername :: Maybe Text
    , updatePassword :: Maybe Text
    }

instance FromJSON IdUser where
    parseJSON (Object v) = val (member "chip" v) (member "username" v)
        where val hasChip hasUser 
                  | hasChip = IdChip <$> v .: "chip"
                  | hasUser = IdUsername <$> v .: "username"
    parseJSON invalid = typeMismatch "IdUser" invalid

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
        <$> v .: "uid"
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
        <$> v .: "uid"
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
rmInvalidArgs req = case rmId req of
                        IdChip chip -> [T.concat ["Chip ID ", chip, " not available"]]
                        IdUsername user -> [T.concat ["Username ", user, "not available"]]

{-| Construct DB Filters according to a RmUserReq -}
rmToFilter :: RmUserReq -> [Filter User]
rmToFilter req = case rmId req of
                     IdChip chip -> [UserChipId ==. chip]
                     IdUsername user -> [UserUsername ==. Just user]

postUserUpdateR :: Handler ()
postUserUpdateR = do
    req <- requireJsonBody :: Handler UpdateUserReq
    user <- runDB $ selectFirst (updateToFilter req) []
    case user of
        Just e -> runDB $ update (entityKey e) (updateToUpdate req)
        Nothing -> invalidArgs $ updateInvalidArgs req

{-| Create Database updates from a update request -}
updateToFilter :: UpdateUserReq -> [Filter User]
updateToFilter u = case idUser u of
                       IdUsername chip -> [UserChipId ==. chip]
                       IdChip user -> [UserUsername ==. Just user]

{-| Create errors from a update request -}
updateInvalidArgs :: UpdateUserReq -> [Text]
updateInvalidArgs u = case idUser u of
                          IdChip chip -> [T.concat ["Unknown chip: ", showt chip]]
                          IdUsername user -> [T.concat ["Unknown username: ", showt user]]

{-| Create a database update from a update request -}
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

{-| Create a database filter from a info request -}
infoToFilter :: InfoUserReq -> [Filter User]
infoToFilter InfoUserReq{..} = catMaybes [ (UserFirstName ==.) <$> infoFirstName
                                         , (UserLastName ==.) <$> infoLastName
                                         , (UserGrade ==.) <$> infoGrade
                                         , (UserChipId ==.) <$> infoChipId
                                         , (UserUsername ==.) <$> Just <$> infoUsername ]
