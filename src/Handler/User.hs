{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.User
    ( postUserAddR
    , postUserRemoveR
    , postUserInfoR
    , postUserUpdateR
    ) where

import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.HashMap.Lazy      (member)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Database.Persist       ((==.), (=.))
import           Database.Persist.Class (insert_, selectFirst, delete, update, selectList)
import           Database.Persist.Types (Filter, Entity(..), Update)
import           Error
import           Foundation
import           Model
import           Yesod.Core.Json        (requireJsonBody)
import           Yesod.Persist.Core

{-| Uniquely identifies a user -}
data IdUser = IdChip Text | IdUsername Text -- TODO: Elliminate case statements

data AddUserReq = AddUserReq
    { addFirstName :: Text
    , addLastName :: Text
    , addGrade :: Text
    , addChipId :: Text
    , addUsername :: Text
    , addPassword :: Maybe Text
    , addRoles :: Role
    }

data RmUserReq = RmUserReq { rmId :: IdUser } 

data InfoUserReq = InfoUserReq
    { infoFirstName :: Maybe Text
    , infoLastName :: Maybe Text
    , infoGrade :: Maybe Text
    , infoChipId :: Maybe Text
    , infoUsername :: Maybe Text
    , infoRole :: Maybe Role
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
    , updateRole :: Maybe Role
    }

instance FromJSON IdUser where
    parseJSON (Object v) = val (member "chip" v) (member "username" v)
        where val hasChip hasUser 
                  | hasChip = IdChip <$> v .: "chip"
                  | hasUser = IdUsername <$> v .: "username"
              val _ _ = fail "uid object has neither 'chip' nor 'username' field"
    parseJSON invalid = typeMismatch "IdUser" invalid

instance FromJSON AddUserReq where
    parseJSON (Object v) = AddUserReq
        <$> v .: "firstName"
        <*> v .: "lastName"
        <*> v .: "grade"
        <*> v .: "chip"
        <*> v .: "username"
        <*> v .:? "password"
        <*> v .: "role"
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
        <*> v .:? "role"
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
        <*> v .:? "role"
    parseJSON invalid = typeMismatch "UpdateUserReq" invalid

postUserAddR :: Handler Value
postUserAddR = do
    user <- addToUser <$> requireJsonBody
    runDB $ insert_ user
    return Null

{-| Transform a request to a User -}
addToUser :: AddUserReq -> User
addToUser AddUserReq{..} = User
    { userFirstName = addFirstName
    , userLastName = addLastName
    , userGrade = addGrade
    , userChipId = addChipId
    , userUsername = addUsername
    , userPassword = addPassword -- TODO: Hash
    , userRoles = addRoles
    , userFails = 0
    , userAccess = []
    }

postUserRemoveR :: Handler Value
postUserRemoveR = do
    req <- requireJsonBody :: Handler RmUserReq
    del <- runDB $ selectFirst (rmToFilter req) []
    case del of
        Just e -> runDB $ delete (entityKey e) >>= (\_ -> return Null)
        Nothing -> return . toJSON . WrongFieldValue $ rmInvalidArgs req

{-| Construct invalid args error message, according to a RmUserReq -}
rmInvalidArgs :: RmUserReq -> [(Text,Text)]
rmInvalidArgs req = case rmId req of
                        IdChip chip -> [("chip", chip)]
                        IdUsername user -> [("username", user)]

{-| Construct DB Filters according to a RmUserReq -}
rmToFilter :: RmUserReq -> [Filter User]
rmToFilter req = case rmId req of
                     IdChip chip -> [UserChipId ==. chip]
                     IdUsername user -> [UserUsername ==. user]

postUserUpdateR :: Handler Value
postUserUpdateR = do
    req <- requireJsonBody :: Handler UpdateUserReq
    user <- runDB $ selectFirst (updateToFilter req) []
    case user of
        Just e -> runDB $ update (entityKey e) (updateToUpdate req) >>= (\_ -> return Null)
        Nothing -> return . toJSON . WrongFieldValue  $ updateInvalidArgs req

{-| Create Database updates from a update request -}
updateToFilter :: UpdateUserReq -> [Filter User]
updateToFilter u = case idUser u of
                       IdUsername chip -> [UserChipId ==. chip]
                       IdChip user -> [UserUsername ==. user]

{-| Create errors from a update request -}
updateInvalidArgs :: UpdateUserReq -> [(Text,Text)]
updateInvalidArgs u = case idUser u of
                          IdChip chip -> [("chip", chip)]
                          IdUsername user -> [("username", user)]

{-| Create a database update from a update request -}
updateToUpdate :: UpdateUserReq -> [Update User]
updateToUpdate UpdateUserReq{..} = catMaybes [ (UserFirstName =.) <$> updateFirstName
                                             , (UserLastName =.) <$> updateLastName
                                             , (UserGrade =.) <$> updateGrade
                                             , (UserChipId =.) <$> updateChipId
                                             , (UserUsername =.) <$> updateUsername
                                             , (UserPassword =.) <$> Just <$> updatePassword
                                             , (UserRoles =.) <$> updateRole ] -- TODO: Make more flexible

postUserInfoR :: Handler Value
postUserInfoR = do
    req <- requireJsonBody :: Handler InfoUserReq
    users <- runDB $ selectList (infoToFilter req) []
    return $ toJSON users

{-| Create a database filter from a info request -}
infoToFilter :: InfoUserReq -> [Filter User]
infoToFilter InfoUserReq{..} = catMaybes [ (UserFirstName ==.) <$> infoFirstName
                                         , (UserLastName ==.) <$> infoLastName
                                         , (UserGrade ==.) <$> infoGrade
                                         , (UserChipId ==.) <$> infoChipId
                                         , (UserUsername ==.) <$> infoUsername
                                         , (UserRoles ==.) <$> infoRole ]
