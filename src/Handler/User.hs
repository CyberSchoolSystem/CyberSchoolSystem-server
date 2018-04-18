{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.User
    ( postApiUserAddR
    , postApiUserRemoveR
    , postApiUserInfoR
    , postApiUserUpdateR
    , getApiUserSelfInfoR
    , postApiUserSelfSetPwR
    ) where

import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Database.Persist       ((==.), (=.))
import           Database.Persist.Class (insert_, selectFirst, delete, update, selectList)
import           Database.Persist.Types (Filter, Entity(..), Update)
import           Error
import           Foundation
import           Model
import           Yesod.Auth             (requireAuthId)
import           Yesod.Core.Json        (requireJsonBody)
import           Yesod.Persist.Core

{- Represent requests by users/admins. Used to tune the toJSON instance -}
newtype AdminResp a = AdminResp { admGetUsr :: a }
newtype UserResp a = UserResp { usrGetUsr :: a }

data AddUserReq = AddUserReq
    { addFirstName :: Text
    , addLastName :: Text
    , addGrade :: Key Grade
    , addUsername :: Text
    , addPassword :: Maybe Text
    , addRoles :: Role
    }

data RmUserReq = RmUserReq { rmId :: Text } 

data InfoUserReq = InfoUserReq
    { infoFirstName :: Maybe Text
    , infoLastName :: Maybe Text
    , infoGrade :: Maybe (Key Grade)
    , infoUsername :: Maybe Text
    , infoRole :: Maybe Role
    }

-- TODO Do not give people full access on this
-- TODO How to handle reseting fails & managing access
data UpdateUserReq = UpdateUserReq
    { idUsername :: Text
    , updateFirstName :: Maybe Text
    , updateLastName :: Maybe Text
    , updateGrade :: Maybe (Key Grade)
    , updateUsername :: Maybe Text
    , updatePassword :: Maybe Text
    , updateRole :: Maybe Role
    }

data UpdatePwReq = UpdatePwReq
    { newPw :: Text
    }

instance FromJSON AddUserReq where
    parseJSON (Object v) = AddUserReq
        <$> v .: "firstName"
        <*> v .: "lastName"
        <*> v .: "gradeId"
        <*> v .: "username"
        <*> v .:? "password"
        <*> v .: "role"
    parseJSON invalid = typeMismatch "AddUserReq" invalid

instance FromJSON RmUserReq where
    parseJSON (Object v) = RmUserReq
        <$> v .: "idUsername"
    parseJSON invalid = typeMismatch "RmUserReq" invalid

instance FromJSON InfoUserReq where
    parseJSON (Object v) = InfoUserReq
        <$> v .:? "firstName"
        <*> v .:? "lastName"
        <*> v .:? "gradeId"
        <*> v .:? "username"
        <*> v .:? "role"
    parseJSON invalid = typeMismatch "InfoUserReq" invalid

instance FromJSON UpdateUserReq where
    parseJSON (Object v) = UpdateUserReq
        <$> v .: "idUsername"
        <*> v .:? "firstName"
        <*> v .:? "lastName"
        <*> v .:? "gradeId"
        <*> v .:? "username"
        <*> v .:? "password"
        <*> v .:? "role"
    parseJSON invalid = typeMismatch "UpdateUserReq" invalid

instance FromJSON UpdatePwReq where
    parseJSON (Object v) = UpdatePwReq <$> v .: "password"
    parseJSON invalid = typeMismatch "UpdateUserReq" invalid

instance ToJSON (AdminResp User) where
    toJSON AdminResp{admGetUsr = u} = object
        [ "firstName" .= userFirstName u
        , "lastName" .= userLastName u
        , "gradeId" .= userGrade u
        , "username" .= userUsername u
        , "roles" .= userRoles u
        ]

instance ToJSON (UserResp User) where
    toJSON UserResp{usrGetUsr = u} = object
        [ "firstName" .= userFirstName u
        , "lastName" .= userLastName u
        , "gradeId" .= userGrade u
        , "username" .= userUsername u
        , "fails" .= userFails u
        , "access" .= userAccess u
        , "roles" .= userRoles u
        ]


postApiUserAddR :: Handler Value
postApiUserAddR = do
    user <- addToUser <$> requireJsonBody
    runDB $ insert_ user
    return Null

{-| Transform a request to a User -}
addToUser :: AddUserReq -> User
addToUser AddUserReq{..} = User
    { userFirstName = addFirstName
    , userLastName = addLastName
    , userGrade = addGrade
    , userUsername = addUsername
    , userPassword = addPassword -- TODO: Hash
    , userRoles = addRoles
    , userFails = 0
    , userAccess = []
    }

postApiUserRemoveR :: Handler Value
postApiUserRemoveR = do
    req <- requireJsonBody :: Handler RmUserReq
    del <- runDB $ selectFirst (rmToFilter req) []
    case del of
        Just e -> runDB $ delete (entityKey e) >> return Null
        Nothing -> return . toJSON . Unknown "User not known" $ rmInvalidArgs req

{-| Construct invalid args error message, according to a RmUserReq -}
rmInvalidArgs :: RmUserReq -> [(Text,Text)]
rmInvalidArgs req = [("username", rmId req)]

{-| Construct DB Filters according to a RmUserReq -}
rmToFilter :: RmUserReq -> [Filter User]
rmToFilter req = [UserUsername ==. rmId req]

postApiUserUpdateR :: Handler Value
postApiUserUpdateR = do
    req <- requireJsonBody :: Handler UpdateUserReq
    user <- runDB $ selectFirst (updateToFilter req) []
    case user of
        Just e -> runDB $ update (entityKey e) (updateToUpdate req) >> return Null
        Nothing -> return . toJSON . Unknown "User not known"  $ updateInvalidArgs req

{-| Create Database updates from a update request -}
updateToFilter :: UpdateUserReq -> [Filter User]
updateToFilter u =  [UserUsername ==. idUsername u]

{-| Create errors from a update request -}
updateInvalidArgs :: UpdateUserReq -> [(Text,Text)]
updateInvalidArgs u = [("username", idUsername u)]

{-| Create a database update from a update request -}
updateToUpdate :: UpdateUserReq -> [Update User]
updateToUpdate UpdateUserReq{..} = catMaybes [ (UserFirstName =.) <$> updateFirstName
                                             , (UserLastName =.) <$> updateLastName
                                             , (UserGrade =.) <$> updateGrade
                                             , (UserUsername =.) <$> updateUsername
                                             , (UserPassword =.) <$> Just <$> updatePassword
                                             , (UserRoles =.) <$> updateRole ] -- TODO: Make more flexible

postApiUserInfoR :: Handler Value
postApiUserInfoR = do
    req <- requireJsonBody :: Handler InfoUserReq
    users <- runDB $ selectList (infoToFilter req) []
    return . toJSON $ map (AdminResp . entityVal) users

{-| Create a database filter from a info request -}
infoToFilter :: InfoUserReq -> [Filter User]
infoToFilter InfoUserReq{..} = catMaybes [ (UserFirstName ==.) <$> infoFirstName
                                         , (UserLastName ==.) <$> infoLastName
                                         , (UserGrade ==.) <$> infoGrade
                                         , (UserUsername ==.) <$> infoUsername
                                         , (UserRoles ==.) <$> infoRole ]

{-| Get information about the current user -}
getApiUserSelfInfoR :: Handler Value
getApiUserSelfInfoR = do
    auth <- requireAuthId
    user <- runDB $ selectFirst [UserId ==. auth] []
    case user of
        Just u -> return . toJSON . UserResp . entityVal $ u
        Nothing -> return . toJSON . Unknown "User was deleted" $ [("username", user)]

{-| Set the password of the current user -}
postApiUserSelfSetPwR :: Handler Value
postApiUserSelfSetPwR = do
    req <- requireJsonBody :: Handler UpdatePwReq
    auth <- requireAuthId
    user <- runDB $ selectFirst [UserId ==. auth] []
    case user of
        Just _ -> runDB $ update auth [UserPassword =. (Just $ newPw req)] >> return Null
        Nothing -> return . toJSON . Unknown "User was Deleted" $ [("username", user)]
