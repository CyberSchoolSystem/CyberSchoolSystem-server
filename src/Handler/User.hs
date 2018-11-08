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

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.Types       (typeMismatch)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Database.Persist       ((==.), (=.))
import           Database.Persist.MongoDB ((=~.), MongoRegex())
import           Database.Persist.Class (insert_, selectFirst, delete, update, selectList)
import           Database.Persist.Types (Filter, Entity(..), Update)
import qualified Error                  as E
import           Foundation
import qualified Message                as M
import           Model
import           Text.HTML.SanitizeXSS  (sanitizeBalance)
import           Yesod.Auth             (requireAuthId)
import           Yesod.Auth.HashDB      (setPassword, validatePass)
import           Yesod.Auth.Util.PasswordStore (makePassword)
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
    , addPassword :: Text
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
    , oldPw :: Text
    }

instance FromJSON AddUserReq where
    parseJSON (Object v) = AddUserReq
        <$> v .: "firstName"
        <*> v .: "lastName"
        <*> v .: "gradeId"
        <*> v .: "username"
        <*> v .: "password"
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
    parseJSON (Object v) = UpdatePwReq
        <$> v .: "newPW"
        <*> v .: "oldPW"
    parseJSON invalid = typeMismatch "UpdateUserReq" invalid

instance ToJSON (AdminResp User) where
    toJSON AdminResp{admGetUsr = u} = object
        [ "firstName" .= (sanitizeBalance $ userFirstName u)
        , "lastName" .= (sanitizeBalance $ userLastName u)
        , "gradeId" .= userGrade u
        , "username" .= (sanitizeBalance $ userUsername u)
        , "roles" .= userRoles u
        ]

instance ToJSON (UserResp User) where
    toJSON UserResp{usrGetUsr = u} = object
        [ "firstName" .= (sanitizeBalance $ userFirstName u)
        , "lastName" .= (sanitizeBalance $ userLastName u)
        , "gradeId" .= userGrade u
        , "username" .= (sanitizeBalance $ userUsername u)
        , "fails" .= userFails u
        , "access" .= userAccess u
        , "roles" .= userRoles u
        ]


postApiUserAddR :: Handler Value
postApiUserAddR = do
    req <- requireJsonBody :: Handler AddUserReq
    user <- setPassword (addPassword req) $ addToUser req
    res <- runDB $ selectFirst [UserUsername ==. userUsername user] []
    case res of
        Just _ -> return . toJSON $ (E.NotUnique
                                         (M.fromMessage $ M.NotUnique M.Username)
                                         [("username", userUsername user)])
        Nothing -> runDB $ insert_ user >> (return . toJSON $ (E.ENull :: E.Error Value))

{-| Transform a request to a User -}
addToUser :: AddUserReq -> User
addToUser AddUserReq{..} = User
    { userFirstName = T.toLower addFirstName
    , userLastName = T.toLower addLastName
    , userGrade = addGrade
    , userUsername = T.toLower addUsername
    , userPassword = Just $ addPassword -- TODO: Hash
    , userRoles = addRoles
    , userFails = 0
    , userAccess = []
    }

postApiUserRemoveR :: Handler Value
postApiUserRemoveR = do
    req <- requireJsonBody :: Handler RmUserReq
    del <- runDB $ selectFirst (rmToFilter req) []
    case del of
        Just e -> runDB $ delete (entityKey e) >> (return . toJSON $ (E.ENull :: E.Error Value))
        Nothing -> return . toJSON . E.Unknown (M.fromMessage $ M.Unknown M.User) $ rmInvalidArgs req

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
        Just e -> do
            updates <- liftIO $ updateToUpdate req
            runDB $ update (entityKey e) updates
            (return . toJSON $ (E.ENull :: E.Error Value))
        Nothing -> return . toJSON . E.Unknown (M.fromMessage $ M.Unknown M.User) $ updateInvalidArgs req

{-| Create Database updates from a update request -}
updateToFilter :: UpdateUserReq -> [Filter User]
updateToFilter u =  [UserUsername ==. idUsername u]

{-| Create errors from a update request -}
updateInvalidArgs :: UpdateUserReq -> [(Text,Text)]
updateInvalidArgs u = [("username", idUsername u)]

{-| Create a database update from a update request -}
updateToUpdate :: UpdateUserReq -> IO [Update User]
updateToUpdate UpdateUserReq{..} = do
    pw <- case updatePassword of
                Just u -> Just <$> makePassword' u 14
                Nothing -> return Nothing
    return $ catMaybes [ (UserFirstName =.) <$> T.toLower <$> updateFirstName
                       , (UserLastName =.) <$> T.toLower <$> updateLastName
                       , (UserGrade =.) <$> updateGrade
                       , (UserUsername =.) <$> (T.toLower <$> updateUsername)
                       , (UserPassword =.) <$> Just <$> pw
                       , (UserRoles =.) <$> updateRole ] -- TODO: Make more flexible

{-| Make a Text password -}
makePassword' :: Text -> Int -> IO Text
makePassword' p s = do
    let pw = encodeUtf8 p
    decodeUtf8 <$> makePassword pw s

postApiUserInfoR :: Handler Value
postApiUserInfoR = do
    req <- requireJsonBody :: Handler InfoUserReq
    users <- runDB $ selectList (infoToFilter req) []
    return . toJSON $ map (AdminResp . entityVal) users

{-| Create a database filter from a info request -}
infoToFilter :: InfoUserReq -> [Filter User]
infoToFilter InfoUserReq{..} = catMaybes [ (UserFirstName =~.) . toMongoRegex <$> infoFirstName
                                         , (UserLastName =~.) . toMongoRegex <$> infoLastName
                                         , (UserGrade ==.) <$> infoGrade
                                         , (UserUsername ==.) . T.toLower <$> infoUsername
                                         , (UserRoles ==.) <$> infoRole ]

{-| Create Regex from text |-}
toMongoRegex :: Text -> MongoRegex
toMongoRegex t = (t, "ims")

{-| Get information about the current user -}
getApiUserSelfInfoR :: Handler Value
getApiUserSelfInfoR = do
    auth <- requireAuthId
    user <- runDB $ selectFirst [UserId ==. auth] []
    case user of
        Just u -> return . toJSON . UserResp . entityVal $ u
        Nothing -> return . toJSON . E.Unknown (M.fromMessage $ M.Unknown M.User) $ [("username", user)]

{-| Set the password of the current user -}
postApiUserSelfSetPwR :: Handler Value
postApiUserSelfSetPwR = do
    req <- requireJsonBody :: Handler UpdatePwReq
    password <- liftIO $ makePassword' (newPw req) 14
    auth <- requireAuthId
    user <- runDB $ selectFirst [UserId ==. auth] []
    case user of
        Just u -> updateWithPW u (oldPw req) password
        Nothing -> return . toJSON . E.Unknown (M.fromMessage $ M.Unknown M.User) $ [("username", user)]

updateWithPW :: Entity User -> Text -> Text -> Handler Value
updateWithPW user old password
    | validatePass (entityVal user) old == Just True = do
        runDB $ update (entityKey user) [UserPassword =. (Just password)]
        return . toJSON $ (E.ENull :: E.Error Value)
    | otherwise = return . toJSON . E.Wrong (M.fromMessage $ M.Wrong M.Password) $ [("oldPW", old)]
