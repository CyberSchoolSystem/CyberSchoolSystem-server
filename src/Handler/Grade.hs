{-# LANGUAGE OverloadedStrings #-}
module Handler.Grade
    ( postApiGradeAddR
    , getApiGradeInfoR
    , postApiGradeRemoveR
    ) where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.Text (Text)
import           Database.Persist ((==.))
import           Database.Persist.Class (insert, selectFirst, selectList, delete)
import           Database.Persist.Types (Entity(..), Filter)
import           Error
import           Foundation
import           Model
import           Yesod.Core.Json (requireJsonBody)
import           Yesod.Persist.Core

data AddGradeReq = AddGradeReq
    { addGradeName :: Text
    }

data RmGradeReq = RmGradeReq
    { rmGradeId :: Key Grade
    }

instance FromJSON AddGradeReq where
    parseJSON (Object v) = AddGradeReq <$> v .: "grade"
    parseJSON invalid = typeMismatch "AddGradeReq" invalid

instance FromJSON RmGradeReq where
    parseJSON (Object v) = RmGradeReq <$> v .: "gradeId"
    parseJSON invalid = typeMismatch "RmGradeReq" invalid

{-| Add a Grade -}
postApiGradeAddR :: Handler Value
postApiGradeAddR = do
    req <- requireJsonBody :: Handler AddGradeReq
    res <- runDB $ selectFirst [GradeName ==. addGradeName req] []
    case res of
        Nothing -> runDB $ insert Grade{gradeName = addGradeName req} >>= return . toJSON
        Just _ -> return . toJSON . NotUnique "Grade already exists" $ [("grade", addGradeName req)]

{-| Return all gradeNames and gradeIds -}
getApiGradeInfoR :: Handler Value
getApiGradeInfoR = do
    res <- runDB $ selectList ([] :: [Filter Grade]) []
    return . toJSON $ res

{-| Delete a grade -}
postApiGradeRemoveR :: Handler Value
postApiGradeRemoveR = do
    req <- requireJsonBody :: Handler RmGradeReq
    res <- runDB $ selectFirst [GradeId ==. rmGradeId req] []
    case res of
        Just g -> runDB $ delete (entityKey g) >> return Null
        Nothing -> return . toJSON . Unknown "Grade does not exist" $ [("gradeId", rmGradeId req)]
