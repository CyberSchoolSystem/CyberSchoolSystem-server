{-# LANGUAGE OverloadedStrings #-}
module Error
    ( Error(..)
    ) where

import           Data.Text (Text())
import           Data.Aeson (ToJSON(..), (.=), object)

type Field = Text
data Error a = MissingField [Field]
             | WrongFieldValue [(Field,a)]
             | Impossible [(Field,a)]
             | PermissionDenied a

instance ToJSON a => ToJSON (Error a) where
    toJSON (MissingField f) = object ["missingField" .= f]
    toJSON (WrongFieldValue f) = object ["wrongFieldValue" .= f]
    toJSON (Impossible f) = object ["impossible" .= f]
    toJSON (PermissionDenied f) = object ["permissionDenied" .= f]
