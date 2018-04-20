{-# LANGUAGE OverloadedStrings #-}
module Error
    ( Error(..)
    ) where

import           Data.Text (Text())
import           Data.Aeson (ToJSON(..), (.=), object, Value)

type Field = Text
data Error a = MissingField Text [Field]
             | TimedOut Text [(Field, a)]
             | AlreadyDone Text [(Field, a)]
             | Unknown Text [(Field, a)]
             | NotUnique Text [(Field, a)]

instance ToJSON a => ToJSON (Error a) where
    toJSON (MissingField t f) = object ["missingField" .= errorField t f]
    toJSON (TimedOut t f) = object ["timedOut" .= errorField t f]
    toJSON (AlreadyDone t f) = object ["alreadyDone" .= errorField t f]
    toJSON (Unknown t f) = object ["unknown" .= errorField t f]
    toJSON (NotUnique t f) = object ["notUnique" .= errorField t f]


errorField :: ToJSON a => Text -> a -> Value
errorField x y = object ["msg" .= x, "field" .= y]
