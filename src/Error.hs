{-# LANGUAGE OverloadedStrings #-}
module Error
    ( Error(..)
    ) where

import           Data.Text (Text())
import           Data.Aeson (ToJSON(..), (.=), object, Value(..))

type Field = Text
data Error a = MissingField Text [Field]
             | TimedOut Text [(Field, a)]
             | AlreadyDone Text [(Field, a)]
             | Unknown Text [(Field, a)]
             | NotUnique Text [(Field, a)]
             | ENull

instance ToJSON a => ToJSON (Error a) where
    toJSON (MissingField t f) = wrap $ object ["missingField" .= errorField t f]
    toJSON (TimedOut t f)     = wrap $ object ["timedOut" .= errorField t f]
    toJSON (AlreadyDone t f)  = wrap $ object ["alreadyDone" .= errorField t f]
    toJSON (Unknown t f)      = wrap $ object ["unknown" .= errorField t f]
    toJSON (NotUnique t f)    = wrap $ object ["notUnique" .= errorField t f]
    toJSON ENull              = wrap Null

errorField :: ToJSON a => Text -> a -> Value
errorField x y = object ["msg" .= x, "field" .= y]

wrap :: Value -> Value
wrap x = object ["error" .= x]
