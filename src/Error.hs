{-# LANGUAGE OverloadedStrings #-}
module Error
    ( Error(..)
    ) where

import           Data.Text (Text())
import           Data.Aeson (ToJSON(..), (.=), object, Value(..))

type Field = Text
type Msg = Text

data Error a = MissingField Msg [Field]
             | TimedOut Msg [(Field, a)]
             | AlreadyDone Msg [(Field, a)]
             | Unknown Msg [(Field, a)]
             | NotUnique Msg [(Field, a)]
             | NotFound Msg
             | InternalError Msg Text -- 500
             | InvalidArgs Msg [Text] -- 400
             | NotAuthenticated Msg -- 401
             | PermissionDenied Msg Text -- 403
             | BadMethod Msg Text -- 405
             | ENull

instance ToJSON a => ToJSON (Error a) where
    toJSON (MissingField t f)     = wrap $ object ["missingField" .= errorField t f]
    toJSON (TimedOut t f)         = wrap $ object ["timedOut" .= errorField t f]
    toJSON (AlreadyDone t f)      = wrap $ object ["alreadyDone" .= errorField t f]
    toJSON (Unknown t f)          = wrap $ object ["unknown" .= errorField t f]
    toJSON (NotUnique t f)        = wrap $ object ["notUnique" .= errorField t f]
    toJSON (NotFound t)           = wrap $ object ["notFound" .= object ["msg" .= t]]
    toJSON (InternalError t f)    = wrap $ object ["internalError" .= object ["msg" .= t, "info" .= f]]
    toJSON (InvalidArgs t f)      = wrap $ object ["invalidArgs" .= object ["msg" .= t, "info" .= f]]
    toJSON (NotAuthenticated t)   = wrap $ object ["notAuthenticated" .= object ["msg" .= t]]
    toJSON (PermissionDenied t f) = wrap $ object ["permissionDenied" .= object ["msg" .= t, "info" .= f]]
    toJSON (BadMethod t f)        = wrap $ object ["badMethod" .= object ["msg" .= t, "info" .= f]]
    toJSON ENull                  = wrap Null

errorField :: ToJSON a => Text -> a -> Value
errorField x y = object ["msg" .= x, "field" .= y]

wrap :: Value -> Value
wrap x = object ["error" .= x]
