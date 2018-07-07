{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Message
    ( Message(..)
    , Object(..)
    , fromMessage
    ) where

import           Data.Text (Text)

data Message = AlreadyDone Object
             | TimedOut Object
             | Unknown Object
             | NotUnique Object
             | Wrong Object

data Object = Vote
            | Username
            | User
            | Password
            | Border
            | Grade

fromMessage :: Message -> Text
fromMessage (AlreadyDone Vote) = "Du hast bereits an dieser Abstimmung teilgenommen!"
fromMessage (TimedOut Vote) = "Diese Abstimmung ist bereits abgelaufen. Du kannst also nichtmehr abstimmen!"
fromMessage (Unknown Vote) = "Diese Abstimmung ist nicht im System registriert. Bitte lade die Seite neu!"

fromMessage (NotUnique Username) = "Dieser Username existiert bereits. Bitte wähle einen anderen"
fromMessage (Unknown User) = "Dieser User existiert nict. Bitte lade die Seite neu!"

fromMessage (AlreadyDone Border) = "Der User ist bereits auf dieser Seite der Grenze"

fromMessage (NotUnique Grade) = "Diese Klasse existiert bereits!"
fromMessage (Unknown Grade) = "Diese Klasse existiert nicht. Bitte lade die seite neu!"

fromMessage (Wrong Password) = "Falsches Passwort. Bitte versuche es erneut!"
