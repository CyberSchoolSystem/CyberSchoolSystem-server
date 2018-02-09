{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Handler.Joke
    ( getJokeR
    ) where

import Yesod
import Foundation

getJokeR :: Handler Html
getJokeR = defaultLayout [whamlet| lol |]
