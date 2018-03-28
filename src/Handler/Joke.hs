{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Joke
    ( getJokeR
    ) where

import Yesod
import Foundation
import Model
import Data.Text()
import Data.ByteString()

getJokeR :: Handler ()
getJokeR = do
    _ <- runDB . insert $ User "Fabian" "10c" "0" Nothing Nothing []
    redirect RootR
