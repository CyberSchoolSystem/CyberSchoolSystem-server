{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Joke
    ( getJokeR
    ) where

import           Data.Text()
import           Foundation
import           Settings
import           Text.Blaze.Html    (Html)
import           Text.Hamlet        (hamletFile, hamletFileReload)
import           Text.Julius        (juliusFile, juliusFileReload)
import           Text.Lucius        (luciusFile, luciusFileReload)
import           Yesod.Core         (defaultLayout)
import           Yesod.Core.Handler (getYesod)
import           Yesod.Core.Widget  (setTitle, toWidget)

getJokeR :: Handler Html
getJokeR = defaultLayout $ do
    setTitle "FLAPPYYYYYY"
    app <- getYesod
    if appReload . appSettings $ app
        then do
            toWidget $(juliusFileReload "templates/flappy.julius")
            toWidget $(hamletFileReload "templates/flappy.hamlet")
            toWidget $(luciusFileReload "templates/flappy.lucius")
        else do
            toWidget $(juliusFile "templates/flappy.julius")
            toWidget $(luciusFile "templates/flappy.lucius")
            toWidget $(hamletFile "templates/flappy.hamlet")
