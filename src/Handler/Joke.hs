{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Joke
    ( getJokeR
    ) where

import           Yesod
import           Foundation
import           Model
import           Data.Text()

getJokeR :: Handler ()
getJokeR = do
    _ <- runDB . insert $ User "Fabian" "Geiselhart" "10c" "0" "JESUS" (Just "sha256|17|pZUkwz0cnbtcVxSJBD3qeQ==|D3KVx2A/tAhcy44z2QjcG/FvoV2jEyvKm1nH+dgv0bw=") 0 [] Role{
                            roleCitizen = True, roleRepresentative = True, roleTeacher = Nothing, roleAdmin = False}
    redirect RootR
