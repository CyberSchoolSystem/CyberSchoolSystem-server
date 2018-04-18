{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Joke
    ( getJokeR
    ) where

import           Data.Text()
import           Foundation
import           Model
import           Yesod

getJokeR :: Handler ()
getJokeR = do
    g <- runDB . insert $ Grade "master"
    _ <- runDB . insert $ User "Fabian" "Geiselhart" g "JESUS"
              (Just "sha256|17|pZUkwz0cnbtcVxSJBD3qeQ==|D3KVx2A/tAhcy44z2QjcG/FvoV2jEyvKm1nH+dgv0bw=")
              0 [] Role{roleCitizen = True, roleRepresentative = True,
                        roleTeacher = Nothing, roleAdmin = False,
                        roleCustoms = True, roleTech = True}
    redirect RootR
