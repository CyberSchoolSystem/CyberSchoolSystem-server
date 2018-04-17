{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Dashboard
    ( getDashboardR
    , getUiUserAddR
    , getUiUserInfoR
    , getUiVoteAddR
    , getUiVoteInfoR
    ) where

import           Foundation
import           Data.Monoid        ((<>))
import           Settings           (appReload)
import           Text.Blaze.Html    (Html)
import           Text.Julius        (juliusFile, juliusFileReload)
import           Text.Hamlet        (hamletFile, hamletFileReload)
import           Yesod.Core         (defaultLayout)
import           Yesod.Core.Handler (getYesod)
import           Yesod.Core.Widget  (toWidget, setTitle)

getUiUserAddR :: Handler Html
getUiUserAddR = defaultLayout $ do
    setTitle "User add"
    app <- getYesod
    toWidget $(hamletFile "templates/userAdd.hamlet")
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/userAdd.julius")
        else toWidget $(juliusFile "templates/userAdd.julius")

getUiUserInfoR :: Handler Html
getUiUserInfoR = defaultLayout $ do
    setTitle "User search"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/userInfo.julius")
             <> toWidget $(hamletFileReload "templates/userInfo.hamlet")
        else toWidget $(juliusFile "templates/userInfo.hamlet")
             <> toWidget $(hamletFileReload "templates/userInfo.hamlet")

getUiVoteAddR :: Handler Html
getUiVoteAddR = defaultLayout $ do
    setTitle "Vote add"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/voteAdd.julius")
             <> toWidget $(hamletFileReload "templates/voteAdd.hamlet")
        else toWidget $(juliusFile "templates/voteAdd.hamlet")
             <> toWidget $(hamletFileReload "templates/voteAdd.hamlet")

getUiVoteInfoR :: Handler Html
getUiVoteInfoR = defaultLayout $ do
    setTitle "Vote info"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/voteInfo.julius")
             <> toWidget $(hamletFileReload "templates/voteInfo.hamlet")
        else toWidget $(juliusFile "templates/voteInfo.hamlet")
             <> toWidget $(hamletFileReload "templates/voteInfo.hamlet")

getDashboardR :: Handler Html
getDashboardR = defaultLayout $ do
    setTitle "Dashboard"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/dashboard.julius")
             <> toWidget $(hamletFileReload "templates/dashboard.hamlet")
        else toWidget $(juliusFile "templates/dashboard.hamlet")
             <> toWidget $(hamletFileReload "templates/dashboard.hamlet")

