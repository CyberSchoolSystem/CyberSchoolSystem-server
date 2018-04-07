{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Dashboard
    ( getDashboardR
    , getUiUserAddR
    , getUiUserInfoR
    ) where

import           Foundation
import           Data.Monoid        ((<>))
import           Settings           (appReload)
import           Text.Blaze.Html    (Html)
import           Text.Julius        (juliusFile, juliusFileReload)
import           Text.Hamlet        (hamlet, hamletFile, hamletFileReload)
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

getDashboardR :: Handler Html
getDashboardR = defaultLayout $ toWidget
    [hamlet|
        <p> todo: check logged in status
        <h1> Citizen Stuff
        <ul>
            <li> Votes
        <h1> Rep Stuff
        <ul>
            <li> Add Votes
        <h1> Teacher stuff
        <ul>
            <li> Access times
        <h1> Admin Stuff
        <ul>
            <li> <a href=@{UiUserAddR}>User add
            <li> <a href=@{UiUserInfoR}>User search
    |]
