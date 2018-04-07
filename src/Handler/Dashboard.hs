{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Dashboard
    ( getDashboardR
    , getUiUserAddR
    ) where

import           Foundation
import           Settings           (appReload)
import           Text.Blaze.Html    (Html)
import           Text.Julius        (juliusFile, juliusFileReload)
import           Text.Hamlet        (hamlet, hamletFile)
import           Yesod.Core         (defaultLayout)
import           Yesod.Core.Handler (getYesod)
import           Yesod.Core.Widget  (toWidget, addScriptRemote)

getUiUserAddR :: Handler Html
getUiUserAddR = defaultLayout $ do
    addScriptRemote "https://ajax.aspnetcdn.com/ajax/jQuery/jquery-3.3.1.js"
    app <- getYesod
    toWidget $(hamletFile "templates/addUser.hamlet")
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/addUser.julius")
        else toWidget $(juliusFile "templates/addUser.julius")

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
    |]
