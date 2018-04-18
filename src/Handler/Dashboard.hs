{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Dashboard
    ( getDashboardR
    , getUiUserAddR
    , getUiUserInfoR
    , getUiVoteAddR
    , getUiVoteInfoR
    , getUiAccessOutR
    , getUiAccessInR
    , getUiAccessExportR
    , getUiGradeAddR
    , getUiGradeInfoR
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


getUiAccessInR :: Handler Html
getUiAccessInR = defaultLayout $ do
    setTitle "Access in"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/accessIn.julius")
             <> toWidget $(hamletFileReload "templates/accessIn.hamlet")
        else toWidget $(juliusFile "templates/accessIn.hamlet")
             <> toWidget $(hamletFileReload "templates/accessIn.hamlet")

getUiAccessOutR :: Handler Html
getUiAccessOutR = defaultLayout $ do
    setTitle "Access out"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/accessOut.julius")
             <> toWidget $(hamletFileReload "templates/accessOut.hamlet")
        else toWidget $(juliusFile "templates/accessOut.hamlet")
             <> toWidget $(hamletFileReload "templates/accessOut.hamlet")


getUiAccessExportR :: Handler Html
getUiAccessExportR = defaultLayout $ do
    setTitle "Access export"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/accessExport.julius")
             <> toWidget $(hamletFileReload "templates/accessExport.hamlet")
        else toWidget $(juliusFile "templates/accessExport.hamlet")
             <> toWidget $(hamletFileReload "templates/accessExport.hamlet")


getUiGradeAddR :: Handler Html
getUiGradeAddR = defaultLayout $ do
    setTitle "grade add"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/gradeAdd.julius")
             <> toWidget $(hamletFileReload "templates/gradeAdd.hamlet")
        else toWidget $(juliusFile "templates/gradeAdd.hamlet")
             <> toWidget $(hamletFileReload "templates/gradeAdd.hamlet")


getUiGradeInfoR :: Handler Html
getUiGradeInfoR = defaultLayout $ do
    setTitle "grade info"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/gradeInfo.julius")
             <> toWidget $(hamletFileReload "templates/gradeInfo.hamlet")
        else toWidget $(juliusFile "templates/gradeInfo.hamlet")
             <> toWidget $(hamletFileReload "templates/gradeInfo.hamlet")

getDashboardR :: Handler Html
getDashboardR = defaultLayout $ toWidget
    [hamlet|
        <p> todo: check logged in status
        <h1> Citizen Stuff
        <ul>
            <li>
                <a href=@{UiVoteInfoR}>Vote info
                TODO: Zeigt alle Votes an und lässt user voten
        <h1> Rep Stuff
        <ul>
            <li>
                <a href=@{UiVoteAddR}>Add vote
                TODO: Implement
        <h1> Teacher stuff
        <ul>
            <li> Access times (lass mich das machen)
        <h1> Admin Stuff
        <ul>
            <li>
                <a href=@{UiUserAddR}>User add
            <li>
                <a href=@{UiUserInfoR}>User search
                TODO: Die User sollten hier verändert / gelöschct werden können.
    |]
