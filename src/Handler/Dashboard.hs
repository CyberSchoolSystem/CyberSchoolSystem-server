{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Dashboard
    ( getUiUserAddR
    , getUiUserInfoR
    , getUiVoteAddR
    , getUiVoteInfoR
    , getUiAccessOutR
    , getUiAccessInR
    , getUiAccessExportR
    , getUiGradeAddR
    , getUiGradeInfoR
    , getFaqR
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
    setTitle "Benutzer hinzufügen"
    app <- getYesod
    toWidget $(hamletFile "templates/userAdd.hamlet")
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/userAdd.julius")
        else toWidget $(juliusFile "templates/userAdd.julius")

getUiUserInfoR :: Handler Html
getUiUserInfoR = defaultLayout $ do
    setTitle "Benutzer suchen"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/userInfo.julius")
             <> toWidget $(hamletFileReload "templates/userInfo.hamlet")
        else toWidget $(juliusFile "templates/userInfo.julius")
             <> toWidget $(hamletFile "templates/userInfo.hamlet")

getUiVoteAddR :: Handler Html
getUiVoteAddR = defaultLayout $ do
    setTitle "Abstimmung erstellen"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/voteAdd.julius")
             <> toWidget $(hamletFileReload "templates/voteAdd.hamlet")
        else toWidget $(juliusFile "templates/voteAdd.julius")
             <> toWidget $(hamletFile "templates/voteAdd.hamlet")

getUiVoteInfoR :: Handler Html
getUiVoteInfoR = defaultLayout $ do
    setTitle "Wahlen"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/voteInfo.julius")
             <> toWidget $(hamletFileReload "templates/voteInfo.hamlet")
        else toWidget $(juliusFile "templates/voteInfo.julius")
             <> toWidget $(hamletFile "templates/voteInfo.hamlet")

getUiAccessInR :: Handler Html
getUiAccessInR = defaultLayout $ do
    setTitle "Access in"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/accessIn.julius")
             <> toWidget $(hamletFileReload "templates/accessIn.hamlet")
        else toWidget $(juliusFile "templates/accessIn.julius")
             <> toWidget $(hamletFile "templates/accessIn.hamlet")

getUiAccessOutR :: Handler Html
getUiAccessOutR = defaultLayout $ do
    setTitle "Access out"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/accessOut.julius")
             <> toWidget $(hamletFileReload "templates/accessOut.hamlet")
        else toWidget $(juliusFile "templates/accessOut.julius")
             <> toWidget $(hamletFile "templates/accessOut.hamlet")

getUiAccessExportR :: Handler Html
getUiAccessExportR = defaultLayout $ do
    setTitle "access export"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/accessExport.julius")
             <> toWidget $(hamletFileReload "templates/accessExport.hamlet")
        else toWidget $(juliusFile "templates/accessExport.julius")
             <> toWidget $(hamletFile "templates/accessExport.hamlet")

getUiGradeAddR :: Handler Html
getUiGradeAddR = defaultLayout $ do
    setTitle "Klasse erstellen"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/gradeAdd.julius")
             <> toWidget $(hamletFileReload "templates/gradeAdd.hamlet")
        else toWidget $(juliusFile "templates/gradeAdd.julius")
             <> toWidget $(hamletFile "templates/gradeAdd.hamlet")

getUiGradeInfoR :: Handler Html
getUiGradeInfoR = defaultLayout $ do
    setTitle "Klassenliste"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/gradeInfo.julius")
             <> toWidget $(hamletFileReload "templates/gradeInfo.hamlet")
        else toWidget $(juliusFile "templates/gradeInfo.julius")
             <> toWidget $(hamletFile "templates/gradeInfo.hamlet")


getFaqR :: Handler Html
getFaqR = defaultLayout $ do
    setTitle "FAQ"
    app <- getYesod
    if appReload . appSettings $ app
        then toWidget $(juliusFileReload "templates/faq.julius")
             <> toWidget $(hamletFileReload "templates/faq.hamlet")
        else toWidget $(juliusFile "templates/faq.julius")
             <> toWidget $(hamletFile "templates/faq.hamlet")

