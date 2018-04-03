{-# LANGUAGE OverloadedStrings #-}
module Handler.VoteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Handler.Vote.postVoteAddR" $ do
        it "can only be used by representatives" $ testAuth modRep VoteAddR
        it "adds correct votes" $ do
            authenticateAs (Entity undefined adm)
            vdesc <- liftIO $ generate (arbitrary :: Gen Text)
            cdesc <- liftIO $ generate (arbitrary :: Gen [Text])
            let j = object ["description" .= vdesc, "choices" .= cdesc]
            postBody VoteAddR $ encode j
            statusIs 200
            r <- runDB $ selectList [VoteDescription ==. vdesc] []
            assertNotEq "Vote not found in db" r []
    describe "Handler.Vote.postVoteActR" $ do
        it "can onley be used by citizens" $ testAuth modCit VoteActR
        -- it "lets citizens vote" $ do
        -- it "does not let citizens vote twice" $ do
    -- describe "Handler.Vote.postVoteInfoR" $ do
        -- it "gives correct information" $ do
    describe "Handler.Vote.postVoteRemoveR" $ do
        it "can only be used by representatives" $ testAuth modRep VoteRemoveR
        -- it "deletes votes" $ do
