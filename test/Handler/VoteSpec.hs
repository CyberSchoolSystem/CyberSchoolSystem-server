{-# LANGUAGE OverloadedStrings #-}
module Handler.VoteSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Handler.Vote.postApiVoteAddR" $ do
        it "can only be used by representatives" $ testAuth modRep ApiVoteAddR
        it "adds correct votes" $ do
            authenticateAs (Entity undefined adm)
            vdesc <- liftIO $ generate (arbitrary :: Gen Text)
            cdesc <- liftIO $ generate (arbitrary :: Gen [Text])
            let j = object ["description" .= vdesc, "choices" .= cdesc]
            postBody ApiVoteAddR $ encode j
            statusIs 200
            r <- runDB $ selectList [VoteDescription ==. vdesc] []
            assertNotEq "Vote not found in db" r []
    describe "Handler.Vote.postApiVoteActR" $ do
        it "can onley be used by citizens" $ testAuth modCit ApiVoteActR
        -- it "lets citizens vote" $ do
        -- it "does not let citizens vote twice" $ do
    -- describe "Handler.Vote.postApiVoteInfoR" $ do
        -- it "gives correct information" $ do
    describe "Handler.Vote.postApiVoteRemoveR" $ do
        it "can only be used by representatives" $ testAuth modRep ApiVoteRemoveR
        -- it "deletes votes" $ do
