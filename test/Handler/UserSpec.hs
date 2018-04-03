{-# LANGUAGE OverloadedStrings #-}
module Handler.UserSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Handler.User.postUserAddR" $ do
        it "can only be used by admins" $ testAuth modAdmin UserAddR
        it "makes adding users with the same Username or ChipId impossible" $ do
            authenticateAs (Entity undefined adm)
            sameCID <- liftIO $ generate (arbitrary :: Gen User)
            sameCID' <- fmap (modChipId (userChipId sameCID)) . liftIO $
                generate (arbitrary :: Gen User)

            sameUname <- liftIO $ generate (arbitrary :: Gen User)
            sameUname' <- fmap (modUsername (userUsername sameUname)) . liftIO $
                generate (arbitrary :: Gen User)

            _ <- addUser' sameCID
            _ <- addUser' sameUname

            _ <- apiAddUser sameCID'
            bodyNotContains "null"
            _ <- apiAddUser sameUname'
            bodyNotContains "null"

    describe "Handler.User.postUserInfoR" $ do
        it "can only be used by admins" $ testAuth modAdmin UserInfoR
    --     it "gives correct information" $
    --         pending
    describe "Handler.User.postUserUpdateR" $ do
        it "can only be used by admins" $ testAuth modAdmin UserUpdateR
        -- it "updates users correctly" $ do
        --     pending
    describe "Handler.User.postUserRemoveR" $ do
        it "can only be used by admins" $ testAuth modAdmin UserRemoveR
        it "removes users correctly" $ do
            authenticateAs (Entity undefined adm)
            u <- liftIO $ generate (arbitrary :: Gen User)
            u' <- liftIO $ generate (arbitrary :: Gen User)
            uk <- addUser' u
            uk' <- addUser' u'

            let j = object ["uid" .= object ["chip" .= userChipId u]]
                j' = object ["uid" .= object ["username" .= userUsername u']]

            postBody UserRemoveR $ encode j
            bodyContains "null"
            r <- runDB $ selectList [UserId ==. uk] []
            assertEq "User is still in database" r []

            postBody UserRemoveR $ encode j'
            bodyContains "null"
            r' <- runDB $ selectList [UserId ==. uk'] []
            assertEq "User is still in database" r' []

apiAddUser :: User -> YesodExample App (Maybe SResponse)
apiAddUser u = do
    let j = object [ "firstName" .= userFirstName u
                   , "lastName" .= userLastName u
                   , "grade" .= userGrade u
                   , "chip" .= userChipId u
                   , "username" .= userUsername u
                   , "password" .= userPassword u
                   , "role" .= userRoles u ]
    postBody UserAddR (encode j)
    getResponse

