{-# LANGUAGE OverloadedStrings #-}
module Handler.AccessSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Handler.Access.postAccessInR" $ do
        it "lets users in" $ prepareUsers

        it "does not let them in a second time" $ do
            prepareUsers
            access AccessInR "User was let in a second time" "F"
                  (encode $ object [ "impossible" .= [toJSON (("chip", "F") :: (String, String))
                                                     ,toJSON (("inside", True) :: (String, Bool))] ])

    describe "Handler.Access.postAccessOutR" $ do
        it "lets users out" $ do
            prepareUsers
            letout

        it "does not let them out a second time" $ do
            prepareUsers
            letout
            access AccessOutR "User was let out a second time" "F"
                  (encode $ object [ "impossible" .= [toJSON (("chip", "F") :: (String, String))
                                                     ,toJSON (("inside", False) :: (String, Bool))] ])

access :: Yesod site => Route site -> String -> String -> ByteString -> YesodExample site ()
access route msg chip cmp = do
    postBody route $ encode $ object [ "chip" .= chip ]
    (Just r) <- getResponse
    assertEq (msg <> (toString $ simpleBody r)) (simpleBody r) cmp

letout :: YesodExample App ()
letout = access AccessOutR "User was not let out" "F" "null"

prepareUsers :: YesodExample App ()
prepareUsers = do
    _ <- addUser "" "" "" "F" "" Nothing nobody
    access AccessInR "User was not let in" "F" "null"
