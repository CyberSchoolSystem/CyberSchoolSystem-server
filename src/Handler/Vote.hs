{-# LANGUAGE OverloadedStrings #-}
module Handler.Vote
    ( postApiVoteInfoR
    , postApiVoteActR
    , postApiVoteAddR
    , postApiVoteRemoveR
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Database.Persist         ((==.), (=.))
import           Database.Persist.Types   (Filter, Entity(..), entityKey)
import           Database.Persist.Class   (selectFirst, selectList, insert, update, delete)
import           Database.Persist.MongoDB (nestEq, (->.), push)
import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Maybe               (catMaybes)
import           Data.Text                (Text)
import           Data.Time.Clock          (getCurrentTime)
import           Data.Time.LocalTime      (ZonedTime, zonedTimeToUTC)
import           Error
import           Foundation
import           Model
import           Yesod.Auth               (requireAuthId)
import           Yesod.Core.Json          (FromJSON, requireJsonBody)
import           Yesod.Persist.Core       (runDB)

data VoteReq = VoteReq
    { reqVoteId   :: Maybe (Key Vote)
    , reqChoiceId :: Maybe Int
    } deriving (Show)

data ApiVoteAct = ApiVoteAct
    { actVoteId   :: Key Vote
    , actChoiceId :: Int
    } deriving (Show)

data ApiVoteAdd = ApiVoteAdd
    { addDesc     :: Text
    , addChoices  :: [Text]
    , addEOL      :: ZonedTime
    }

data VoteDel = VoteDel { delId :: Key Vote }

instance FromJSON VoteReq where
    parseJSON (Object v) = VoteReq
        <$> v .:? "vid"
        <*> v .:? "choice"
    parseJSON invalid = typeMismatch "VoteReq" invalid

instance FromJSON ApiVoteAct where
    parseJSON (Object v) = ApiVoteAct
        <$> v .: "vid"
        <*> v .: "choice"
    parseJSON invalid = typeMismatch "ApiVoteAct" invalid

instance FromJSON ApiVoteAdd where
    parseJSON (Object v) = ApiVoteAdd
        <$> v .: "description"
        <*> v .: "choices"
        <*> v .: "endOfLife"
    parseJSON invalid = typeMismatch "ApiVoteAdd" invalid

instance FromJSON VoteDel where
    parseJSON (Object v) = VoteDel
        <$> v .: "vid"
    parseJSON invalid = typeMismatch "VoteDel" invalid

{-| Handle Post on /api/vote/info -}
postApiVoteInfoR :: Handler Value
postApiVoteInfoR = do
    inp  <- requireJsonBody :: Handler VoteReq -- TODO: Check Application Type?
    rec <- runDB $ selectList (reqToDBFilter inp) []
    return $ toJSON rec

{-| Create a list of Mongo filters based on a VoteReq -}
reqToDBFilter :: VoteReq -> [Filter Vote]
reqToDBFilter req = catMaybes go
    where go = [(VoteId ==.) <$> reqVoteId req,
                (nestEq $ VoteChoices ->. ChoiceIdentity) <$> reqChoiceId req] -- TODO: Obsolete?

{-| Let users vote -}
postApiVoteActR :: Handler Value
postApiVoteActR = do
    inp <- requireJsonBody :: Handler ApiVoteAct
    auth <- requireAuthId
    vote <- runDB $ selectFirst [VoteId ==. actVoteId inp,
                                 VoteChoices ->. ChoiceIdentity `nestEq` actChoiceId inp] []
    user <- runDB $ selectFirst [UserId ==. auth] []
    case (user, vote) of
        (Just u, Just v) -> do
            a <- liftIO $ isActive (entityVal v)
            if a
                then
                    if isAllowed u v
                        then updateVote u v inp
                        else return . toJSON $ AlreadyDone "You already voted" [("vid", actVoteId inp)]
                else return . toJSON $ TimedOut "You already voted" [("vid", actVoteId inp)]
        (_,_) -> return . toJSON . Unknown "This vote is unknown" $
            catMaybes [ const ("userId", toJSON $ auth) <$> user
                      , const ("vid", toJSON $ actVoteId inp) <$> vote]

{-| Vote. Update all neccessary records -}
updateVote :: Entity User -> Entity Vote -> ApiVoteAct -> Handler Value
updateVote user vote inp = do
    let choices = voteChoices . entityVal $ vote
    runDB $ update (entityKey vote) [push VoteVoted (entityKey user)]
    runDB $ update (entityKey vote) [VoteChoices =. updateChoices inp choices]
    return Null

{-| Insert Description -}
updateChoices :: ApiVoteAct -> [Choice] -> [Choice]
updateChoices ApiVoteAct{actChoiceId = cid} = map inc
    where inc x = if choiceIdentity x == cid
                      then x { choiceVotes = choiceVotes x + 1}
                      else x

{-| Check wether user is allowed to vote -}
isAllowed :: Entity User -> Entity Vote -> Bool
isAllowed user vote = not $ elem (entityKey user) (voteVoted . entityVal $ vote)

{-| Check whether the vote's eol is reached -}
isActive :: Vote -> IO Bool
isActive Vote{voteEndOfLife = v} = do
    t <- getCurrentTime
    if t > v
        then return False
        else return True

{-| Add a vote to the database -}
postApiVoteAddR :: Handler Value
postApiVoteAddR = do
    inp <- requireJsonBody :: Handler ApiVoteAdd
    _ <- runDB $ insert (addToVote inp)
    return Null


{-| Transform ApiVoteAdd request to database entity -}
addToVote :: ApiVoteAdd -> Vote
addToVote v = Vote {
    voteDescription = addDesc v,
    voteVoted = [],
    voteEndOfLife = zonedTimeToUTC $ addEOL v,
    voteChoices = addToChoices v }

{-| Transform ApiVoteAdd request to Choice entity -}
addToChoices :: ApiVoteAdd -> [Choice]
addToChoices ApiVoteAdd{addChoices = c} = addInfo c 0
    where addInfo (t:ts) i = Choice i 0 t : addInfo ts (i+1)
          addInfo [] _ = []

{-| Remove a vote from the database -}
postApiVoteRemoveR :: Handler Value
postApiVoteRemoveR = do
    inp <- requireJsonBody :: Handler VoteDel
    result <- runDB $ selectFirst [VoteId ==. delId inp] []
    case result of
        Just ent -> runDB $ delete (entityKey ent) >>= (\_ -> return Null)
        Nothing  -> return . toJSON $ Unknown "This vote is unknown" [("vid", delId inp)]
