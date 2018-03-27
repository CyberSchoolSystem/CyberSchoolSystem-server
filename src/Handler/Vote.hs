{-# LANGUAGE OverloadedStrings #-}
module Handler.Vote
    ( getVoteInfoR
    , postVoteActR
    , postVoteAddR
    , postVoteRemoveR
    ) where

-- TODO: Cleanup Import list
import           Database.Persist         ((==.), (=.), (+=.))
import           Database.Persist.Types   (Filter, Entity(..), entityKey, updateValue)
import           Database.Persist.Class   (selectFirst, selectList, insert, update, delete)
import           Database.Persist.MongoDB (nestEq, (->.), push, nestInc, eachOp)
import           Data.Aeson
import           Data.Maybe               (catMaybes)
import qualified Data.Text as T
import           Data.Text                (Text)
import           Foundation
import           Model
import           Text.Blaze.Html          (Html)
import           TextShow                 (showt)
import           Yesod.Persist.Core       (runDB)
import           Yesod.Core.Handler       (invalidArgs)
import           Yesod.Core.Json          (FromJSON, parseJsonBody)
import           Data.Aeson.Types         (Result(..), Object(..), typeMismatch)

data VoteReq = VoteReq
    { reqVoteId   :: Maybe Int
    , reqChoiceId :: Maybe Int
    } deriving (Show)

data VoteAct = VoteAct
    { chip        :: Text
    , actVoteId   :: Int
    , actChoiceId :: Int
    } deriving (Show)

data VoteAdd = VoteAdd
    { addDesc     :: Text
    , addChoices  :: [Text]
    }

data VoteDel = VoteDel { delId :: Int }

instance FromJSON VoteReq where
    parseJSON (Object v) = VoteReq
        <$> v .:? "id"
        <*> v .:? "choice"
    parseJSON invalid = typeMismatch "VoteReq" invalid

instance FromJSON VoteAct where
    parseJSON (Object v) = VoteAct
        <$> v .: "chip"
        <*> v .: "id"
        <*> v .: "choice"
    parseJSON invalid = typeMismatch "VoteAct" invalid

instance FromJSON VoteAdd where
    parseJSON (Object v) = VoteAdd
        <$> v .: "description"
        <*> v .: "choices"
    parseJSON invalid = typeMismatch "VoteAdd" invalid

instance FromJSON VoteDel where
    parseJSON (Object v) = VoteDel
        <$> v .: "id"
    parseJSON invalid = typeMismatch "VoteDel" invalid

{-| Handle GET on /api/vote/info -}
-- TODO: Show as JSON
getVoteInfoR :: Handler Text
getVoteInfoR = do
    (Success inp) <- parseJsonBody :: Handler (Result VoteReq) -- TODO: Check Application Type?
    rec <- runDB $ selectList (reqToDBFilter inp) []
    return $ T.pack $ show rec

{-| Create a list of Mongo filters based on a VoteReq -}
reqToDBFilter :: VoteReq -> [Filter Vote]
reqToDBFilter req = catMaybes go
    where go = [(VoteIdentity ==.) <$> reqVoteId req,
                (nestEq $ VoteChoices ->. ChoiceIdentity) <$> reqChoiceId req] -- TODO: Obsolete?

{-| Handle POST on /api/vote/vote -}
postVoteActR :: Handler ()
postVoteActR = do
    (Success inp) <- parseJsonBody :: Handler (Result VoteAct) -- TODO: Check for malformed JSON
    user <- runDB $ selectFirst [UserChipId ==. chip inp] []
    vote <- runDB $ selectFirst [VoteIdentity ==. actVoteId inp,
                                 VoteChoices ->. ChoiceIdentity `nestEq` actChoiceId inp] []
    case (user, vote) of
        (Just u, Just v) -> if isAllowed u v
                                then updateVote u v inp
                                else invalidArgs ["You already voted"] -- TODO: permissionDenied
        _                -> invalidArgs ["User or Vote not found"]

{-| Vote. Update all neccessary records -}
updateVote :: Entity User -> Entity Vote -> VoteAct -> Handler ()
updateVote user vote inp = do
    let choices = voteChoices . entityVal $ vote
    runDB $ update (entityKey vote) [push VoteVoted (entityKey user)]
    runDB $ update (entityKey vote) [VoteChoices =. updateChoices inp choices]

{-| Insert Description -}
updateChoices :: VoteAct -> [Choice] -> [Choice]
updateChoices VoteAct{actChoiceId = cid} = map inc
    where inc x = if choiceIdentity x == cid
                      then x { choiceVotes = choiceVotes x + 1}
                      else x

{-| Check wether user is allowed to vote -}
isAllowed :: Entity User -> Entity Vote -> Bool
isAllowed user vote = not $ elem (entityKey user) (voteVoted . entityVal $ vote)

{-| Add a vote to the database -}
postVoteAddR :: Handler ()
postVoteAddR = do
    (Success inp) <- parseJsonBody :: Handler (Result VoteAdd)
    minId <- minVoteId
    _ <- runDB $ insert (addToVote inp minId)
    return ()

{-| Find the smallest available voteId -}
minVoteId :: Handler Int
minVoteId = do
    votes <- runDB $ selectList [] []
    return $ minimumFree $ map (voteIdentity . entityVal) votes
        where minimumFree x = head $ filter (not . flip elem x) [1..]

{-| Transform VoteAdd request to database entity -}
addToVote :: VoteAdd -> Int -> Vote
addToVote v i = Vote {
    voteIdentity = i,
    voteDescription = addDesc v,
    voteVoted = [],
    voteChoices = addToChoices v }

{-| Transform VoteAdd request to Choice entity -}
addToChoices :: VoteAdd -> [Choice]
addToChoices VoteAdd{addChoices = c} = addInfo c 0
    where addInfo (t:ts) i = Choice i 0 t : addInfo ts (i+1)
          addInfo [] _ = []

{-| Remove a vote from the database -}
postVoteRemoveR :: Handler ()
postVoteRemoveR = do
    (Success inp) <- parseJsonBody :: Handler (Result VoteDel)
    result <- runDB $ selectFirst [VoteIdentity ==. delId inp] []
    case result of
        Just ent -> runDB $ delete (entityKey ent)
        Nothing  -> invalidArgs [ T.concat ["Id ", showt $ delId inp, " does not exist"]]

