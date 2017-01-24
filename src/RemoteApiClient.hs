{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module RemoteApiClient where
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T
import qualified Data.Set                   as S
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List
import           Data.Time.Clock.POSIX
import           Data.Ord
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           GHC.Generics
import qualified Group                      as G
import qualified LocalData                  as L
import qualified Message                    as M
import ResponseWrapper
import           Network.HTTP.Conduit
import qualified User                       as U

baseUrl = "https://api.groupme.com/v3"

getGroups apiKey = do
    bs <- simpleHttp $ baseUrl ++ "/groups" ++ "?token=" ++ apiKey
    let Just (ResponseWrapper groups) = A.decode bs :: Maybe (ResponseWrapper [G.Group])
    return groups

getMessages apiKey groupId = do
    let requestUrl = concat [ baseUrl
                            , "/groups/", groupId, "/messages"
                            , "?token=", apiKey
                            , "&limit=100" ]
    bs <- simpleHttp requestUrl
    let Just msgs = A.decode bs :: Maybe MessageResponseWrapper
    return $ messages $ response msgs



downloadMore :: String -> String -> [M.Message] -> IO ([M.Message])
downloadMore apiKey groupId msgs
    | (length msgs) < 100 = do return (msgs)
    | otherwise = do
        let oldestMessageId = T.unpack $ M.id $ minimumBy (comparing M.created_at) msgs
        print $ (posixSecondsToUTCTime . fromIntegral . M.created_at) $ minimumBy (comparing M.created_at) msgs
        moreMsgs <- getAllMessagesBefore apiKey groupId oldestMessageId
        return (msgs ++ moreMsgs)

getAllMessagesBefore :: String -> String -> String -> IO ([M.Message])
getAllMessagesBefore apiKey groupId beforeId = do
    let requestUrl = concat [ baseUrl
                            , "/groups/", groupId, "/messages"
                            , "?token=", apiKey
                            , "&before_id=", beforeId
                            , "&limit=100" ]
    bs <- simpleHttp requestUrl
    let Just resp = A.decode bs :: Maybe MessageResponseWrapper
        msgs = messages $ response resp
    moreMsgs <- downloadMore apiKey groupId msgs
    return (moreMsgs)

getAllMessages apiKey groupId = do
    firstMsgs <- getMessages apiKey groupId
    downloadMore apiKey groupId firstMsgs

getMessagesUntil :: String -> T.Text -> M.Message -> IO (S.Set M.Message)
getMessagesUntil apiKey groupId msg = do
    msgs <- getMessages apiKey $ T.unpack groupId
    downloadOlderThan apiKey groupId msg $ S.fromList msgs

downloadOlderThan :: String -> T.Text -> M.Message -> S.Set M.Message -> IO (S.Set M.Message)
downloadOlderThan apiKey groupId targetMsg msgSet
    | S.member targetMsg msgSet = return (msgSet)
    | otherwise = do
    let oldestMessageId = T.unpack $ M.id $ minimumBy (comparing M.created_at) msgSet
        requestUrl = concat [ baseUrl
                            , "/groups/", T.unpack groupId, "/messages"
                            , "?token=", apiKey
                            , "&before_id=", oldestMessageId
                            , "&limit=100" ]
    bs <- simpleHttp requestUrl
    let Just resp = A.decode bs :: Maybe MessageResponseWrapper
        oldermsgSet = S.fromList $ messages $ response resp
    older <- downloadOlderThan apiKey groupId targetMsg oldermsgSet
    return (S.union older msgSet)


saveData :: String -> G.Group -> [M.Message] -> IO ()
saveData outputFile g m = do
    let localData = L.makeLocalData g m
    B.writeFile outputFile $ A.encode localData
