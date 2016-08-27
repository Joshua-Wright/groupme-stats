{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Maths where
import           Prelude  hiding (show)
import           BasicPrelude (show)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function
import           Data.List
import           Data.Ratio
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           GHC.Generics
import qualified Group                      as G
import qualified LocalData                  as L
import qualified Message                    as M
import qualified User                       as U
import Debug.Trace


allRawText :: L.LocalData -> T.Text
allRawText l = T.unlines $ catMaybes $ map M.text $ L.messages l

rawTextByUser :: L.LocalData -> T.Text -> T.Text
rawTextByUser l userName = T.unlines $ catMaybes $ map M.text userMsgs
    where
        msgs = L.messages l
        Just user = find ((== userName) . U.nickname) $ G.members $ L.group l
        userId = U.user_id user
        userMsgs = filter ((== userId) . M.user_id) msgs

wordFrequency :: L.LocalData -> T.Text
wordFrequency l = T.unlines $ map lineToDat $ sortBy (comparing $ Down . snd) wordFrequency
    where
        rawText = allRawText l
        wordFrequency = groupByFrequency $ T.words rawText
        lineToDat (a,b) = T.unwords [show b, a]

allTimes :: L.LocalData -> T.Text
allTimes l = T.intercalate "\n" times
    where
        msgs = L.messages l
        times = map (show . posixSecondsToUTCTime . fromIntegral . M.created_at) msgs

-- list of user IDs to .dat of their frequency
likesGivenByUser :: L.LocalData -> T.Text
likesGivenByUser l = assocListToDat likesList
    where
        -- list of user IDs that have liked things
        idsPerLike = zip (map userName $ concatMap M.favorited_by msgs) (repeat 1)
        -- dedupe the list, and then assign indexes to them (to make gnuplot)
        likesList = Map.toList $ Map.fromListWith (+) idsPerLike
        -- 
        msgs = L.messages l
        userMap = userIdToName l
        userName "system" = "system"
        userName id = fromMaybe "unknown" $ Map.lookup id userMap

-- likes received by user
likesReceivedByUser :: L.LocalData -> T.Text
likesReceivedByUser l = assocListToDat likesList
    where
        -- tuples of (username, number of message likes)
        userNameLikeTuples = map (\m -> (userName . M.user_id $ m, length . M.favorited_by $ m)) msgs
        -- dedupe the list, and then assign indexes to them (to make gnuplot)
        likesList = {-zip [1..] $-} Map.toList $ Map.fromListWith (+) userNameLikeTuples
        -- 
        msgs = L.messages l
        userMap = userIdToName l
        userName "system" = "system"
        userName id = fromMaybe "unknown" $ Map.lookup id userMap


userIdToName :: L.LocalData -> Map.Map T.Text T.Text
userIdToName l = foldr (\u acc -> Map.insert (U.user_id u) (U.nickname u) acc) Map.empty userList
    where
        userList = G.members $ L.group l

usagePerTime :: L.LocalData -> T.Text
usagePerTime l = T.unlines $ map toDatLine $ zip [0..] $ usagePerTimeData $ L.messages l
    where
        toDatLine (a,b) = T.unwords [show a, show b ]

usagePerTimePerUser :: L.LocalData -> T.Text
usagePerTimePerUser l = T.unlines $ (header : (map toDatLine $ transpose allUserData))
    where
        userIds = map U.user_id $ G.members $ L.group l
        header :: T.Text
        header = T.unwords $ "\"\"" : (map (escape . userName) userIds)
        usagePerUser :: T.Text -> [Int]
        usagePerUser userId = usagePerTimeData $ filter (\m -> (M.user_id m) == userId) msgs
        allUserData :: [[Int]]
        allUserData = [0..23] : map usagePerUser userIds
        toDatLine :: [Int] -> T.Text
        toDatLine xs = T.unwords $ map show xs
        -- 
        msgs = L.messages l
        userMap = userIdToName l
        userName "system" = "system"
        userName id = fromMaybe "unknown" $ Map.lookup id userMap


-- gets number of messages sent per each hour of the day
usagePerTimeData :: [M.Message] -> [Int]
usagePerTimeData xs = map getHour [0..23]
    where
        -- utcTime behaves like a Num of seconds
        hourOfMessage :: M.Message -> Int
        hourOfMessage m = ((`div` 3600) . floor . utctDayTime . posixSecondsToUTCTime . fromIntegral . M.created_at) m
        -- get hours of all the messages
        msgHours = map hourOfMessage xs
        msgsByHour = Map.fromListWith (+) $ zip  msgHours (repeat 1)
        getHour hour = Map.findWithDefault 0 hour msgsByHour


likesFromUserToUser :: [M.Message] -> T.Text -> T.Text -> Int
likesFromUserToUser msgs fromId toId = length likedMsgs
    where
        userMsgs = filter (\m -> (M.user_id m) == toId) msgs
        likedMsgs = filter (\m -> fromId `elem` (M.favorited_by m)) userMsgs

allLikesGivenByUserToUser :: L.LocalData -> T.Text
allLikesGivenByUserToUser l = T.unlines $ (header : (map toDatLine $ map likesByUser userIds))
    where
        header = T.unwords $ "\"\"" : (map (escape . userName) userIds)
        likesByUser userId = (escape . userName) userId : (map show $ likesGivenByUserToUserData l userId)
        toDatLine = T.unwords
        -- 
        userIds = map U.user_id $ G.members $ L.group l
        msgs = L.messages l
        userMap = userIdToName l
        userName "system" = "system"
        userName id = fromMaybe "unknown" $ Map.lookup id userMap

likesGivenByUserToUserData :: L.LocalData -> T.Text -> [Int]
likesGivenByUserToUserData l userId = likeCombos
    where
        likeCombos = [likesFromUserToUser msgs userId toId | toId <- userIds]
        userIds = map U.user_id $ G.members $ L.group l
        msgs = L.messages l

allMessageLengths :: L.LocalData -> T.Text
allMessageLengths l = T.unlines $ map show $ lengths
    where
        -- not actually *all* message lengths, just the 90th percentile
        msgs = L.messages l
        nMsgs = floor $ (9%10) * (fromIntegral . length $ msgs)
        lengths :: [Int]
        lengths = take nMsgs $ sort $ map T.length $ catMaybes $ map M.text msgs


messageLengthsByUser :: L.LocalData -> T.Text
messageLengthsByUser l = "" {-todo-}
    where
        nBoxes = 10
        dx = nMsgs `div` nBoxes
        nMsgs = floor $ (9%10) * (fromIntegral . length $ msgs)
        lengthBounds = [(x, x+dx-1) | x <- [0,dx..nBoxes] ]
        msgCounts min max msgs = map (messagesOfLength min max) msgs
        -- messagesByUser userId = [ length (messagesOfLength a b userMsgs) | (a,b) <- lengthBounds]
        --     where userMsgs = messagesByUser userId
        -- userMsgCounts = [ (userName userId, messagesByUser userId) | userId <- userIds ]
        -- 
        msgs = L.messages l
        userIds = map U.user_id $ G.members $ L.group l
        userName "system" = "system"
        userName id = fromMaybe "unknown" $ Map.lookup id userMap
            where userMap = userIdToName l


-----------------------------------------------------
-----------------------------------------------------

messagesOfLength :: Int -> Int -> [M.Message] -> [M.Message]
messagesOfLength minLen maxLen msgs = filter pred msgs
    where pred m = all ($ m) [ ((>= minLen) . T.length . (fromMaybe "") . M.text)
                             , ((<= maxLen) . T.length . (fromMaybe "") . M.text)
                             ]
messagesByUser :: T.Text -> [M.Message] -> [M.Message]
messagesByUser userId msgs = filter pred msgs
    where pred = ((== userId) . M.user_id)
messagesOfLengthByUser :: T.Text -> Int -> Int -> [M.Message] -> Int
messagesOfLengthByUser userId minLen maxLen msgs = 
    length $ messagesOfLength minLen maxLen $ messagesByUser userId msgs


-- zip to tuples of counts, use map to group them adding the counts
groupByFrequency :: (Ord a) => [a] -> [(a, Int)]
groupByFrequency xs = Map.toList $ Map.fromListWith (+) $ zip  xs (repeat 1)

escape :: T.Text -> T.Text
escape str =T.concat ["\"", T.concatMap escapeChar str, "\""]
    where
        escapeChar :: Char -> T.Text
        escapeChar '\"' = "\\\""
        escapeChar '\n' = "\\n"
        escapeChar '\t' = "\\t"
        escapeChar x    = T.pack [x]

numPairListToDat :: (Num a, Show a) => [(a,a)] -> T.Text
numPairListToDat xs = T.intercalate "\n" $ map toLine xs
    where toLine (a,b) = T.concat [show a, " ", show b]

assocListToDat :: (Num a, Show a) => [(T.Text, a)] -> T.Text
assocListToDat xs = T.intercalate "\n" $ map toLine $ zip [1..] xs
    where
        toLine (idx, (label, content)) = T.concat [show idx, " ", escape label, " ", show content]
