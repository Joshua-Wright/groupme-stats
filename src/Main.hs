{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Function
import           Data.List
import qualified Data.Map.Lazy              as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           GHC.Generics
import           GHC.IO.Encoding
import qualified Group                      as G
import qualified LocalData                  as L
import           Maths
import qualified Message                    as M
import           RemoteApiClient
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified User                       as U


outputFolder :: String
outputFolder = "data"

openDataFile filePath = do
    contents <- B.readFile filePath
    let Just localData = A.decode contents :: Maybe L.LocalData
    return localData

stats filePath = do
    localData <- openDataFile filePath
    putStrLn $ "Message count: " ++ (show . length . L.messages) localData
    createDirectoryIfMissing False outputFolder
    T.writeFile (outputFolder </> "likesReceivedByUser.dat"      ) $ likesReceivedByUser       localData
    T.writeFile (outputFolder </> "likesGivenByUser.dat"         ) $ likesGivenByUser          localData
    T.writeFile (outputFolder </> "usagePerHour.dat"             ) $ usagePerTime              localData
    T.writeFile (outputFolder </> "usagePerTimePerUser.dat"      ) $ usagePerTimePerUser       localData
    T.writeFile (outputFolder </> "allRawText.dat"               ) $ allRawText                localData
    T.writeFile (outputFolder </> "wordFrequency.dat"            ) $ wordFrequency             localData
    T.writeFile (outputFolder </> "allLikesGivenByUserToUser.dat") $ allLikesGivenByUserToUser localData
    T.writeFile (outputFolder </> "allMessageLengths.dat"        ) $ allMessageLengths         localData

action :: [String] -> IO()
action ["stats", filePath] = stats filePath
action ["userText", filePath, userName] = do
    localData <- openDataFile filePath
    let userText = rawTextByUser localData $ T.pack userName
    T.putStrLn userText
action ["downloadAll", apiKey, groupName, outputFile] = do
    groups <- getGroups apiKey
    let Just group = find (\g -> T.pack groupName == G.name g) groups
        groupId    = (T.unpack . G.group_id) group
    messages <- getAllMessages apiKey groupId
    saveData outputFile group messages
    putStrLn $ "Downloaded " ++ (show . length) messages ++ " messages"
action ["update", apiKey, dataPath] = do
    contents <- B.readFile dataPath
    let Just localData = A.decode contents :: Maybe L.LocalData
        group          = L.group localData
        oldmsgs        = L.messages localData
        groupId        = G.group_id group
        newestMessage  = maximumBy (comparing M.created_at) oldmsgs
    newMsgsSet <- getMessagesUntil apiKey groupId newestMessage
    putStrLn $ "Downloaded approx " ++ (show $ length newMsgsSet) ++ " messages"
    let allmsgs      = S.toList $ S.union newMsgsSet $ S.fromList oldmsgs
        newLocalData = localData { L.messages=allmsgs }
    putStrLn $ (show $ length allmsgs) ++ " total messages"
    B.writeFile dataPath $ A.encode newLocalData
action ["help"] = T.putStrLn $ T.unlines
    [ "Usage: groupme-stuff COMMAND [ARGS...]"
    , "Commands:"
    , "    stats FILE                             runs statistics on the file FILE"
    , "    downloadAll API_KEY GROUP_NAME FILE    dumps the group GROUP_NAME dat to FILE"
    , "    update API_KEY FILE                    updates the group data for FILE"
    ]
action _ = do
    putStrLn "Invalid command"
    action ["help"]

main = do
    -- this is needed or else writing to file sometimes fails on other systems
    setLocaleEncoding utf8
    getArgs >>= action
