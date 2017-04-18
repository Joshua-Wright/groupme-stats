{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module ResponseWrapper(ResponseWrapper(..), MessageResponseWrapper, MessageList(..)) where
import           Data.Aeson
import           GHC.Generics
import qualified Group        as G
import qualified Message      as M
import           Prelude      hiding (id)
import qualified User         as U


data ResponseWrapper a = ResponseWrapper {
    response :: a
    } deriving (Generic, Show)
instance FromJSON (ResponseWrapper M.Message)
instance FromJSON (ResponseWrapper [M.Message])
instance FromJSON (ResponseWrapper U.User)
instance FromJSON (ResponseWrapper G.Group)
instance FromJSON (ResponseWrapper [G.Group])

data MessageList = MessageList {
    count    :: Int,
    messages :: [M.Message]
    } deriving (Generic, Show)
instance FromJSON MessageList
instance FromJSON (ResponseWrapper MessageList)
type MessageResponseWrapper = ResponseWrapper MessageList
