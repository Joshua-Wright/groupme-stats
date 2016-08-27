{-# LANGUAGE DeriveGeneric #-}
module Group
(Group(..)
, MessagePreview(..)
, MessagePreviewWrapper(..)
) where
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import qualified User as U
import qualified Message as Msg
import           Prelude      hiding (id)

data Group = Group {
      id         :: Text
    , name       :: Text
    , group_id   :: Text
    , share_url  :: Maybe Text
    , created_at :: Int
    , updated_at :: Int
    , members    :: [U.User]
    , messages   :: Maybe MessagePreviewWrapper
    } deriving (Generic, Show)
instance FromJSON Group
instance ToJSON   Group where
    toEncoding = genericToEncoding defaultOptions

data MessagePreviewWrapper = MessagePreviewWrapper {
      count           :: Int
    , last_message_id :: Text
    , preview         :: MessagePreview
    } deriving (Generic, Show)
instance FromJSON MessagePreviewWrapper
instance ToJSON   MessagePreviewWrapper where
    toEncoding = genericToEncoding defaultOptions

data MessagePreview = MessagePreview {
      nickname  :: Maybe Text
    , text      :: Maybe Text
    , image_url :: Maybe Text
    , system    :: Maybe Bool
    } deriving (Generic, Show)
instance FromJSON MessagePreview
instance ToJSON   MessagePreview where
    toEncoding = genericToEncoding defaultOptions
