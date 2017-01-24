{-# LANGUAGE DeriveGeneric #-}
module User(User(..),UserId) where
import           Data.Text
import           Data.Aeson
import           GHC.Generics
import           Prelude      hiding (id)


type UserId = Text
data User = User {
      user_id   :: UserId
    , id        :: Text -- no clue what this one is
    , nickname  :: Text
    , image_url :: Maybe Text
    , muted     :: Bool
    } deriving (Generic, Show)
instance FromJSON User
instance ToJSON   User where
    toEncoding = genericToEncoding defaultOptions
