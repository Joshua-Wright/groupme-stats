{-# LANGUAGE DeriveGeneric #-}
module LocalData(LocalData(..)) where
import           Data.Aeson
import           Data.Map.Lazy
import qualified Data.Set      as S
import           Data.Text
import           GHC.Generics
import qualified Group         as G
import qualified Message       as Msg
import           Prelude       hiding (id)

data LocalData = LocalData {
      group    :: G.Group
    -- , messages :: S.Set Msg.Message
    , messages :: [Msg.Message]
    } deriving (Generic, Show)
instance FromJSON LocalData
instance ToJSON   LocalData where
    toEncoding = genericToEncoding defaultOptions
