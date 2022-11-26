module IOApp.SQLiteInstances where

import Database.SQLite.Simple (FromRow (fromRow), ToRow (toRow), field)
import GHC.Generics (Generic)
import Types (Person (Person), User(User))

instance FromRow User where
  fromRow = User <$> (Person <$> field <*> field)

instance ToRow User where
  toRow (User (Person n p)) = toRow (n, p)