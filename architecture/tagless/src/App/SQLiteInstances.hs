{-# OPTIONS_GHC -Wno-orphans #-}

module App.SQLiteInstances where

import Database.SQLite.Simple (
  FromRow (fromRow), ToRow (toRow), field)
import Types (mkUser, User, userId,
  userName, userPhone,
  Cargo (cOwner, cGoods, Cargo), cId, CargoId (CargoId), getId, goodsList, Goods (Goods))

import Data.Text qualified as T
import Data.Maybe (fromJust)
import Data.UUID (fromText)

instance FromRow User where
  fromRow =  mkUser <$> field <*> field <*> field

instance ToRow User where
  toRow u = toRow (userId u, userName u, userPhone u)

instance FromRow Cargo where
  fromRow =  Cargo <$> _id <*> _ownerId <*> _goods
    where
      _id = fmap (CargoId . fromJust . fromText) field -- FIXME: fromJust
      _ownerId = field
      _goods = fmap (Goods . T.splitOn "||") field

instance ToRow Cargo where
  toRow c = toRow
    ( show @Text $ getId $ cId c
    , cOwner c
    , serGoods $ cGoods c
    )
    where
      serGoods = T.intercalate "||" . goodsList


