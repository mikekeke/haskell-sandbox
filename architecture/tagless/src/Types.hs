{-# LANGUAGE PatternSynonyms #-}
module Types (
  Cargo(..),
  CargoId(..),
  Goods(..),
  Person(..),
  User, 
  UserPhone,
  UserId,
  userId,
  mkUser,
  userName,
  userPhone,

 userFromPerson)where

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (for)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Text.Show.Pretty (ppShow)

data Cargo = Cargo
  { cId :: CargoId,
    cOwner :: UserId,
    cGoods :: Goods
  }
  deriving stock (Show)

newtype CargoId = CargoId {getId :: UUID}
  deriving stock (Eq, Ord, Show, Generic)

newtype Goods = Goods
  { goodsList :: [Text]
  }
  deriving stock (Show)

data Person = Person
  { pName :: Text,
    pPhone :: UserPhone -- used for indetification
  }
  deriving stock (Eq, Show, Generic)

type UserPhone = Text

type UserId = UserPhone
data User = MkUser UserId Person
  deriving stock (Eq, Show, Generic)

userId :: User -> UserId
userId (MkUser uid _) = uid 

getPerson :: User -> Person
getPerson (MkUser _ p) = p 

mkUser :: UserId -> Text -> Text -> User
mkUser uid name phone = MkUser uid (Person name phone)

userFromPerson :: Person -> User
userFromPerson p = MkUser (pPhone p) p

userName :: User -> Text
userName = pName . getPerson

userPhone :: User -> UserPhone
userPhone = pPhone . getPerson


