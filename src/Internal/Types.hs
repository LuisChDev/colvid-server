{-# LANGUAGE DeriveGeneric #-}

module Internal.Types where

-- Aquí se definen los tipos empleados por la aplicación

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant (Get, JSON, Post, QueryParam, ReqBody, (:<|>), type (:>))
import Database.SQLite.Simple (ResultError(ConversionFailed), SQLData(SQLInteger), field, FromRow(fromRow))
import Database.SQLite.Simple.ToField (ToField, toField)
import Database.SQLite.Simple.FromField (returnError, FromField, fromField)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))

-- |
-- El tipo de los usuarios
data User = User
  { name :: String,
    idn :: Int,
    email :: String,
    registrationDay :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

-- |
-- El tipo de las películas
data Movie = Movie
  { title :: String,
    idn :: Int,
    description :: String,
    duration :: Int,
    rating :: Rating
  } deriving (Eq, Show, Generic)
instance ToJSON Movie
instance FromJSON Movie

instance FromRow Movie where
  fromRow = Movie <$> field <*> field <*> field <*> field <*> field

data Rating = Todos | MayoresDe7 | MayoresDe12 | MayoresDe15 | MayoresDe18
  deriving (Eq, Show, Generic)
instance ToJSON Rating
instance FromJSON Rating
instance ToField Rating where
  toField Todos = SQLInteger 0
  toField MayoresDe7 = SQLInteger 1
  toField MayoresDe12 = SQLInteger 2
  toField MayoresDe15 = SQLInteger 3
  toField MayoresDe18 = SQLInteger 4

instance FromField Rating where
  fromField f@(Field (SQLInteger r) _)
    | r == 0 = Ok Todos
    | r == 1 = Ok MayoresDe7
    | r == 2 = Ok MayoresDe12
    | r == 3 = Ok MayoresDe15
    | r == 4 = Ok MayoresDe18
    | otherwise = returnError ConversionFailed f $ "Expecting 0 =< r < 5, got" ++ show r
  fromField f = returnError ConversionFailed f "expecting an SQLInteger column type"

-- -- -- --

-- El tipo de las partes de la API de nuestra aplicación.

type UserAPI =
  "users"
    :> ( QueryParam "id" Int :> Get '[JSON] User
           :<|> ReqBody '[JSON] User :> Post '[JSON] String
       )

-- data SortBy = Age | Name

type MovieAPI = "movies" :> (QueryParam "id" Int :> Get '[JSON] Movie
                             :<|> ReqBody '[JSON] Movie :> Post '[JSON] String)

type AppAPI = UserAPI :<|> MovieAPI

{--}
