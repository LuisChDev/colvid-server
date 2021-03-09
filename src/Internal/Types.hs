{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Internal.Types
-- Description : Aquí se definen los tipos empleados por la aplicación
-- Copyright   : (c) Luis Chavarriaga, 2021
-- License     : BSD-3-Clause
-- Maintainer  : luischa123@gmail.com
-- Stability   : experimental
module Internal.Types where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.ByteString.Builder (byteString)
import Data.Either.Extra (mapRight)
import Data.Text (unpack)
import Data.Time (Day)
import Database.PostgreSQL.Simple.FromField
  ( Field (typeOid),
    FromField,
    ResultError (..),
    fromField,
    returnError,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToField (Action (Plain), ToField, toField)
import Database.PostgreSQL.Simple.TypeInfo.Static (int2Oid)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Servant
  ( BasicAuth,
    Get,
    JSON,
    Post,
    QueryParam,
    Raw,
    ReqBody,
    (:<|>),
    (:>),
  )
import Servant.Multipart
  ( FromMultipart (..),
    MultipartForm,
    Tmp,
    fdPayload,
    files,
    inputs,
    lookupFile,
    lookupInput,
  )

-- -- -- --

-- * Tipos básicos de la aplicación

-- Estos son los tipos de datos que se intercambiarán entre el servidor y la
-- aplicación.

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
    rating :: Rating,
    url :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Movie

instance FromJSON Movie

instance FromRow Movie where
  fromRow = Movie <$> field <*> field <*> field <*> field <*> field <*> field

data Rating = Todos | MayoresDe7 | MayoresDe12 | MayoresDe15 | MayoresDe18
  deriving (Eq, Show, Read, Generic)

instance ToJSON Rating

instance FromJSON Rating

instance ToField Rating where
  toField Todos = Plain $ byteString "0"
  toField MayoresDe7 = Plain $ byteString "1"
  toField MayoresDe12 = Plain $ byteString "2"
  toField MayoresDe15 = Plain $ byteString "3"
  toField MayoresDe18 = Plain $ byteString "4"

instance FromField Rating where
  fromField f bs
    | typeOid f /= int2Oid = returnError Incompatible f ""
    | bs == Nothing = returnError UnexpectedNull f ""
    | bs == Just "0" = pure Todos
    | bs == Just "1" = pure MayoresDe7
    | bs == Just "2" = pure MayoresDe12
    | bs == Just "3" = pure MayoresDe15
    | bs == Just "4" = pure MayoresDe18
    | otherwise = returnError ConversionFailed f ""

-- |
-- instancia que permite extraer un objeto de tipo película a partir del
-- cuerpo de una solicitud multipart.
-- el objeto resultante es TEMPORAL, es necesario guardar apropiadamente
-- la imagen para obtener la URL permanente y ahí sí retornar
instance FromMultipart Tmp Movie where
  fromMultipart form =
    Movie <$> extract' "title"
      <*> extract "idn"
      <*> extract' "description"
      <*> extract "duration"
      <*> extract "rating"
      <*> extractFile "poster"
    where
      extract field_ = mapRight (read . unpack) $ lookupInput field_ form
      extract' field_ = mapRight unpack $ lookupInput field_ form
      extractFile field_ = mapRight fdPayload $ lookupFile field_ form

-- dupl :: (a -> a -> b) -> a -> b
-- dupl f val = f val val

-- -- -- --

-- * componentes API

-- El tipo de las partes de la API de nuestra aplicación.

type UserAPI =
  "users"
    :> ( QueryParam "id" Int :> Get '[JSON] User
           :<|> ReqBody '[JSON] User
             :> Post '[JSON] String
       )

type MovieAPI =
  "movies"
    :> ( ("all" :> Get '[JSON] [Movie])
           :<|> QueryParam "id" Int
             :> Get '[JSON] Movie
           -- :<|> BasicAuth "admin-realm" User
           --   :> ReqBody '[JSON] Movie
           --   :> Post '[JSON] String   -- subir película
           :<|> BasicAuth "admin-realm" User
             :> MultipartForm Tmp Movie
             :> Post '[JSON] String -- subir película
       )

type StaticAPI = "assets" :> Raw

type AuthAPI = "auth" :> QueryParam "usr" String :> QueryParam "pwd" String :> Get '[JSON] Bool

type AppAPI = UserAPI :<|> MovieAPI :<|> StaticAPI :<|> AuthAPI

{--}
