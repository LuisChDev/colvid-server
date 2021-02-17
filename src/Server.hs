{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (colvidServer) where

import Internal.Types
    (StaticAPI, AppAPI, UserAPI,  Movie(..), MovieAPI, User(..) )
import Servant
    (serveDirectoryWebApp, err400, Handler, (:<|>)(..), err404, errBody, throwError, serve, Application,
     Proxy(..), Server)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.SQLite.Simple
import Data.Text (Text)

-- --

handleNewUser :: String -> User -> Handler String
handleNewUser dbfile User{..} = do
  liftIO . withConnection dbfile $ \conn ->
    execute conn "INSERT INTO users VALUES (?, ?, ?, date(?))"
    (name, idn, email, registrationDay)
  return "usuario insertado exitosamente"

handleNewMovie :: String -> Movie -> Handler String
handleNewMovie dbfile Movie{..} = do
  liftIO . withConnection dbfile $ \conn ->
    execute conn "INSERT INTO movies VALUES (?, ?, ?, ?, ?)"
    (title, idn, description, duration, rating)
  return "película insertada exitosamente"

queryById :: FromRow a => String -> Text -> Maybe Int -> Handler a
queryById _ _ Nothing = throwError err400 {errBody = "No se especificó ID"}
queryById db tbl (Just idn) = do
  result <- liftIO . withConnection db  $ \conn ->
    query conn ("SELECT * FROM " <> Query tbl <> " WHERE idn = ?") (Only idn)
  if not $ null result
    then return $ head result
    else throwError err404 {errBody = "No Existe"}


database :: String
database = "testdb.sqlite"


app :: Application
app = serve appAPI $ userServer :<|> movieServer :<|> staticServer
  where
    appAPI :: Proxy AppAPI
    appAPI = Proxy

    userServer :: Server UserAPI
    userServer = queryById database "users" :<|> handleNewUser database

    movieServer :: Server MovieAPI
    movieServer = queryById database "movies" :<|> handleNewMovie database

    staticServer :: Server StaticAPI
    staticServer = serveDirectoryWebApp "assets"

-- -- -- --

-- |
-- inicializa la base de datos
initDB :: String -> IO ()
initDB db = withConnection db $ \conn -> do
  execute
    conn ("CREATE TABLE IF NOT EXISTS users (name TEXT," <>
           " idn INTEGER PRIMARY KEY, email TEXT UNIQUE," <>
           "registrationDay TEXT)") ()
  execute
    conn ("CREATE TABLE IF NOT EXISTS movies (title TEXT," <>
           " idn INTEGER PRIMARY KEY, description TEXT, " <>
           "duration INTEGER, rating INTEGER)") ()

-- |
-- la rutina de IO que se va a ejecutar como servidor.
colvidServer :: IO ()
colvidServer = do
  initDB database
  run 8081 app


