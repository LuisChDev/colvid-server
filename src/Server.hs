{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (colvidServer) where

import Internal.Types
    (AppAPI, UserAPI,  Movie(..), MovieAPI, Rating(MayoresDe12), User(..) )
import Data.Time (fromGregorian)
import Servant
    (Handler, (:<|>)(..), err404, errBody, throwError, serve, Application,
     Proxy(..), Server)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Database.SQLite.Simple
import Data.Text (Text)

-- lista temporal de usuarios
users1 :: [User]
users1 =
  [ User {name = "Carlos", idn = 1, email = "carlos@logo.com",
          registrationDay = fromGregorian 2010 2 3},
    User {name = "Luis", idn = 2, email = "luis@logo.com",
          registrationDay = fromGregorian 2010 5 6}
  ]

-- lista temporal de películas
movies1 :: [Movie]
movies1 =
  [ Movie {idn = 1, title = "Godzilla",
           description = "king of the monsters",
           duration = 120, rating = MayoresDe12},
    Movie {idn = 2, title = "King Kong",
           description = "eighth wonder of the world",
           duration = 130, rating = MayoresDe12}
  ]

queryById :: (Z.HasField "idn" a Int) => [a] -> Maybe Int -> Handler a
queryById _ Nothing = throwError err404 {errBody = "No se especificó ID"}
queryById source (Just idd) = let item = filter (\x -> x.idn == idd) source
  in if not $ null item
  then return $ head item
  else throwError err404 {errBody = "No existe"}

handleNewUser :: String -> User -> Handler String
handleNewUser dbfile User{..} = do
  liftIO . withConnection dbfile $ \conn ->
    execute conn "INSERT INTO users VALUES (?, ?, ?, date(?))" (name, idn, email, registrationDay)
  return "usuario insertado exitosamente"

handleNewMovie :: String -> Movie -> Handler String
handleNewMovie dbfile Movie{..} = do
  liftIO . withConnection dbfile $ \conn ->
    execute conn "INSERT INTO movies VALUES (?, ?, ?, ?, ?)" (title, idn, description, duration, rating)
  return "película insertada exitosamente"

queryById' :: FromRow a => String -> Text -> Maybe Int -> Handler a
queryById' _ _ Nothing = throwError err404 {errBody = "No se especificó ID"}
queryById' db tbl (Just idn) = do
  result <- liftIO . withConnection db  $ \conn ->
    query conn ("SELECT * FROM " <> Query tbl <> " WHERE idn = ?") (Only idn)
  if not $ null result
    then return $ head result
    else throwError err404 {errBody = "No Existe"}


database :: String
database = "testdb.sqlite"

userServer :: Server UserAPI
userServer = queryById' database "users" :<|> handleNewUser database

movieServer :: Server MovieAPI
movieServer = queryById' database "movies" :<|> handleNewMovie database

appAPI :: Proxy AppAPI
appAPI = Proxy

app2 :: Application
app2 = serve appAPI $ userServer :<|> movieServer

-- -- -- --

-- |
-- inicializa la base de datos
initDB :: String -> IO ()
initDB db = withConnection db $ \conn -> do
  execute conn "CREATE TABLE IF NOT EXISTS users (name TEXT, idn INTEGER PRIMARY KEY, email TEXT UNIQUE, registrationDay TEXT)" ()
  execute conn "CREATE TABLE IF NOT EXISTS movies (title TEXT, idn INTEGER PRIMARY KEY, description TEXT, duration INTEGER, rating INTEGER)" ()

-- |
-- la rutina de IO que se va a ejecutar como servidor.
colvidServer :: IO ()
colvidServer = do
  initDB database
  run 8081 app2


