{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Server (colvidServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Database.SQLite.Simple hiding ((:.))
import Internal.Types
  (ClientAPI,  AppAPI,
    -- ClientAPI,
    Movie (..),
    MovieAPI,
    StaticAPI,
    User (..),
    UserAPI,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
  ( Application,
    BasicAuthCheck (BasicAuthCheck),
    BasicAuthData (..),
    BasicAuthResult (..),
    Context (EmptyContext, (:.)),
    Handler,
    Proxy (..),
    Server,
    err400,
    err404,
    errBody,
    serveDirectoryWebApp,
    serveWithContext,
    throwError,
    (:<|>) (..),
  )
import qualified System.Directory as Dir

-- --

handleNewUser :: String -> User -> Handler String
handleNewUser dbfile User {..} = do
  liftIO . withConnection dbfile $ \conn ->
    execute
      conn
      "INSERT INTO users VALUES (?, ?, ?, date(?))"
      (name, idn, email, registrationDay)
  return "usuario insertado exitosamente"

handleNewMovie :: String -> Movie -> Handler String
handleNewMovie dbfile Movie {..} = do
  liftIO . withConnection dbfile $ \conn ->
    execute
      conn
      "INSERT INTO movies VALUES (?, ?, ?, ?, ?, ?)"
      (title, idn, description, duration, rating, url)
  return "película insertada exitosamente"

queryById :: FromRow a => String -> Text -> Maybe Int -> Handler a
queryById _ _ Nothing = throwError err400 {errBody = "No se especificó ID"}
queryById db tbl (Just idn) = do
  result <- liftIO . withConnection db $ \conn ->
    query conn ("SELECT * FROM " <> Query tbl <> " WHERE idn = ?") (Only idn)
  if not $ null result
    then return $ head result
    else throwError err404 {errBody = "No Existe"}

allMovies :: String -> Handler [Movie]
allMovies db = liftIO . withConnection db $ \conn ->
  query conn "SELECT * FROM movies" ()

authCheck :: BasicAuthCheck User
authCheck =
  BasicAuthCheck $
    \(BasicAuthData usr pss) ->
      if usr == "admin" && pss == "123456"
        then
          return $
            Authorized
              User
                { name = "admin",
                  idn = 0,
                  email = "administrator@colvid.com.co",
                  registrationDay = fromOrdinalDate 2010 150
                }
        else return Unauthorized

database :: String
-- database = "/var/db/testdb.sqlite"  -- esta para producción, la de abajo para
database = "testdb.sqlite"       -- pruebas

app :: Application
app =
  simpleCors $
    serveWithContext appAPI authContext $
      userServer :<|> movieServer :<|> staticServer :<|> clientServer
  where
    appAPI :: Proxy AppAPI
    appAPI = Proxy

    authContext :: Context (BasicAuthCheck User ': '[])
    authContext = authCheck :. EmptyContext

    userServer :: Server UserAPI
    userServer = queryById database "users" :<|> handleNewUser database

    movieServer :: Server MovieAPI
    movieServer =
      allMovies database :<|> queryById database "movies"
        :<|> \_ -> handleNewMovie database

    staticServer :: Server StaticAPI
    staticServer = serveDirectoryWebApp "/var/assets"

    clientServer :: Server ClientAPI
    clientServer = serveDirectoryWebApp "/var/assets/homepage"


-- -- -- --

-- |
-- inicializa la base de datos
initDB :: String -> IO ()
initDB db = do
  -- crea las carpetas y archivo de base de datos, si no existen.
  Dir.createDirectoryIfMissing True "/var/assets"
  Dir.createDirectoryIfMissing True "/var/db"
  withConnection db $ \conn -> do
    execute
      conn
      ( "CREATE TABLE IF NOT EXISTS users (name TEXT,"
          <> " idn INTEGER PRIMARY KEY, email TEXT UNIQUE,"
          <> "registrationDay TEXT)"
      )
      ()
    execute
      conn
      ( "CREATE TABLE IF NOT EXISTS movies (title TEXT,"
          <> " idn INTEGER PRIMARY KEY, description TEXT, "
          <> "duration INTEGER, rating INTEGER, url TEXT)"
      )
      ()

-- |
-- la rutina de IO que se va a ejecutar como servidor.
colvidServer :: IO ()
colvidServer = do
  initDB database
  run 8081 app
