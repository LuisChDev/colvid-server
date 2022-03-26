{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( colvidServer
  )
where

import           Prelude                 hiding ( readFile )

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.ByteString                ( readFile
                                                , ByteString
                                                )
import           Data.String                    ( IsString(fromString) )
import           Data.Time.Calendar.OrdinalDate ( fromOrdinalDate )
import           Database.PostgreSQL.Simple
                                         hiding ( (:.) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query(Query) )
import           Internal.Types                 ( AppAPI
                                                , AuthAPI
                                                , Movie(..)
                                                , MovieAPI
                                                , StaticAPI
                                                , User(..)
                                                , UserAPI
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Servant                        ( Application
                                                , BasicAuthCheck(BasicAuthCheck)
                                                , BasicAuthData(..)
                                                , BasicAuthResult(..)
                                                , Context(EmptyContext, (:.))
                                                , Handler
                                                , Proxy(..)
                                                , Server
                                                , err400
                                                , err404
                                                , errBody
                                                , serveDirectoryWebApp
                                                , serveWithContext
                                                , throwError
                                                , (:<|>)(..)
                                                )
import           System.Environment             ( getEnv )
import           Data.Pool                      ( withResource
                                                , createPool
                                                , Pool
                                                )
import           Control.Exception              ( bracket )
import           Network.AWS                    ( send
                                                , runAWS
                                                , runResourceT
                                                , Env
                                                , newEnv
                                                )
import           Network.AWS.Auth               ( Credentials(Discover) )
import           Network.AWS.S3                 ( ObjectKey(..)
                                                , putObject
                                                )
import           Network.AWS.Data.Body
import           Data.Text                      ( pack )

-- --

handleNewUser :: Pool Connection -> User -> Handler String
handleNewUser conns User {..} = do
  liftIO . withResource conns $ \conn -> execute
    conn
    "INSERT INTO users VALUES (?, ?, ?, date(?))"
    (name, idn, email, registrationDay)
  return "usuario insertado exitosamente"

handleNewMovie :: Env -> Pool Connection -> Movie -> Handler String
handleNewMovie env conns Movie {..} = do
  -- se guarda la imagen del cliente en un balde S3 y se guarda
  -- la URL resultante en la base de datos
  liftIO $ do
    -- TODO: revisar si esta es la mejor manera de cargar archivos 0_0
    file <- readFile url
    _    <-
      runResourceT
      $ runAWS env
      $ send
      $ putObject "colombiavideo" (ObjectKey . pack $ title)
      $ toBody file
    withResource conns $ \conn -> execute
      conn
      "INSERT INTO movies VALUES (?, ?, ?, ?, ?, ?)"
      (title, idn, description, duration, rating, title)
  return "película insertada exitosamente"

queryById
  :: FromRow a => Pool Connection -> ByteString -> Maybe Int -> Handler a
queryById _ _ Nothing = throwError err400 { errBody = "No se especificó ID" }
queryById conns tbl (Just idn) = do
  result <- liftIO . withResource conns $ \conn ->
    query conn ("SELECT * FROM " <> Query tbl <> " WHERE idn = ?") (Only idn)
  if not $ null result
    then return $ head result
    else throwError err404 { errBody = "No Existe" }

allMovies :: Pool Connection -> Handler [Movie]
allMovies conns =
  liftIO . withResource conns $ \conn -> query conn "SELECT * FROM movies" ()

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck $ \(BasicAuthData usr pss) ->
  if usr == "admin" && pss == "123456"
    then return $ Authorized User { name            = "admin"
                                  , idn             = 0
                                  , email = "administrator@colvid.com.co"
                                  , registrationDay = fromOrdinalDate 2010 150
                                  }
    else return Unauthorized

-- este código es una simple demostración
authSimple :: Maybe String -> Maybe String -> Bool
authSimple Nothing    _          = False
authSimple _          Nothing    = False
authSimple (Just usr) (Just pwd) = usr == "admin" && pwd == "123456"

-- database :: String
-- database = "/var/db/testdb.sqlite" -- esta para producción, la de abajo para
-- database = "testdb.sqlite"         -- pruebas

app :: Env -> Pool Connection -> Application
app env database =
  simpleCors
    $    serveWithContext appAPI authContext
    $    userServer
    :<|> movieServer
    :<|> staticServer
    :<|> authServer
 where
  appAPI :: Proxy AppAPI
  appAPI = Proxy

  authContext :: Context (BasicAuthCheck User ': '[])
  authContext = authCheck :. EmptyContext

  userServer :: Server UserAPI
  userServer = queryById database "users" :<|> handleNewUser database

  movieServer :: Server MovieAPI
  movieServer = allMovies database :<|> queryById database "movies" :<|> \_ ->
    handleNewMovie env database

  staticServer :: Server StaticAPI
  staticServer = serveDirectoryWebApp "/var/assets"

  authServer :: Server AuthAPI
  authServer a b = return $ authSimple a b

-- -- -- --

-- |
-- inicializa la base de datos
initDB :: Connection -> IO ()
initDB conn = do
  _ <- execute
    conn
    (  "CREATE TABLE IF NOT EXISTS users (name TEXT,"
    <> " idn INTEGER PRIMARY KEY, email TEXT UNIQUE,"
    <> "registrationDay DATE)"
    )
    ()
  _ <- execute
    conn
    (  "CREATE TABLE IF NOT EXISTS movies (title TEXT,"
    <> " idn INTEGER PRIMARY KEY, description TEXT, "
    <> "duration INTEGER, rating SMALLINT, url TEXT)"
    )
    ()
  return ()

-- |
-- la rutina de IO que se va a ejecutar como servidor.
colvidServer :: IO ()
colvidServer = do
  port   <- getEnv "PORT"
  db     <- getEnv "DATABASE_URL"
  awsEnv <- newEnv Discover
  conn   <- initPool $ fromString db
  bracket (connectPostgreSQL $fromString db) close $ initDB
  run (read port) $ app awsEnv conn
 where
  initPool :: ByteString -> IO (Pool Connection)
  initPool dbStr = createPool (connectPostgreSQL dbStr) close 2 60 10
