module Main where

import qualified Server (colvidServer)

main :: IO ()
main = do
  putStrLn "Running server now..."
  Server.colvidServer
