module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (when)
import qualified Servidor as S
import qualified Cliente as C

-- Función principal para ejecutar el servidor o cliente
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["server"] -> S.main -- Ejecuta el servidor
        ["client"] -> C.main -- Ejecuta el cliente
        _ -> do
            putStrLn "Uso: Main [server|client]"
            exitFailure

