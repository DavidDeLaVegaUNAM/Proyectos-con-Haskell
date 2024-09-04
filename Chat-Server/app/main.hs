{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Server (startServer)
import Client (startClient)

main :: IO ()
main = do
    -- Inicia el servidor en un hilo separado
    void $ forkIO startServer
    
    -- Esperar un momento para asegurar que el servidor esté listo
    threadDelay 1000000
    
    -- Iniciar el cliente en otro hilo
    void $ forkIO startClient
    
    -- Mantener el programa corriendo
    putStrLn "Presiona enter para salir"
    _ <- getLine
    import Control.Concurrent (threadDelay)

    main :: IO ()
    main = do
        -- Inicia el servidor en un hilo separado
        void $ forkIO startServer
        
        -- Esperar un momento para asegurar que el servidor esté listo
        threadDelay 1000000
        
        -- Iniciar el cliente en otro hilo
        void $ forkIO startClient
        
        -- Mantener el programa corriendo
        putStrLn "Presiona enter para salir"
        _ <- getLine
        return ()
