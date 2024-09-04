{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.Socket
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (forkFinally)
import Data.Aeson (decode, encode)
import ChatProtocol

-- Función para iniciar el servidor
startServer :: IO ()
startServer = do
    addr <- resolve "3000"
    sock <- open addr
    putStrLn "Servidor escuchando en el puerto 3000"
    mainLoop sock

-- Configuración de la dirección del servidor
resolve :: ServiceName -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) Nothing (Just port)

-- Abrir el socket del servidor
open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    bind sock (addrAddress addr)
    listen sock 10
    return sock

-- Bucle principal del servidor para aceptar conexiones
mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    forkFinally (handleClient conn) (\_ -> close (fst conn))
    mainLoop sock

-- Función para manejar cada cliente
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (sock, _) = do
    msg <- BL.hGetContents sock
    case decode msg :: Maybe ChatMessage of
        Just chatMsg -> do
            response <- processMessage chatMsg
            BL.hPut sock (encode response)
        Nothing -> putStrLn "Mensaje inválido recibido"
    close sock

-- Procesamiento del mensaje basado en su tipo
processMessage :: ChatMessage -> IO ChatMessage
processMessage msg = case messageType msg of
    Identify -> handleIdentify msg
    -- Aquí añadirás los demás casos para otros tipos de mensaje
    _ -> return msg

-- Ejemplo de función para manejar la identificación de usuarios
handleIdentify :: ChatMessage -> IO ChatMessage
handleIdentify msg = do
    let user = username msg
    -- Lógica para identificar al usuario
    if user == Just "existingUser"
        then return $ ChatMessage Response (Just "IDENTIFY") Nothing Nothing (Just "USER_ALREADY_EXISTS") user Nothing
        else return $ ChatMessage Response (Just "IDENTIFY") Nothing Nothing (Just "SUCCESS") user Nothing
