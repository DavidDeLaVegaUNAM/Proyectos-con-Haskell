{-# LANGUAGE OverloadedStrings #-}

module Client where

import Network.Socket
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode, encode)
import ChatProtocol

-- Función para iniciar el cliente y enviar un mensaje
startClient :: IO ()
startClient = do
    addr <- resolve "127.0.0.1" "3000"
    sock <- open addr
    let identifyMessage = ChatMessage Identify (Just "newUser") Nothing Nothing Nothing Nothing Nothing Nothing
    BL.hPut sock (encode identifyMessage)
    response <- BL.hGetContents sock
    case decode response :: Maybe ChatMessage of
        Just chatMsg -> print chatMsg
        Nothing -> putStrLn "Respuesta inválida recibida"
    close sock

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock