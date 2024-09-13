module Servidor where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Aeson (encode, decode)
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import ProtocoloServidor

-- Función principal para iniciar el servidor
main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "3000" -- Puerto en el que va a escuchar el servidor
  sock <- openSocket addr
  putStrLn "Servidor iniciado en el puerto 3000"
  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Cliente conectado: " ++ show peer
    void $ forkFinally (handleClient conn) (\_ -> close conn)

-- Resolver la dirección del socket
resolve :: ServiceName -> IO AddrInfo
resolve port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

-- Abrir el socket del servidor
openSocket :: AddrInfo -> IO Socket
openSocket addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 10
  return sock

-- Manejar las conexiones de los clientes
handleClient :: Socket -> IO ()
handleClient conn = do
  msg <- recv conn 1024 -- Recibe mensaje del cliente
  case decode (BL.fromStrict msg) :: Maybe MensajeServidor of
    Just mensaje -> do
      putStrLn $ "Mensaje recibido: " ++ show mensaje
      let respuesta = procesarMensaje mensaje
      sendAll conn (BL.toStrict $ encode respuesta) -- Enviar respuesta al cliente
    Nothing -> putStrLn "Mensaje inválido"
  handleClient conn -- Sigue escuchando más mensajes

-- Procesa los mensajes de acuerdo a las reglas del servidor
procesarMensaje :: MensajeServidor -> RespuestaServidor
procesarMensaje (Identificar usuario) =
  Respuesta "IDENTIFICAR" "EXITO" usuario
procesarMensaje (CambiarEstado estado) =
  Respuesta "ESTADO" "CAMBIO_EXITOSO" estado
procesarMensaje SolicitarUsuarios =
  Respuesta "USUARIOS" "LISTA" "[lista de usuarios]"
procesarMensaje (TextoPrivado usuario texto) =
  Respuesta "TEXTO_PRIVADO" "ENVIADO" texto
procesarMensaje (TextoPublico texto) =
  Respuesta "TEXTO_PUBLICO" "ENVIADO" texto
procesarMensaje (CrearSala sala) =
  Respuesta "SALA" "CREADA" sala
procesarMensaje Desconectar =
  Respuesta "DESCONECTAR" "EXITOSO" ""

-- Manejo de mensajes inválidos
procesarMensaje _ = Respuesta "ERROR" "MENSAJE_INVALIDO" ""

