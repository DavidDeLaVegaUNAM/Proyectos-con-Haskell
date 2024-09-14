module Cliente where

import ProtocoloServidor
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Aeson (encode, decode)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)

-- Función principal para iniciar el cliente
main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "localhost" "3000" -- Dirección del servidor
    sock <- openSocket addr
    putStrLn "Cliente conectado al servidor en localhost:3000"

    -- Iniciar ciclo para enviar y recibir mensajes
    forever $ do
        putStrLn "Escribe un comando (IDENTIFY, STATUS, USERS, etc.):"
        comando <- getLine
        let mensaje = construirMensaje comando
        sendAll sock (BL.toStrict $ encode mensaje) -- Enviar mensaje al servidor

        respuesta <- recv sock 1024 -- Recibir respuesta del servidor
        putStrLn $ "Respuesta del servidor: " ++ mostrarRespuesta respuesta

-- Resolver dirección del socket
resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

-- Abrir el socket del cliente
openSocket :: AddrInfo -> IO Socket
openSocket addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock

-- Construir un mensaje basado en el comando ingresado por el usuario
construirMensaje :: String -> MensajeServidor
construirMensaje comando =
    case words comando of
        ("IDENTIFY":nombre:_)     -> Identificar nombre
        ("STATUS":estado:_)       -> CambiarEstado estado
        ("USERS":_)               -> SolicitarUsuarios
        ("TEXT":usuario:texto)    -> TextoPrivado usuario (unwords texto)
        ("PUBLIC_TEXT":texto)     -> TextoPublico (unwords texto)
        ("NEW_ROOM":nombreSala:_) -> CrearSala nombreSala
        ("LEAVE_ROOM":nombreSala:_) -> AbandonarSala nombreSala
        _                         -> error "Comando no válido"

-- Mostrar la respuesta del servidor de forma legible
mostrarRespuesta :: B8.ByteString -> String
mostrarRespuesta respuesta =
    let decoded = decode (BL.fromStrict respuesta) :: Maybe RespuestaServidor
    in case decoded of
        Just res -> show res
        Nothing  -> "Respuesta inválida"

