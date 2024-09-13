{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ProtocoloServidor where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson.Types (Parser)

type Usuario = Text
type Sala = Text
type Texto = Text
type Estado = Text
type Operacion = Text
type Resultado = Text
type Extra = Text

data MensajeServidor
  = Identificar { username :: Usuario }
  | CambiarEstado { status :: Estado }
  | SolicitarUsuarios
  | TextoPrivado { username :: Usuario, text :: Texto }
  | TextoPublico { text :: Texto }
  | CrearSala { roomname :: Sala }
  | Invitar { roomname :: Sala, usernames :: [Usuario] }
  | UnirseSala { roomname :: Sala }
  | UsuariosSala { roomname :: Sala }
  | TextoSala { roomname :: Sala, text :: Texto }
  | AbandonarSala { roomname :: Sala }
  | Desconectar
  deriving (Show, Generic)

data RespuestaServidor
  = Respuesta { operation :: Operacion, result :: Resultado, extra :: Extra }
  deriving (Show, Generic)

-- Validation Functions
validarLongitud :: Int -> Text -> Text -> Either String Text
validarLongitud maxLen nombre tipo
  | T.length nombre > maxLen = Left $ T.unpack tipo ++ " no puede exceder los " ++ show maxLen ++ " caracteres"
  | otherwise = Right nombre

validarNombreUsuario :: Text -> Either String Text
validarNombreUsuario = validarLongitud 8 "Usuario"

validarNombreCuarto :: Text -> Either String Text
validarNombreCuarto = validarLongitud 16 "Cuarto"

-- FromJSON Instances for decoding
instance FromJSON MensajeServidor where
  parseJSON = withObject "MensajeServidor" $ \v -> do
    tipoMsg <- v .: "type" :: Parser Text
    case tipoMsg of
      "IDENTIFY" -> do
        usuario <- v .: "username"
        case validarNombreUsuario usuario of
          Left err -> fail err
          Right usuarioValido -> return $ Identificar usuarioValido
      "NEW_ROOM" -> do
        cuarto <- v .: "roomname"
        case validarNombreCuarto cuarto of
          Left err -> fail err
          Right cuartoValido -> return $ CrearSala cuartoValido
      -- Other cases here
      _ -> fail $ "Unknown message type: " ++ T.unpack tipoMsg

