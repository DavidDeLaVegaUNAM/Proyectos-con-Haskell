{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ProtocolClien where

import Data.Aeson (FromJSON(..), (.:), withObject)
import Data.Aeson.Types (Parser)  -- Import Parser
import Data.Text (Text)
import qualified Data.Text as T

-- Message Types
data TipoMsg
    = Identificar Text
    | CambiarEstado Text
    | SolicitarUsuarios
    | TextoPrivado Text Text
    | TextoPublico Text
    | CrearSala Text
    | Invitar Text [Text]
    | UnirseSala Text
    | UsuariosSala Text
    | TextoSala Text Text
    | AbandonarSala Text
    | Desconectar
    deriving (Show)

-- JSON Parsing for TipoMsg
instance FromJSON TipoMsg where
  parseJSON = withObject "TipoMsg" $ \v -> do
    tipoMsg <- v .: "type" :: Parser Text  -- Explicitly annotate as Text
    case tipoMsg of
      "IDENTIFY"     -> Identificar <$> v .: "username"
      "STATUS"       -> CambiarEstado <$> v .: "status"
      "USERS"        -> pure SolicitarUsuarios
      "TEXT"         -> TextoPrivado <$> v .: "username" <*> v .: "text"
      "PUBLIC_TEXT"  -> TextoPublico <$> v .: "text"
      "NEW_ROOM"     -> CrearSala <$> v .: "roomname"
      "INVITE"       -> Invitar <$> v .: "roomname" <*> v .: "usernames"
      "JOIN_ROOM"    -> UnirseSala <$> v .: "roomname"
      "ROOM_USERS"   -> UsuariosSala <$> v .: "roomname"
      "ROOM_TEXT"    -> TextoSala <$> v .: "roomname" <*> v .: "text"
      "LEAVE_ROOM"   -> AbandonarSala <$> v .: "roomname"
      "DISCONNECT"   -> pure Desconectar
      _              -> fail $ "Unknown message type: " ++ T.unpack tipoMsg

