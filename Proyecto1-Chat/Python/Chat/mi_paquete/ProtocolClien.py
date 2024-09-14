import json
from dataclasses import dataclass
from typing import List, Union

#Tipos de Mensajes
@dataclass
class Identificar:
    username: str

@dataclass
class CambiarEstado:
    status: str

@dataclass
class SolicitarUsuarios:
    pass

@dataclass
class TextoPrivado:
    username: str
    text: str

@dataclass
class TextoPublico:
    text: str

@dataclass
class CrearSala:
    roomname: str

@dataclass
class Invitar:
    roomname: str
    usernames: List[str]

@dataclass
class UnirseSala:
    roomname: str

@dataclass
class UsuariosSala:
    roomname: str

@dataclass
class TextoSala:
    roomname: str
    text: str

@dataclass
class AbandonarSala:
    roomname: str

@dataclass
class Desconectar:
    pass

TipoMsg = Union[Identificar, CambiarEstado, SolicitarUsuarios, TextoPrivado, TextoPublico, CrearSala, Invitar, UnirseSala, UsuariosSala, TextoSala, AbandonarSala, Desconectar]

# Parseando JSOn para tipo mensaje
def parse_tipo_msg(json_data: str) -> TipoMsg:
    v = json.loads(json_data)
    tipo_msg = v.get("type")
    
    match tipo_msg:
        case "IDENTIFY":
            return Identificar(username=v["username"])
        case "STATUS":
            return CambiarEstado(status=v["status"])
        case "USERS":
            return SolicitarUsuarios()
        case "TEXT":
            return TextoPrivado(username=v["username"], text=v["text"])
        case "PUBLIC_TEXT":
            return TextoPublico(text=v["text"])
        case "NEW_ROOM":
            return CrearSala(roomname=v["roomname"])
        case "INVITE":
            return Invitar(roomname=v["roomname"], usernames=v["usernames"])
        case "JOIN_ROOM":
            return UnirseSala(roomname=v["roomname"])
        case "ROOM_USERS":
            return UsuariosSala(roomname=v["roomname"])
        case "ROOM_TEXT":
            return TextoSala(roomname=v["roomname"], text=v["text"])
        case "LEAVE_ROOM":
            return AbandonarSala(roomname=v["roomname"])
        case "DISCONNECT":
            return Desconectar()
        case _:
            raise ValueError(f"Unknown message type: {tipo_msg}")

