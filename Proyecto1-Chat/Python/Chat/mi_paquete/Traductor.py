import json
from typing import List, Union, Dict, Any

# Tipos de Mensajes
Usuario = str
Sala = str
Texto = str
Estado = str
Operacion = str
Resultado = str
Extra = str

class Identificar:
    def __init__(self, username: Usuario):
        self.username = username

class CambiarEstado:
    def __init__(self, status: Estado):
        self.status = status

class SolicitarUsuarios:
    pass

class TextoPrivado:
    def __init__(self, username: Usuario, text: Texto):
        self.username = username
        self.text = text

class TextoPublico:
    def __init__(self, text: Texto):
        self.text = text

class CrearSala:
    def __init__(self, roomname: Sala):
        self.roomname = roomname

class Invitar:
    def __init__(self, roomname: Sala, usernames: List[Usuario]):
        self.roomname = roomname
        self.usernames = usernames

class UnirseSala:
    def __init__(self, roomname: Sala):
        self.roomname = roomname

class UsuariosSala:
    def __init__(self, roomname: Sala):
        self.roomname = roomname

class TextoSala:
    def __init__(self, roomname: Sala, text: Texto):
        self.roomname = roomname
        self.text = text

class AbandonarSala:
    def __init__(self, roomname: Sala):
        self.roomname = roomname

class Desconectar:
    pass

TipoMsg = Union[Identificar, CambiarEstado, SolicitarUsuarios, TextoPrivado, TextoPublico, CrearSala, Invitar, UnirseSala, UsuariosSala, TextoSala, AbandonarSala, Desconectar]

def parse_tipo_msg(json_data: str) -> TipoMsg:
    v = json.loads(json_data)
    tipo_msg = v.get("type")
    
    match tipo_msg:
        case "IDENTIFY":
            return Identificar(username=v["username"])
        case "CHANGE_STATUS":
            return CambiarEstado(status=v["status"])
        case "REQUEST_USERS":
            return SolicitarUsuarios()
        case "PRIVATE_TEXT":
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

def to_json(msg: TipoMsg) -> str:
    if isinstance(msg, Identificar):
        return json.dumps({"type": "IDENTIFY", "username": msg.username})
    elif isinstance(msg, CambiarEstado):
        return json.dumps({"type": "CHANGE_STATUS", "status": msg.status})
    elif isinstance(msg, SolicitarUsuarios):
        return json.dumps({"type": "REQUEST_USERS"})
    elif isinstance(msg, TextoPrivado):
        return json.dumps({"type": "PRIVATE_TEXT", "username": msg.username, "text": msg.text})
    elif isinstance(msg, TextoPublico):
        return json.dumps({"type": "PUBLIC_TEXT", "text": msg.text})
    elif isinstance(msg, CrearSala):
        return json.dumps({"type": "NEW_ROOM", "roomname": msg.roomname})
    elif isinstance(msg, Invitar):
        return json.dumps({"type": "INVITE", "roomname": msg.roomname, "usernames": msg.usernames})
    elif isinstance(msg, UnirseSala):
        return json.dumps({"type": "JOIN_ROOM", "roomname": msg.roomname})
    elif isinstance(msg, UsuariosSala):
        return json.dumps({"type": "ROOM_USERS", "roomname": msg.roomname})
    elif isinstance(msg, TextoSala):
        return json.dumps({"type": "ROOM_TEXT", "roomname": msg.roomname, "text": msg.text})
    elif isinstance(msg, AbandonarSala):
        return json.dumps({"type": "LEAVE_ROOM", "roomname": msg.roomname})
    elif isinstance(msg, Desconectar):
        return json.dumps({"type": "DISCONNECT"})
    else:
        raise ValueError(f"Unknown message type: {msg}")

def parse_lista_usuarios(value: Any) -> List[Dict[str, str]]:
    if isinstance(value, dict):
        return [{"username": k, "status": v} for k, v in value.items()]
    else:
        raise ValueError("Error con el JSON, lista de usuarios no disponible")

