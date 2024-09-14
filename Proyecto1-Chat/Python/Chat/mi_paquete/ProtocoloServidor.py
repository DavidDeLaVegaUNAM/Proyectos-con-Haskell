import json
from typing import Union, List, Dict, Any

Usuario = str
Sala = str
Texto = str
Estado = str
Operacion = str
Resultado = str
Extra = str

class MensajeServidor:
    def __init__(self, tipo: str, **kwargs):
        self.tipo = tipo
        for key, value in kwargs.items():
            setattr(self, key, value)

    @classmethod
    def identificar(cls, username: Usuario):
        return cls("IDENTIFY", username=username)

    @classmethod
    def cambiar_estado(cls, status: Estado):
        return cls("CHANGE_STATUS", status=status)

    @classmethod
    def solicitar_usuarios(cls):
        return cls("REQUEST_USERS")

    @classmethod
    def texto_privado(cls, username: Usuario, text: Texto):
        return cls("PRIVATE_TEXT", username=username, text=text)

    @classmethod
    def texto_publico(cls, text: Texto):
        return cls("PUBLIC_TEXT", text=text)

    @classmethod
    def crear_sala(cls, roomname: Sala):
        return cls("NEW_ROOM", roomname=roomname)

    @classmethod
    def invitar(cls, roomname: Sala, usernames: List[Usuario]):
        return cls("INVITE", roomname=roomname, usernames=usernames)

    @classmethod
    def unirse_sala(cls, roomname: Sala):
        return cls("JOIN_ROOM", roomname=roomname)

    @classmethod
    def usuarios_sala(cls, roomname: Sala):
        return cls("ROOM_USERS", roomname=roomname)

    @classmethod
    def texto_sala(cls, roomname: Sala, text: Texto):
        return cls("ROOM_TEXT", roomname=roomname, text=text)

    @classmethod
    def abandonar_sala(cls, roomname: Sala):
        return cls("LEAVE_ROOM", roomname=roomname)

    @classmethod
    def desconectar(cls):
        return cls("DISCONNECT")

class RespuestaServidor:
    def __init__(self, operation: Operacion, result: Resultado, extra: Extra):
        self.operation = operation
        self.result = result
        self.extra = extra

def validar_longitud(max_len: int, nombre: Texto, tipo: Texto) -> Union[str, Texto]:
    if len(nombre) > max_len:
        return f"{tipo} no puede exceder los {max_len} caracteres"
    return nombre

def validar_nombre_usuario(nombre: Texto) -> Union[str, Texto]:
    return validar_longitud(8, nombre, "Usuario")

def validar_nombre_cuarto(nombre: Texto) -> Union[str, Texto]:
    return validar_longitud(16, nombre, "Cuarto")

def from_json(data: Dict[str, Any]) -> MensajeServidor:
    tipo_msg = data.get("type")
    if tipo_msg == "IDENTIFY":
        usuario = data.get("username")
        validado = validar_nombre_usuario(usuario)
        if isinstance(validado, str):
            raise ValueError(validado)
        return MensajeServidor.identificar(validado)
    elif tipo_msg == "NEW_ROOM":
        cuarto = data.get("roomname")
        validado = validar_nombre_cuarto(cuarto)
        if isinstance(validado, str):
            raise ValueError(validado)
        return MensajeServidor.crear_sala(validado)
    # Other cases here
    else:
        raise ValueError(f"Unknown message type: {tipo_msg}")

