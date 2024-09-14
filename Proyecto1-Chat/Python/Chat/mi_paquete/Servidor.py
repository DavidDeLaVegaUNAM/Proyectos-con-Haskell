import socket
import json
import threading
import signal
import sys

usuarios = set()

# Manejador de señal para SIGINT (Ctrl + C)
def signal_handler(sig, frame):
    print("\nServidor recibió Ctrl + C, pero no se interrumpirá inmediatamente.")
    # Aquí puedes agregar lógica adicional para cerrar conexiones si es necesario

def resolver(puerto: str) -> tuple:
    return ('', int(puerto))

def abrir_socket(direccion: tuple) -> socket.socket:
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(direccion)
    sock.listen(10)
    return sock

def manejar_cliente(conn: socket.socket):
    global usuarios
    while True:
        try:
            msg = conn.recv(1024)
            if not msg:
                break

            mensaje = msg.decode()

            if mensaje.startswith("@<-"):
                comando = mensaje[3:].strip()
                if comando.startswith("IDENTIFY"):
                    nombre_usuario = comando.split(" ")[1]
                    if nombre_usuario in usuarios:
                        respuesta = "@-> Nombre en uso. Por favor, elija otro nombre."
                    else:
                        usuarios.add(nombre_usuario)
                        respuesta = f"@-> Nombre de usuario '{nombre_usuario}' registrado exitosamente."
                # Aquí puedes agregar otros comandos especiales que comiencen con "@<-"
                
                conn.sendall(respuesta.encode())
            else:
                conn.sendall("@-> Comando no reconocido".encode())
        except ConnectionResetError:
            break

def main():
    # Asignar el manejador de la señal SIGINT
    signal.signal(signal.SIGINT, signal_handler)

    direccion = resolver("3000")
    sock = abrir_socket(direccion)
    print("Servidor iniciado en el puerto 3000")
    
    while True:
        try:
            conn, peer = sock.accept()
            print(f"Cliente conectado: {peer}")
            hilo_cliente = threading.Thread(target=manejar_cliente, args=(conn,))
            hilo_cliente.start()
        except KeyboardInterrupt:
            print("\nServidor detenido manualmente.")
            break

if __name__ == "__main__":
    main()
