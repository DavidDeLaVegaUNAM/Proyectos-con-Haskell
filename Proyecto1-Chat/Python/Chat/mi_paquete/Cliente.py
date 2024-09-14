import socket
import signal
import sys

def signal_handler(sig, frame):
    print("\nCliente recibió Ctrl + C, pero no se interrumpirá inmediatamente.")
    # Puedes añadir lógica para cerrar la conexión con el servidor si es necesario

def main():
    signal.signal(signal.SIGINT, signal_handler)

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    servidor_direccion = ('localhost', 3000)
    sock.connect(servidor_direccion)
    print("Cliente conectado al servidor en localhost:3000")

    try:
        while True:
            nombre_usuario = input("Ingrese su nombre de usuario: ")
            sock.sendall(f"@<- IDENTIFY {nombre_usuario}".encode())

            respuesta = sock.recv(1024).decode()
            if respuesta.startswith("@->"):
                print(respuesta[3:].strip())
                if "registrado exitosamente" in respuesta:
                    break
            else:
                print("Error: ", respuesta)
        
        while True:
            comando = input("Escriba un mensaje o comando (@<- para comandos especiales): ")
            if comando.startswith("@<-"):
                sock.sendall(comando.encode())
            else:
                sock.sendall(comando.encode())  # Enviar mensajes normales
        
            respuesta = sock.recv(1024).decode()
            if respuesta.startswith("@->"):
                print(respuesta[3:].strip())

    except KeyboardInterrupt:
        print("\nCliente detenido manualmente.")
    finally:
        sock.close()

if __name__ == "__main__":
    main()


