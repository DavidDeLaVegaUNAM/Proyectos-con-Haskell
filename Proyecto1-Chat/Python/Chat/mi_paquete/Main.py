import subprocess
import sys
import signal
import Servidor as S
import Cliente as C

# Definir un manejador para la señal SIGINT
def signal_handler(sig, frame):
    print("\nCtrl + C detectado, pero no se interrumpirá el programa principal.")

def main():
    # Asignar el manejador de la señal SIGINT
    signal.signal(signal.SIGINT, signal_handler)

    match sys.argv[1:]:
        case ["server"]:
            S.main()  # Ejecuta solo el servidor
        case ["client"]:
            C.main()  # Ejecuta solo el cliente
        case ["both"]:
            # Ejecuta el servidor en un proceso separado
            servidor_proceso = subprocess.Popen([sys.executable, "servidor.py"])
            print("Servidor iniciado")

            try:
                # Ejecuta el cliente en este proceso
                C.main()
            finally:
                # Asegura que el proceso del servidor se cierre cuando termine el cliente
                servidor_proceso.terminate()
                print("Servidor terminado")
        case _:
            print("Uso: Main [server|client|both]")
            sys.exit(1)

if __name__ == "__main__":
    main()
