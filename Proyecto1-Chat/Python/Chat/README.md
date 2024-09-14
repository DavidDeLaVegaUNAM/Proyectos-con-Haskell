# Proyecto de Chat en Python

Este es un proyecto de chat simple que permite a múltiples clientes conectarse a un servidor utilizando sockets en Python. Los mensajes se envían entre los clientes y se manejan comandos especiales para interacción con el servidor. Además, el servidor maneja el registro de usuarios y la identificación de cada cliente.

## Características

- Los clientes pueden enviar mensajes normales al servidor y a otros usuarios.
- Los clientes pueden enviar comandos especiales al servidor utilizando el prefijo `@<-`.
- Los nombres de los usuarios se asignan con colores aleatorios para mayor diferenciación en los mensajes.
- Manejo de señal `Ctrl + C` para evitar que el programa se cierre inesperadamente.
- Ejecución simultánea del servidor y del cliente en una misma máquina.

## Requisitos

Para ejecutar el proyecto necesitarás:

- Python 3.x
- Conocimientos básicos de sockets en Python
- Conocimientos de threading (hilos) en Python

## Instrucciones de Uso

1.Clona este repositorio en la compu:
git clone https://github.com/tu-usuario/proyecto-chat-python.git
    
2. Instala las dependencias :
 
 - socket
 - json
 - threading
 - signal
 - sys
 -  

3. Para ejecutar puede ser de manera individual o por medio del Main.py

### Modo de Operación

1. Servidor: El servidor inicia en el puerto `3000` por defecto y espera conexiones de los clientes. Los nombres de los usuarios se registran y si un nombre ya está en uso, el cliente será informado para que elija otro nombre.

2. Cliente: Al iniciar el cliente, este se conectará al servidor. El cliente debe ingresar un nombre de usuario único para poder registrarse. Una vez registrado, podrá enviar mensajes o comandos especiales.

3. Mensajes Normales: Los clientes pueden enviar mensajes al servidor que serán transmitidos a todos los clientes conectados.

4. Comandos Especiales: Los clientes pueden enviar comandos especiales al servidor utilizando el prefijo `@<-`. Por ejemplo, para identificarse:
   
    @<- IDENTIFY username

### Manejo de Señales

- Tanto el cliente como el servidor manejan la señal `Ctrl + C` para evitar que el programa se cierre abruptamente. Esto permite un control más fino del cierre de conexiones.

## Ejemplos de Uso

### Comandos Especiales

  @<- IDENTIFY JohnDoe
