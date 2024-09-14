import os

# Define la estructura del proyecto
estructura = {
    'mi_proyecto': [
        'mi_paquete/__init__.py',
        'mi_paquete/modulo1.py',
        'mi_paquete/modulo2.py',
        'mi_paquete/subpaquete/__init__.py',
        'mi_paquete/subpaquete/submodulo1.py',
        'tests/__init__.py',
        'tests/test_modulo1.py',
        'scripts/script_util.py',
        'data/data_file.csv',
        '.gitignore',
        'README.md',
        'requirements.txt',
        'setup.py',
        'pyproject.toml',
        'LICENSE'
    ]
}

# Crea los directorios y archivos
def crear_estructura():
    for directorio, archivos in estructura.items():
        for archivo in archivos:
            ruta = os.path.join(directorio, archivo)
            ruta_directorio = os.path.dirname(ruta)
            os.makedirs(ruta_directorio, exist_ok=True)  # Crea los directorios
            with open(ruta, 'w') as f:
                pass  # Crea archivos vacíos

    print(f"Estructura del proyecto '{list(estructura.keys())[0]}' creada exitosamente.")

if __name__ == "__main__":
    crear_estructura()
