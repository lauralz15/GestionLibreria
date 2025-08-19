practice.hs
Práctica 1 - Gestión de Librería

Integrantes:

Laura Sofia Lizarazo
Juan Jose Sierrra

Plataoforma: Visual Estudio Code
Lenguaje: Haskell
Compilador/Intérprete: GHC y runghc
Terminal: PowerShell

Descripción del programa

Permite:

Registrar el préstamo de un libro.
Registrar la devolución de un libro.
Buscar un libro por código.
Listar todos los libros registrados.
Salir del programa.

Los datos se almacenan en el archivo libreria.txt.
Cada registro incluye:

Código del libro
Hora de entrada (cuando fue prestado)
Hora de salida (cuando fue devuelto, si aplica)


Archivos del repositorio

practice.hs : Código fuente principal en Haskell.

Library.txt: archivo mencionado en el enunciado de la práctica.
libreria.txt: archivo generado y utilizado directamente por el programa en Haskell.
README.md : Documento con información de la práctica.

Ejecución

Para ejecutar directamente sin compilar:
runghc practice.hs

