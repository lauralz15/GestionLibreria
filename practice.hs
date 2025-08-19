import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe, fromMaybe)
import System.Directory (removeFile, renameFile, doesFileExist)
import Text.Read (readMaybe) 

-- Definición del tipo de datos para representar la información de un libro
data Libro = Libro {
    codigo :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime 
} deriving (Show, Read)

-- Función para dividir cadenas
splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn delim s =
    let (item, rest) = break (== delim) s
    in item : case rest of
                [] -> []
                (_:xs) -> splitOn delim xs

-- Función segura para leer libros
leerlibro :: String -> Maybe Libro
leerlibro linea = case splitOn ',' linea of
    [c, e, s] -> Libro c <$> readMaybe e <*> readMaybeSalida s
    _ -> Nothing
  where
    readMaybeSalida "Nothing" = Just Nothing
    readMaybeSalida s = Just <$> readMaybe s

-- Función para registrar la entrada de un libro
registrarPrestamo :: String -> UTCTime -> [Libro] -> [Libro]
registrarPrestamo codigolibro tiempo libreria =
    Libro codigolibro tiempo Nothing : libreria

-- Función para registrar la salida de un libro
registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro]
registrarDevolucion codigolibro tiempo libreria =
    map (\v -> if codigolibro == codigo v then v { salida = Just tiempo } else v) libreria

-- Función para buscar un libro por su código
buscarlibro :: String -> [Libro] -> Maybe Libro
buscarlibro codigolibro libreria =
    find (\v -> codigolibro == codigo v && isNothing (salida v)) libreria
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo en préstamo
tiempoEnPrestamo :: Libro -> UTCTime -> NominalDiffTime
tiempoEnPrestamo libro tiempoActual =
    case salida libro of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada libro)
        Nothing           -> diffUTCTime tiempoActual (entrada libro)

-- Función para guardar la información
guardarBiblioteca :: [Libro] -> IO ()
guardarBiblioteca libreria = do
    let tempFile = "libreria.tmp"
        contenido = unlines (map mostrarlibro libreria)
    writeFile tempFile contenido
    removeFile "libreria.txt" `catch` (\(_ :: IOException) -> return ())
    renameFile tempFile "libreria.txt"
    putStrLn "Datos guardados correctamente."

-- Función para mostrar información del libro
mostrarlibro :: Libro -> String
mostrarlibro libro =
    codigo libro ++ "," ++ show (entrada libro) ++ "," ++ show (salida libro)

-- Función para cargar la información
cargarBiblioteca :: IO [Libro]
cargarBiblioteca = do
    exists <- doesFileExist "libreria.txt"
    if exists then do
        contenido <- readFile "libreria.txt"
        return (mapMaybe leerlibro (lines contenido))
    else return []

-- Ciclo principal del programa
cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal libreria = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar préstamo de libro"
    putStrLn "2. Registrar devolución de libro"
    putStrLn "3. Buscar libro por código"
    putStrLn "4. Listar los libros"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingrese el código del libro: "
            hFlush stdout  
            codigolibro <- getLine
            tiempoActual <- getCurrentTime
            let libreriaActualizado = registrarPrestamo codigolibro tiempoActual libreria
            putStrLn $ "Libro '" ++ codigolibro ++ "' registrado."
            guardarBiblioteca libreriaActualizado
            cicloPrincipal libreriaActualizado

        "2" -> do
            putStr "Ingrese el código del libro a devolver: "
            hFlush stdout  
            codigolibro <- getLine
            tiempoActual <- getCurrentTime
            let libreriaActualizado = registrarDevolucion codigolibro tiempoActual libreria
            putStrLn $ "Libro '" ++ codigolibro ++ "' devuelto."
            guardarBiblioteca libreriaActualizado
            cicloPrincipal libreriaActualizado

        "3" -> do
            putStr "Ingrese el código a buscar: "
            hFlush stdout  
            codigolibro <- getLine
            case buscarlibro codigolibro libreria of
                Just libro -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnPrestamo libro tiempoActual
                    putStrLn $ "Libro encontrado. Tiempo en préstamo: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Libro no encontrado."
            cicloPrincipal libreria

        "4" -> do
            putStrLn "\nLibros registrados:"
            mapM_ (\v -> putStrLn $ "- Código: " ++ codigo v ++ 
                        ", Entrada: " ++ show (entrada v) ++ 
                        ", Salida: " ++ maybe "En préstamo" show (salida v)) libreria
            cicloPrincipal libreria

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida"
            cicloPrincipal libreria

-- Función principal
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering   
    putStrLn "¡Sistema de Gestión de Librería!"
    libreria <- cargarBiblioteca
    cicloPrincipal libreria