import System.IO
import Text.Regex.Posix

-- Estructura recursiva del Arbol de Parseo --
-- TagName = String
-- Atributos = lista de tuplas (nombre,valor)
-- nodos Hijos = lista de arboles
data XMLTree = Empty |  XMLTree { tag :: String
                 , attr :: [(String,String)]
                 , childs :: [XMLTree] } deriving (Show)


--- Lee Archivo .xml
readXML :: String -> IO String
readXML file = do
  handle <- openFile file ReadMode
  hGetContents handle	
	
-- Agrega un atributo al arbol
addAttr :: XMLTree -> String -> XMLTree
addAttr tree element = XMLTree (tag tree) ((attr tree) ++ [crearAttr element]) (childs tree)

-- Determina si un String es un tag de Cierre
isCloseTagName :: String -> Bool
isCloseTagName string = head string == '<' && ('/' == (head $ tail string))

--- Determina si un String es un tag de apertura
isTagName :: String -> Bool
isTagName string = head string == '<' && ('/' /= (head $ tail string))

--- Determina si el String ingresado representa un attributo y a la vez cierra el tag
isCloseAttrTag :: String -> Bool
isCloseAttrTag string = last string == '>' && ((last $ init string) == '/')

--- Dado el nombre de un árbol y un String, determina si el String representa el tag de cierre del arbol
isCloseTag :: String -> String -> Bool
isCloseTag string tagName = string == ("</" ++ tagName ++ ">")

--- Determina si el String representa un atributo mediante una regex
isAttr :: String -> Bool
isAttr string = let regex = ".*=\".*\""
                in string =~ regex


--- Crea un nuevo arbolXML unicamente con el tag de nombre, y con los atributos e hijos vacíos
newXMLTree :: String -> XMLTree
newXMLTree name = XMLTree name [] []

--- Dado un Strig atributo de la forma name="value", retorna el nombre del atributo
getAttrName :: String -> String
getAttrName str = let aux [] name = ""
                      aux ('=':xs) name = name
                      aux (x:xs) name = aux xs (name ++ [x])
                  in aux str ""

--- Dado un String atributo de la forma name="value", retorma el valor del atributo
getAttrValue :: String -> String
getAttrValue [] = ""
getAttrValue ('"':xs) = let aux x val = case x of
                              [] -> ""
                              ('"':_) -> val
                              (y:ys) -> aux ys (val++[y])
                       in aux xs ""
getAttrValue (_:xs) = getAttrValue xs

--- Dado un String devuelve un String que representa un Tag Name
crearTagName :: String -> String
crearTagName string
  | (last string) == '>' = tail ( init string )
  | otherwise = tail string

--Dado un String, retorna una tupla (nombre, valor) que representa a un atributo
crearAttr :: String -> (String,String)
crearAttr str = (getAttrName str , getAttrValue str)

-- Dado un Arbol padre y un Arbol Hijo, retorna un nuevo Árbol con el Arbol hijo contenido dentro del arbol Padre
addChild :: XMLTree -> XMLTree -> XMLTree
addChild parent child = XMLTree (tag parent) (attr parent) (childs parent ++ [child])  

--- Funcion Auxiliar para la creación del Arbol de parseo
crearXMLTreeAux :: XMLTree -> [String] -> (XMLTree,[String])
crearXMLTreeAux tree [] = (tree,[])
crearXMLTreeAux tree (x:xs) 
  | isCloseAttrTag x = ((addAttr tree x),xs) 
  | isAttr x = crearXMLTreeAux (addAttr tree x) xs
  | isCloseTagName x = (tree,xs) 
  | isTagName x = case tree of Empty -> crearXMLTreeAux (newXMLTree (crearTagName x)) xs
                               _ -> let newChildTuple =
                                          crearXMLTreeAux (newXMLTree (crearTagName x)) xs
                                    in let newTree =
                                             XMLTree (tag tree) (attr tree) (childs tree ++
                                                                             [fst newChildTuple])
                                       in crearXMLTreeAux newTree (snd newChildTuple)
  | otherwise = crearXMLTreeAux tree (unirAttr (x:xs))

--- Dado una lista de Strings, retorna la estructura de Arbol de Parseo
crearXMLTree :: [String] -> [XMLTree]
crearXMLTree lista = let aux [] listaTree = listaTree
                         aux listaStr listaTree = let newTree = crearXMLTreeAux Empty listaStr
                                                  in aux (snd newTree) (listaTree ++ [fst newTree])
                     in aux lista []


startLista :: [String] -> [String]
startLista [] = []
startLista (x:xs)
  | x == "</version>" = init xs
  | otherwise = startLista xs

unirAttr :: [String] -> [String]                
unirAttr (x:xs:xss)
  | (xs =~ ".*\"") == True = (x++(' ':xs)):xss
  | (xs =~ ".*\"/>") == True = (x++(' ':xs)):xss
  | otherwise = unirAttr ( (x++(' ':xs)):xss )
  

buscarAttrInAttrList :: [(String,String)] -> (String,String) -> Integer
buscarAttrInAttrList listaAttr tuple = let aux [] _ n = n
                                           aux ((name,val):xs) (name1,val1) n
                                             | (name == name1) && (val == val1) = aux xs (name1,val1) (n+1)
                                             | otherwise = aux xs (name1,val1) n
                                       in aux listaAttr tuple 0

buscarAttrInTreeList :: [XMLTree] -> (String,String) -> Integer
buscarAttrInTreeList tree tuple = let aux  [] _ n = n
                                      aux (x:xs) tup n =
                                        aux xs tup ((buscarAttrInAttrList (attr x) tup) +
                                                    (buscarAttrInTreeList (childs x) tup) + n)
                                  in aux tree tuple 0

--- Dado un string q representa al archivo xml, devuelve el xml ordenado en forma de una lista de XMLTrees
iniciarTree :: String -> [XMLTree]
iniciarTree str = crearXMLTree $ words str

-- Determina si el string es un atributo de la forma fall_back="nokia_generic_series80"
fallback :: String -> Bool
fallback str = let regex = "fall_back=\"nokia_generic_series80\""
               in str =~ regex

-- Determina si el String es un atributo de la forma name="built_in_camera"
builtCamera :: String -> Bool
builtCamera str= let regex = "name=\"built_in_camera\""
                  in str =~ regex

-- Determina si el String es un atributo de la forma value="true"
valueCamera :: String -> Bool
valueCamera str = let regex = "value=\"true\""
                  in str =~ regex

--- Determina si el String es un atributo tipo id
valueId :: String -> Bool
valueId str = let regex = "id=\".*\""
              in str =~ regex

-- Dado un String, determina si es un tag de inicio de un device                 
isDeviceTag :: String -> Bool
isDeviceTag str = let regex = "<device"
               in str=~ regex
                  
--- Dado una reg expression y un String, devuelve Bool indicando si la reg. Expression coincide con el String
device :: String -> String -> Bool
device re str = str =~ re

--- Devuelve 
buscarNumFallback :: [String] -> String -> [String] -> [String]
buscarNumFallback [] id lista = lista
buscarNumFallback (x:xs) id lista
  | isDeviceTag x = buscarNumFallback xs (getAttrValue (head xs)) lista
  | (fallback x) == True = buscarNumFallback xs "" (lista++[id])
  | otherwise = buscarNumFallback xs id lista

--- Devuelve lista de ids de Devices q posean built_in_camera
buscarBuiltCamera :: [String] -> String -> [String] -> [String]
buscarBuiltCamera [] id lista = lista
buscarBuiltCamera (x1:x2:[]) id lista = lista
buscarBuiltCamera (x1:x2:xs) id lista
  | isDeviceTag x1 = buscarBuiltCamera xs (getAttrValue x2) lista
  | (builtCamera x1) && (valueCamera x2) = buscarBuiltCamera xs "" (lista++[id])
  | otherwise = buscarBuiltCamera (x2:xs) id lista
    

-- Dada una listaXML retorna el num de Devices que contiene
buscarNumDevices :: [String] -> Integer
buscarNumDevices lista = let aux (x:xs) n
                               | isDeviceTag x = aux xs (n+1)
                               | otherwise = aux xs n
                         in aux lista 0

-- Devuelve lista de ids de Devices que contengan el substring str contenidos en una lista
buscarInId :: [String] -> [String] -> String
buscarInId [] lista _ = lista
buscarInId (x1:x2:[]) lista str = lista
buscarInId (x1:x2:xs) lista str
  | (isDeviceTag x1) && (device str x2) = buscarInId xs (lista++[(getAttrValue x2)]) str
  | otherwise = buscarInId (x2:xs) lista str
