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
                               _ -> let newChildTuple = crearXMLTreeAux (newXMLTree (crearTagName x)) xs
                                    in let newTree = XMLTree (tag tree) (attr tree) (childs tree ++ [fst newChildTuple])
                                       in crearXMLTreeAux newTree (snd newChildTuple)

--- Dado una lista de Strings, retorna la estructura de Arbol de Parseo
crearXMLTree :: [String] -> [XMLTree]
crearXMLTree lista = let aux [] listaTree = listaTree
                         aux listaStr listaTree = let newTree = crearXMLTreeAux Empty listaStr
                                                  in aux (snd newTree) (listaTree ++ [fst newTree])
                     in aux lista []

                           

  
