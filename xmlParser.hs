import System.IO
import Text.Regex.Posix

data XMLTree = Empty |  XMLTree { tag :: String
                 , attr :: [(String,String)]
                 , childs :: [XMLTree] } deriving (Show)

readXML :: String -> IO String
readXML file = do
  handle <- openFile file ReadMode
  hGetContents handle

addAttr :: XMLTree -> String -> XMLTree
addAttr tree element = XMLTree (tag tree) ((attr tree) ++ [crearAttr element]) (childs tree)

isCloseTagName :: String -> Bool
isCloseTagName string = head string == '<' && ('/' == (head $ tail string))

isTagName :: String -> Bool
isTagName string = head string == '<' && ('/' /= (head $ tail string))

isCloseAttrTag :: String -> Bool
isCloseAttrTag string = last string == '>' && ((last $ init string) == '/')

isCloseTag :: String -> Bool
isCloseTag string tagName = string == ("</" ++ tagName ++ ">")

isAttr :: String -> Bool
isAttr string = let regex = ".*=\".*\""
                in string =~ regex


newXMLTree :: String -> XMLTree
newXMLTree name = XMLTree name [] []

getAttrName :: String -> String
getAttrName str = let aux [] name = ""
                      aux ('=':xs) name = name
                      aux (x:xs) name = aux xs (name ++ [x])
                  in aux str ""


getAttrValue :: String -> String
getAttrValue [] = ""
getAttrValue ('"':xs) = let aux x val = case x of
                              [] -> ""
                              ('"':_) -> val
                              (y:ys) -> aux ys (val++[y])
                       in aux xs ""
getAttrValue (_:xs) = getAttrValue xs
                            
crearAttr :: String -> (String,String)
crearAttr str = (getAttrName str , getAttrValue str)
                             
addChild :: XMLTree -> XMLTree -> XMLTree
addChild parent child = XMLTree (tag parent) (attr parent) (childs parent ++ [child])  


crearXMLTree :: XMLTree -> [String] -> (XMLTree,[String])
crearXMLTree tree [] = (tree,[])
crearXMLTree tree (x:xs) 
  | isCloseAttrTag x = ((addAttr tree x),xs) 
  | isAttr x = crearXMLTree (addAttr tree x) xs
  | isCloseTagName x = (tree,xs) 
  | isTagName x = case tree of Empty -> crearXMLTree (newXMLTree x) xs
                               _ -> let newChildTuple = crearXMLTree (newXMLTree) xs
                                    in let newTree = XMLTree (tag tree) (attr tree) (childs tree ++ [fst newChildTuple])
                                       in crearXMLTree newTree (snd newChildTuple)


  
