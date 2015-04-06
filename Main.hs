import Text.XML.HXT.Core
import Text.XML.HXT.HTTP

main = do
  xml <- readFile "demo-score.xml"
  let doc = readString [withHTTP [], withValidate no] xml
  links <- runX $ doc //> hasName "part" >>> getAttrValue "id"
  mapM_ putStrLn links



-- HXT contains a class XmlPickler defining a single function xpickle
-- for overloading the xpickle function name:
-- class XmlPickler a where
--   xpickle :: PU a
  
-- For simple data types there is an instance for XmlPickler which uses
-- the primitive pickler xpPrim for conversion from and to XML text node
-- instance XmlPickler Int where
--   xpickle = xpPrim
