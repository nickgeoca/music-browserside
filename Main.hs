{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Ratio
import Music               -- This is to import the Music type
  
-- main = do
--   xml <- readFile "demo-score.xml"
--   let doc = readString [withHTTP [], withValidate no] xml
--   links <- runX $ doc //> hasName "part" >>> getAttrValue "id"
--   mapM_ putStrLn links
--   let key = 3 :: KeyH
--   putStrLn $ show key
--   print1234

main = do
  d <- runX $ xunpickleDocument xpMusic
    [withValidate no
--    ,withTrace 2
    ,withRemoveWS yes
    ,withPreserveComment no] "demo-score.xml"
    -- >>>
    -- processXmlNotSureWhatThisDoesYet
  mapM (putStrLn.show) d
  return ()

-- processXmlNotSureWhatThisDoesYet = arrIO (\x -> do {print x; return x})

----------------------------------------------------------------------------------------------------                      
-- Music Type Pickling
----------------------------------------------------------------------------------------------------
instance XmlPickler Music2 where
  xpickle = xpMusic

instance XmlPickler Position where
  xpickle = xpPrim :: PU (Ratio Int)
  
xpMusic :: PU Music2
xpMusic
  = devFilt $
    xpList $
    xpElem "note" $
    keepElems ["pitch", "durationTest"] $
    xpWrap (uncurry3 Note2,
            \t -> (position2 t, pitch2 t, mods2 t)) $
    xpTriple (xpElem "durationTest" xpickle)
             (xpElem "pitch" $ xpFilterCont (hasName "octave") $ xpElem "octave" xpickle)
             (xpOption (xpAttr "blah" xpPrim))

  where findPart1 a = xpElem "score-partwise" $
                      xpFilterCont (hasName "part") $
                      xpElem "part" $ a
        findMeas1 a = xpFilterCont (hasName "measure") $ xpElem "measure" $ a
        filtNote1 a = xpFilterCont (hasName "note") $ a
        keepElems ls= xpFilterCont $ foldr (<+>) zeroArrow $ map hasName ls
        devFilt   a = findPart1 $ findMeas1 $ filtNote1 $ a 
    
--    (Pos, MusElm) -- (1, NoteElm $ Note (1%4) 48 []) -- Note Duration Pitch [NoteRestMod]
    -- xpPair (xpElem "position" xpickle :: PU Position) undefined -- (1, NoteElm $ Note (1%4) 48 [])
    -- xpPair (0%0) (1, NoteElm $ Note (1%4) 48 [])

          
----------------------------------------------------------------------------------------------------                      
-- Example, figuring stuff out
----------------------------------------------------------------------------------------------------
type Title = String
  
instance XmlPickler Title where
  xpickle = xpTitle

-- xpTitle :: PU Title
xpTitle
  = xpElem "score-partwise" $
    xpFilterCont (hasName "movement-title") $
    xpElem "movement-title" xpText

{-
  = xpElem "score-partwise" $ xpFilterCont (hasName "abc123") $ xpAttr "title" xpText    
<score-partwise title="Pitches and accidentals">
  <identification>
    <miscellaneous>
-}
    
----------------------------------------------------------------------------------------------------                      
-- HXT Documentation
----------------------------------------------------------------------------------------------------
{-
HXT contains a class XmlPickler defining a single function xpickle
for overloading the xpickle function name:
class XmlPickler a where
  xpickle :: PU a
  
For simple data types there is an instance for XmlPickler which uses
the primitive pickler xpPrim for conversion from and to XML text node
instance XmlPickler Int where
  xpickle = xpPrim


xpList takes a pickler for an element type and gives a list pickler
instance XmlPickler a => XmlPickler [a] where
 xpickle = xpList xpickle
                                           
In addition to those picklers, we need picklers for generating/reading element and attribute nodes:
xpElem generates/parses an XML element node
xpAttr generates/parses an attribute node

--------------------------------------------------
-- Picklers are developed top down. Season is top level type

instance XmlPickler Season where
    xpickle = xpSeason
-------------------------
xpSeason	:: PU Season
xpSeason
    = xpElem "SEASON" $
      xpWrap ( uncurry Season
	     , \ s -> (sYear s, sLeagues s)) $
      xpPair (xpAttr "YEAR" xpickle) xpLeagues
-------------------------
import Data.Map
 
data Season = Season
  { sYear    :: Int
  , sLeagues :: Leagues
  } deriving (Show, Eq)
 
type Leagues = Map String Divisions
 
type Divisions = Map String [Team]
 
data Team = Team
  { teamName :: String
  , city     :: String
  , players  :: [Player]
  } deriving (Show, Eq)
 
data Player = Player
  { firstName :: String
  , lastName  :: String
  , position  :: String
  , atBats    :: Maybe Int
  , hits      :: Maybe Int
  , era       :: Maybe Float
  } deriving (Show, Eq)

-}


-- /usr/bin/setxkbmap -option "ctrl:swapcaps"

  
