{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Ratio
import Music               -- This is to import the Music type
  
main = do
  d <- runX $ xunpickleDocument xpMusic
    [withValidate no
--    ,withTrace 2
    ,withRemoveWS yes
    ,withPreserveComment no] "demo-score.xml"
    -- >>> arrIO (\x -> do {print x; return x})
  mapM (putStrLn.show) d
  return ()

----------------------------------------------------------------------------------------------------                      
-- Music Type Pickling
----------------------------------------------------------------------------------------------------
instance XmlPickler Music2 where
  xpickle = xpMusic

instance XmlPickler Position where
  xpickle = xpPrim :: PU (Ratio Int)
  
xpMusic :: PU Music2
xpMusic
  = startPt $
    xpList $ xpElem "note" $                       -- Create list out of selected note elems
    keepElems ["pitch", "durationTest"] $          -- Ignore other elems
    xpWrap (uncurry3 Note2,
            \t -> (dur2 t, pitch2 t, mods2 t)) $
    xpTriple (xpElem "durationTest" xpickle)
             (xpElem "pitch" $ keepElem "octave" $ xpElem "octave" xpickle)
             (xpOption (xpAttr "blah" xpPrim))
  where startPt a = xpElem "score-partwise" $                -- Select
                    keepElem "part"    $ xpElem "part"    $  -- Fitler, select
                    keepElem "measure" $ xpElem "measure" $  -- Filter, select
                    keepElem "note" $ a                      -- Filter
        
----------------------------------------------------------------------------------------------------                      
-- Helper functions
----------------------------------------------------------------------------------------------------
keepElems :: [String] -> PU a -> PU a
keepElems ls = let msum' = foldr (<+>) zeroArrow  -- (hasName "a") <+> (hasName "b")
               in xpFilterCont $ msum' (map hasName ls)

keepElem :: String -> PU a -> PU a
keepElem  x  = xpFilterCont (hasName x)

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
    

