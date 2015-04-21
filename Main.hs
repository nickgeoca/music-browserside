{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Ratio
import Control.Monad.State.Lazy
import Music               -- This is to import the Music type
import Data.Default  

  
main = do
  d <- runX $ xunpickleDocument pmusic
    [withValidate no
    -- ,withTrace 2
    ,withRemoveWS yes
    ,withPreserveComment no] "demo-score.xml"             
    -- >>> arrIO (\x -> do {print x; return x})
  let d2 = postProcessing d                    
  mapM print d2
  return ()
  where postProcessing = id.head -- fixPositions 

----------------------------------------------------------------------------------------------------                      
-- MusicXml Pickler functions
----------------------------------------------------------------------------------------------------

pmusic :: PU Music
pmusic
  = startPt $
    xpWrap (forward,backward) $                              -- Convert [MXMeasElm] to Music
    xpWrap (concat,undefined) $ xpList $ xpElem "measure" $  -- Convert MXMeasElm   to [MXMeasElm]
    xpList $ (xpickle :: PU MXMeasElm)                       -- Pickle
  where startPt a = xpElem "score-partwise" $                -- Select
                    keepElem "part"    $ xpElem "part"    $  -- Fitler, select
                    keepElem "measure" $ a                   -- Filter

instance XmlPickler MXMeasElm where
  xpickle = xpAlt tag ps
    where tag (MXNoteElm _) = 0
          tag (MXAttrElm _) = 1
          tag (MXMeasNum _) = 2
          ps = [ xpWrap (MXNoteElm, \(MXNoteElm n) -> n) pNote
               , xpWrap (MXAttrElm, \(MXAttrElm a) -> a) pAttr
               , xpWrap (MXMeasNum, \(MXMeasNum n) -> n) pMeasNum
               ]

pMeasNum :: PU Int
pMeasNum  = xpWrap (read, show) $ xpAttr "number" xpText

-- Measure Attributes
pAttr
  = xpElem "attributes" $ wAttr
    pdivs -- Divisions
    pkey  -- Key
    ptime -- Time
    pclef -- Clef
  where pdivs  = maySelNodeAndPickle "divisions"            -- Divisions per beat

        pkey   = xpOption $ xpElem "key" (wKey pfif pmode)  :: PU (Maybe MXKey)
        pfif   = selNodeAndPickle "fifths"                  -- Key Fifths     
        pmode  = selNodeAndPickle "mode"                    -- Key Mode       

        ptime  = xpOption $
                 xpElem "time" (wTime pbeats pbtyp pattr)   :: PU (Maybe MXTime)       
        pbeats = selNodeAndPickle "beats"                   -- Time Beats per measure
        pbtyp  = selNodeAndPickle "beat-type"               -- Time Beat Division
        pattr  = xpOption $ xpWrap (read, show) $           -- Time Annotation
                 xpAttr "symbol" xpText

        pclef  = xpOption $
                 xpElem "clef" (wClef pcsign pcline pcalt)   :: PU (Maybe MXClef)       
        pcsign = selNodeAndPickle "sign"                     -- Clef Sign   
        pcline = xpDefault def $ selNodeAndPickle "line"     -- Clef Line             -- BUG: Should use xpWrap so (NoneClef,0) to MXML doesn't include line # 0
        pcalt  = maySelNodeAndPickle "clef-octave-change"    -- Clef octave change

        -- Wrap functions. Converts tuple to type and vice versa
        wAttr a b c d = xpWrap (uncurry4 MXAttr, \(MXAttr divs key time clef) -> (divs,key,time,clef)) $ xp4Tuple a b c d
        wKey a b = xpWrap (uncurry MXKey, \ (MXKey fifths mode) -> (fifths, mode)) $ xpPair a b
        wTime a b c = xpWrap (uncurry3 MXTime, \(MXTime beats beatType anno) -> (beats, beatType, anno)) $ xpTriple a b c
        wClef a b c = xpWrap (uncurry3 MXClef, \(MXClef sign line octalt) -> (sign, line, octalt)) $ xpTriple a b c

        maySelNodeAndPickle a = xpOption $ selNodeAndPickle a

-- Measure Notes
pNote
  = xpElem "note" $ wNote
    ppitch -- Pitch
    pdur   -- Duration
    pvoice -- Voice
    ptype  -- Type
  where ppitch = xpElem "pitch" (wPitch pstep poct palter)   :: PU MXPitch      
        pstep  = selNodeAndPickle "step"                     -- Pitch Step       
        poct   = selNodeAndPickle "octave"                   -- Pitch Octave     
        palter = xpOption $ selNodeAndPickle "alter"         -- Ptich Maybe Alter    
        
        pdur   = selNodeAndPickle "duration"                 -- Duration
                 
        pvoice = selNodeAndPickle "voice"                    -- Voice

        ptype  = selNodeAndPickle "type"                     -- Note Type (eg quarter)    
          
        -- Wrap functions
        wNote a b c d = xpWrap (uncurry4 MXNote, \(MXNote pitch dur voice notetype) -> (pitch,dur,voice,notetype)) $ xp4Tuple a b c d  
        wPitch a b c = xpWrap (uncurry3 MXPitch, \(MXPitch step oct alt) -> (step, oct, alt)) $ xpTriple a b c

----------------------------------------------------------------------------------------------------                      
-- Helper functions
----------------------------------------------------------------------------------------------------

keepElems :: [String] -> PU a -> PU a
keepElems ls = let msum' = foldr (<+>) zeroArrow  -- (hasName "a") <+> (hasName "b") ... 
               in xpFilterCont $ msum' (map hasName ls)

keepElem :: String -> PU a -> PU a
keepElem x = xpFilterCont (hasName x)

instance (Default a, Eq a) => Default (PU a) where
  def = xpLift (def::a)

-- Selects one XML Node and pickles with xpPrim
selNodeAndPickle s = xpElem s xpPrim
        

