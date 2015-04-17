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
  d <- runX $ xunpickleDocument xpMusic            -- TODO: Return Music2 instead of [Music2]
    [withValidate no
    -- ,withTrace 2
    ,withRemoveWS yes
    ,withPreserveComment no] "demo-score.xml"             
    -- >>> arrIO (\x -> do {print x; return x})
  let d2 = postProcessing (head d)                     
  mapM print d2
  return ()
  where postProcessing = id -- fixPositions 

----------------------------------------------------------------------------------------------------                      
-- Post Processing
----------------------------------------------------------------------------------------------------
fixPositions :: [(Position, Note2)] -> Music2
fixPositions m = evalState (mapM f m) (0%1)
  where f (_, note) = do
          let duration = dur2 note
          position <- get             -- Get position for this note
          put (position + duration)   -- Put position for next note
          return (position, note)
  
----------------------------------------------------------------------------------------------------                      
-- Music Type Pickling
----------------------------------------------------------------------------------------------------
instance XmlPickler Music2   where  xpickle = xpMusic
instance XmlPickler Note2    where  xpickle = xpNote2
instance XmlPickler Position where  xpickle = xpPrim :: PU (Ratio Int)
-- instance XmlPickler MXStep where  xpickle = xpPrim

instance (Default a, Eq a) => Default (PU a) where
  def = xpLift (def::a)

xpMusic :: PU Music2
xpMusic
  = startPt $
    xpList $
    xpPair def xpickle 
  where startPt a = xpElem "score-partwise" $                -- Select
                    keepElem "part"    $ xpElem "part"    $  -- Fitler, select
                    keepElem "measure" $ xpElem "measure" $  -- Filter, select
                    keepElem "note" $ a                      -- Filter

xpNote2 :: PU Note2
xpNote2
  = xpElem "note" $                       -- Create list out of selected note elems
    keepElems ["pitch", "durationTest"] $          -- Ignore other elems
    xpWrap (uncurry3 Note2,                        -- 
            \t -> (dur2 t, pitch2 t, mods2 t)) $
    xpTriple def (xpElem "pitch" xpPitch) def

-- xpDuration :: (Int,Int) -> PU Duration
-- xpDuration
--   (divs,beat) = 
      
xpPitch :: PU Pitch
xpPitch = undefined
-- xpPitch = xpWrap (forward,backward) (xpTriple pstep poct palt)           
--   where pstep = (xpElem "step"   xpPrim)           :: PU MXStep
--         poct  = (xpElem "octave" xpickle)           :: PU Int
--         palt  = (xpOption $ xpElem "alter" xpickle) :: PU (Maybe Int)
    
----------------------------------------------------------------------------------------------------                      
-- Helper functions
----------------------------------------------------------------------------------------------------
keepElems :: [String] -> PU a -> PU a
keepElems ls = let msum' = foldr (<+>) zeroArrow  -- (hasName "a") <+> (hasName "b") ... 
               in xpFilterCont $ msum' (map hasName ls)

keepElem :: String -> PU a -> PU a
keepElem x = xpFilterCont (hasName x)

----------------------------------------------------------------------------------------------------                      
-- MusicXml Pickler functions
----------------------------------------------------------------------------------------------------
mxMeasure :: PU MXMeasure
mxMeasure
  = startPt $
    xpWrap (concat,undefined) $ xpList $ xpElem "measure" $
    xpList $ xpickle 
  where startPt a = xpElem "score-partwise" $                -- Select
                    keepElem "part"    $ xpElem "part"    $  -- Fitler, select
                    keepElem "measure" $ a                    -- Filter

instance XmlPickler MXMeasElm where
  xpickle = xpAlt tag ps
    where tag (MXAttrElm _) = 0
          tag (MXNoteElm _) = 1
          ps = [ xpWrap (MXAttrElm, \(MXAttrElm a) -> a) pAttr
               , xpWrap (MXNoteElm, \(MXNoteElm n) -> n) pNote
               ]

  
-- Selects one XML Node and pickles with xpPrim
selNodeAndPickle s = xpElem s xpPrim
                     
---- Measure Attributes
pAttr     = xpElem "attributes" $ wAttr
            pdivs -- Divisions
            pkey  -- Key
            ptime -- Time
            pclef -- Clef
    where pdivs  = selNodeAndPickle "divisions"         -- Divisions per beat

          pkey   = xpElem "key" (wKey pfif pmode)       :: PU MXKey
          pfif   = selNodeAndPickle "fifths"            -- Key Fifths     
          pmode  = selNodeAndPickle "mode"              -- Key Mode       

          ptime  = xpElem "time" (wTime pbeats pbtyp)    :: PU MXTime       
          pbeats = selNodeAndPickle "beats"             -- Time Beats per measure
          pbtyp  = selNodeAndPickle "beat-type"         -- Time Beat Division

          pclef  = xpElem "clef" (wClef pcsign pcline pcalt)   :: PU MXClef       
          pcsign = selNodeAndPickle "sign"              -- Clef Sign   
          pcline = selNodeAndPickle "line"              -- Clef Line
          pcalt  = xpOption $ selNodeAndPickle "clef-octave-change" -- Clef octave change

          -- Wrap functions. Converts tuple to type and vice versa
          wAttr a b c d = xpWrap (uncurry4 MXAttr, \(MXAttr divs key time clef) -> (divs,key,time,clef)) $ xp4Tuple a b c d
          wKey a b = xpWrap (uncurry MXKey, \ (MXKey fifths mode) -> (fifths, mode)) $ xpPair a b
          wTime a b = xpWrap (uncurry MXTime, \(MXTime beats beatType) -> (beats, beatType)) $ xpPair a b
          wClef a b c = xpWrap (uncurry3 MXClef, \(MXClef sign line octalt) -> (sign, line, octalt)) $ xpTriple a b c

---- Measure Notes
pNote     = xpElem "note" $ wNote
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

