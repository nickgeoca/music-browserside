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
  d <- runX $ xunpickleDocument xpMusic33            -- TODO: Return Music2 instead of [Music2]
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
xpPitch = xpWrap (forward,backward) (xpTriple pstep poct palt)           
  where pstep = (xpElem "step"   xpPrim)           :: PU MXStep
        poct  = (xpElem "octave" xpickle)           :: PU MXOctave
        palt  = (xpOption $ xpElem "alter" xpickle) :: PU (Maybe MXAlter)
    
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
xpMusic33 :: PU MXMeasure
xpMusic33
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


-- instance Default MXAttr where
--   def = (0,(0,MXMajor),(0,0),('G',0))

-- Selects one XML Node and pickles with xpPrim
selNodeAndPickle s = xpElem s xpPrim
                     
---- Measure Attributes
pAttr     = xpElem "attributes" $ xp4Tuple
            pdivs -- Divisions
            pkey  -- Key
            ptime -- Time
            pclef -- Clef
    where pdivs  = selNodeAndPickle "divisions"         :: PU MXDivisions
          
          pkey   = xpElem "key" (xpPair pfif pmode)     :: PU MXKey        
          pfif   = selNodeAndPickle "fifths"            :: PU MXFifths     
          pmode  = selNodeAndPickle "mode"              :: PU MXMode       
          
          ptime  = xpElem "time" (xpPair pbeats pbtyp)  :: PU MXTime       
          pbeats = selNodeAndPickle "beats"             :: PU MXBeats
          pbtyp  = selNodeAndPickle "beat-type"         :: PU MXBeatType   

          pclef  = xpElem "clef" (xpPair pcsign pcline) :: PU MXClef       
          pcsign = selNodeAndPickle "sign"              :: PU MXClefSign   
          pcline = selNodeAndPickle "line"              :: PU MXClefLine   

---- Measure Notes
pNote     = xpElem "note" $ xp4Tuple
            ppitch -- Pitch
            pdur   -- Duration
            pvoice -- Voice
            ptype  -- Type
    where ppitch = xpElem "pitch" (xpTriple pstep poct palter) :: PU MXPitch      
          pstep  = selNodeAndPickle "step"                     :: PU MXStep       
          poct   = selNodeAndPickle "octave"                   :: PU MXOctave     
          palter = xpOption $ selNodeAndPickle "alter"         :: PU (Maybe MXAlter)    

          pdur   = selNodeAndPickle "duration"                 :: PU MXDuration   
            
          pvoice = selNodeAndPickle "voice"                    :: PU MXVoice      

          ptype  = selNodeAndPickle "type"                     :: PU MXNoteType   
                      
----------------------------------------------------------------------------------------------------                      
-- Example, figuring stuff out
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Made up time parsing. Wanted to use state monad again.
data TymeElmX= XTHour Int    | XTMeri Int     deriving (Show, Read)
data TymeElm = THour Int Int | TMeri Int      deriving (Show, Read)              
type TymeXml = [TymeElmX]
type Tyme    = [TymeElm]

xpTopLvlTyme = xpElem "time" $ xpTyme 

xpTyme :: PU Tyme
xpTyme = xpWrap (forward,backward)  $
         xpList $
         xpTymeX

xpTymeX :: PU TymeElmX
xpTymeX
  = xpAlt tag ps
    where tag (XTMeri _) = 0
          tag (XTHour _) = 1
          ps = [ xpWrap (XTMeri,
                         \ (XTMeri i) -> i
                        ) $
                 xpElem "meridiem" xpickle
               , xpWrap (XTHour,
                         \ (XTHour h) -> h
                        ) $
                 xpElem "hour" xpickle
               ]

main2 = do
  d <- runX $ xunpickleDocument xpTopLvlTyme
       [withValidate no
       ,withRemoveWS yes
       ,withPreserveComment no] "test.xml"
  mapM print $ head d
  return ()

instance ConvertBothWay Tyme TymeXml where
  forward x = evalState (mapM f x) 0
    where f (XTHour h) = do
            m <- get
            return (THour (h+m) m)
          f (XTMeri m) = do
            put m
            return (TMeri m)

  backward _ = []    
