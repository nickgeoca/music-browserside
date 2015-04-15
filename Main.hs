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
--    ,withTrace 2
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

-- xpMusic :: PU Music
xpMusic33
  = startPt $
    xpList $
    (xpickle :: PU MXMeasElm)
  where startPt a = xpElem "score-partwise" $                -- Select
                    keepElem "part"    $ xpElem "part"    $  -- Fitler, select
                    keepElem "measure" $ xpElem "measure" $  -- Filter, select
                    a

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
  where pstep = (xpElem "step"   xpickle)           :: PU MXStep
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
instance XmlPickler MXMeasure where
  xpickle = xpList $ xpickle

instance XmlPickler MXMeasElm where
  xpickle = xpAlt tag ps
    where tag (MXAttrElm _) = 0
          tag (MXNoteElm _) = 1
          ps = [ (xp4Tuple xpickle xpickle xpickle xpickle) :: MXAttr PU
               , (xp4Tuple xpickle xpickle xpickle xpickle) :: MXNote PU
               ]

-- Selects one XML Node and pickles with xpPrim
selNodeAndPickle s = xpElem s xpPrim

---- Measure Attributes
-- Divisions
instance XmlPickler MXDivisions where xpickle = selNodeAndPickle "divisions"  
-- Key
instance XmlPickler MXKey       where xpickle = xpElem "key" (xpPair xpickle xpickle)
instance XmlPickler MXFifths    where xpickle = selNodeAndPickle "fifths" 
instance XmlPickler MXMode      where xpickle = selNodeAndPickle "mode"
-- Time
instance XmlPickler MXTime      where xpickle = xpElem "time" (xpPair xpickle xpickle)
instance XmlPickler MXBeats     where xpickle = selNodeAndPickle "beats"
instance XmlPickler MXBeatType  where xpickle = selNodeAndPickle "beat-type"
-- Clef
instance XmlPickler MXClef      where xpickle = xpElem "clef" (xpPair xpickle xpickle)
instance XmlPickler MXClefSign  where xpickle = selNodeAndPickle "sign"
instance XmlPickler MXClefLine  where xpickle = selNodeAndPickle "line"
---- Notes
-- Pitch                                      
instance XmlPickler MXPitch     where xpickle = xpElem "pitch" (xpTriple xpickle xpickle xpickle)
instance XmlPickler MXStep      where xpickle = selNodeAndPickle "step"
instance XmlPickler MXOctave    where xpickle = selNodeAndPickle "octave"
instance XmlPickler (Maybe MXAlter)   where xpickle = xpOption $ selNodeAndPickle "alter"
-- Duration
instance XmlPickler MXDuration  where xpickle = selNodeAndPickle "duration"
-- Voice
instance XmlPickler MXVoice     where xpickle = selNodeAndPickle "voice"
-- Type
instance XmlPickler MXNoteType  where xpickle = selNodeAndPickle "type"
                      
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
