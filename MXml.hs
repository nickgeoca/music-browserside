{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MXml where
import Music
import Data.Ratio
import Control.Monad.State.Lazy
import Data.Maybe

-- BUG: Consider case of multi attribute in single measure. Example, mutliple clefs in a single measure.
--  

----------------------------------------------------------------------------------------------------                      
-- MusicXML Type
----------------------------------------------------------------------------------------------------
data MXStep = C | CD_ | D | DE_ | E | F | FG_ | G | GA_ | A | AB_ | B
              deriving (Show, Read, Enum)
type MXTimeAnno = TimeAnno
type MXClefSign = ClefSign
data MXMode = MXMinor | MXMajor deriving (Eq)
-------------------------
data MXKey = MXKey {
  mxfifths :: Int,          -- 
  mxmode   :: MXMode
  } deriving (Show, Read)

data MXTime = MXTime {
  mxbeats    :: Int,
  mxbeattype :: Int,
  mxtimeanno :: Maybe MXTimeAnno
  } deriving (Show, Read)

data MXClef = MXClef {
  mxclefsign   :: MXClefSign,  -- Clef sign
  mxclefline   :: Int,         -- Clef line
  mxclefoctalt :: Maybe Int
  } deriving (Show, Read)

-----------
data MXPitch = MXPitch {
  mxstep    :: MXStep,     -- Step (C,D,F,etc)
  mxoctave  :: Int,        -- Ocatve
  mxalter   :: Maybe Int   -- Pitch alter
  } deriving (Show, Read)

data MXNoteType = Wn | Hn | Qn | En | Sn | Tn | S64n | On deriving (Enum)                  

-------------------------
data MXAttr = MXAttr {
  mxdivs    :: Maybe Int,
  mxkey     :: Maybe MXKey,
  mxtime    :: Maybe MXTime,
  mxclef    :: Maybe MXClef  
  } deriving (Show, Read)

data MXNote = MXNote {
  mxpitch    :: MXPitch,
  mxduration :: Int,
  mxvoice    :: Int,
  mxnotetype :: MXNoteType
  } deriving (Show, Read)

-------------------------

data MXMeasElm = MXNoteElm MXNote |
                 MXAttrElm MXAttr |
                 MXMeasNum Int       -- Int is measure number: <measure number="1">
               deriving (Show, Read)

type MXMeasure = [MXMeasElm]
                 
-- TODO: Parsing
--        * Note pitch annotating needs either:
--          1) Note (absolute pitch, "accidental"); Modifier (key, clef)
--             * For double accidentals (double-sharp, flat-flat)
--          2) Note (absolute pitch); Modifier (key, clef)
--             * Sufficient without oddball double accidental cases
--          3) Note (absolute pitch); Modifier (key, clef, "accidental-prop")
  


data MusMXMeasState = MusMXMeasState {
  sMeasNum  :: Int,
  sDivs     :: Int,
  sBeats    :: Int,
  sBeatType :: Int,
  sPosition :: Duration
  }

instance ConvertBothWay Music MXMeasure where
  forward mxMeas = evalState (mapM f filteredMeasure) $ MusMXMeasState 1 0 0 0 (0%1) -- (MeasNum, divisions, beats, beat-type, position)
    where filteredMeasure = filter notMeasurePartNumber mxMeas
          notMeasurePartNumber (MXMeasNum _) = False
          notMeasurePartNumber _             = True

          f :: MXMeasElm -> State MusMXMeasState (Position, MusElm)
          f (MXNoteElm n) = do
            state <- get
            let divisionDur = mxduration n
                beatType    = sBeatType state
                divisions   = sDivs state
                noteDuration= divisionDur % (beatType * divisions)
                ptch        = forward $ mxpitch n
                note        = Note noteDuration ptch []
                oldPosition = sPosition state
                newPosition = oldPosition + noteDuration
            put $ state {sPosition = newPosition}
            return (oldPosition, NoteElm note)

          f (MXAttrElm a) = do
            let ms = [ (Just . AnnoTime  . forward) =<< mxtimeanno =<< (mxtime a)
                     , (Just . KeySym    . forward) =<< (mxkey a) 
                     , (Just . TimingSym . forward) =<< (mxtime a)
                     , (Just . ClefSym   . forward) =<< (mxclef a)
                     ]
                modsAnnos = sort . ModElm . catMaybes ms               -- NOTE: Sorting at data conversion is safer as long as it is done consistently. 
            st <- get
            put $ st { sDivs     = fromMaybe (sDivs st) (mxdivs a)
                     , sBeats    = maybe (sBeats st) mxbeats (mxtime a)
                     , sBeatType = maybe (sBeatType st) mxbeattype (mxtime a)
                     }
            return (sPosition st, modsAnnos)

  
  backward = undefined

instance ConvertBothWay TimeAnno TimeAnno where
  forward  = id
  backward = id
  
instance ConvertBothWay Key MXKey where
  forward  (MXKey f MXMinor) = Key f Minor
  forward  (MXKey f MXMajor) = Key f Major
  backward (Key  f Minor) = MXKey f MXMinor
  backward (Key  f Major) = MXKey f MXMajor
         
instance ConvertBothWay Clef MXClef where
  forward (MXClef sign line Nothing)       = forward (MXClef sign line (Just 0))
  forward (MXClef sign line (Just octalt)) = Clef sign line octalt
  backward (Clef sign line octalt) = MXClef sign line moctalt
    where moctalt = if octalt == 0 then Nothing else Just octalt

instance ConvertBothWay Timing MXTime where
  forward  (MXTime beat beattype _) = beat % beattype
  backward t                        = MXTime (numerator t) (denominator t) Nothing  -- BUG: Timing/MXTime

-- 58: (C,5,-2) = (B,4,-1) = (AB_,4,0) = (A,4,+1) = (GA_,4,+2)
-- 54: (G,4,-1) = (FG_,4,0) = (F,4,1)

-- DEPENDS on key, xml-accidental
instance ConvertBothWay Pitch MXPitch where
  forward (MXPitch s o (Just a))  = (12 * o) + (fromEnum s) + a
  forward (MXPitch s o Nothing) = forward $ MXPitch s o (Just 0)

  backward pitch = MXPitch step octave alter  -- NOTE: Must consider case of octave changing if going above/below C
    where step   = toEnum $ (pitch `mod` 12) + (if altTrue then (-1) else 0)
          octave = quot pitch 12
          alter  = if altTrue then Just 1 else Nothing
          altTrue= case (toEnum $ pitch `mod` 12) of
                    CD_ -> True
                    DE_ -> True
                    FG_ -> True
                    GA_ -> True
                    AB_ -> True
                    _   -> False


----------------------------------------------------------------------------------------------------
-- Read/Show Instances
instance Show MXNoteType where
  show a = case a of
    Wn -> "whole"
    Hn -> "half"
    Qn -> "quarter"
    En -> "eight"
    Sn -> "16th"
    Tn -> "32nd"
    S64n->"64th"
    On -> "128th"

instance Read MXNoteType where
  readsPrec _ a = case a of
    "whole"   -> [(Wn, "")]
    "half"    -> [(Hn, "")]
    "quarter" -> [(Qn, "")]
    "eight"   -> [(En, "")]
    "16th"    -> [(Sn, "")]
    "32nd"    -> [(Tn, "")]
    "64th"    -> [(S64n, "")]
    "128th"   -> [(On, "")]
    _         -> []

instance Show MXMode where
  show MXMajor = "major"
  show MXMinor = "minor"

instance Read MXMode where
  readsPrec _ "major" = [(MXMajor, "")]
  readsPrec _ "minor" = [(MXMinor, "")]
  readsPrec _ _       = []

                 
