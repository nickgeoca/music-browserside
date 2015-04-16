{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Music where
import Data.Ratio
import Control.Monad.State.Lazy

----------------------------------------------------------------------------------------------------                      
-- Music Type
----------------------------------------------------------------------------------------------------
-- Notes/Rest Modifiers/Annotations
-- Modifies notes through time. Might be useful for parallel notes, such as Arpeggio
type KeyH = Int          -- TODO: This should be a hash style value.                                                      
type NoteTie a = (KeyH,  -- Not key in music sense. Key when matching up to other notes. Must be unique across types.
                         -- Rough example, Crescendo of two notes: (a-note, key:0) (b-note) (c-note, key:0)
                         -- The 'a' and 'c' should increase in time, but not 'b'. Would be more useful for slurs.
                  Int,   -- Number of notes it effects. This value does not decrement in case a decremented
                         -- value is read before the initial value. e.g. notes are in parallel
                  a)     -- Can be any type
-- Types: Layer 1
data DynamicType   = Crescendo | Diminuendo
data NoteRelatType = Slur | Arpeggio                              
-- Types: Layer 2
data Accents       = Staccato | Tenuto        -- Staccato, Tenuto, etc
type Dynamic       = NoteTie DynamicType      -- Crescendo, Diminuendo, etc
type NoteRelat     = NoteTie NoteRelatType    -- Slur, Arpeggio, etc
-- Types: Layer 3
data NoteRestMod   = ModAcc Accents | ModDyn Dynamic | ModRel NoteRelat

--------------------------------------------------
-- Notes/Rest                                                      
-- Types: Layer 1
type Duration   = Ratio Int
type Pitch      = Int            -- Absolute: C0=~16.35Hz,0; C1,12; C2,24
-- Types: Layer 2
data Rest  = Rest Duration       [NoteRestMod]  
data Note  = Note {dur::Duration, pitch::Pitch, mods::[NoteRestMod]}  

--------------------------------------------------
-- Global Modifiers/Annotations
-- Types: Layer 1                
data Clef     = Treble | Bass | Alto | Tenor                    
type Key      = Int               -- ???? Major Keys: C=0; G=1; D=2... clockwise order
-- Types: Layer 2
data GlobalMod= ClefSym Clef | KeySym Key     -- Clef, Music key, etc

--------------------------------------------------
-- Top level music type
data MusElm = NoteElm  Note
             | RestElm Rest
             | ModElm  GlobalMod
type Position = Duration
type Music = [(Position, MusElm)]          -- TODO: The global modifiers/annotations probably don't need Position

data Note2  = Note2 {dur2::Duration, pitch2::Pitch, mods2::String}  deriving (Show)
type Music2 = [(Position, Note2)]             

----------------------------------------------------------------------------------------------------                      
-- MusicXML Type
----------------------------------------------------------------------------------------------------
data MXStep = C | CD_ | D | DE_ | E | F | FG_ | G | GA_ | A | AB_ | B
              deriving (Show, Read, Enum)

data MXClefSign = Gclef | Fclef
data MXMode = MXMinor | MXMajor deriving (Eq)
-------------------------
data MXClef = MXClef {
  mxclefsign :: MXClefSign,  -- Clef sign
  mxclefline :: Int          -- Clef line
  } deriving (Show, Read)

data MXTime = MXTime {
  mxbeats    :: Int,
  mxbeattype :: Int
  } deriving (Show, Read)

data MXKey = MXKey {
  mxfifths :: Int,
  mxmode   :: MXMode
  } deriving (Show, Read)

-----------
data MXPitch = MXPitch {
  mxstep    :: MXStep,     -- Step (C,D,F,etc)
  mxoctave  :: Int,        -- Ocatve
  mxalter   :: Maybe Int   -- Pitch alter
  } deriving (Show, Read)

data MXNoteType = Hn | Qn | En | Sn | Tn | S64n | On deriving (Enum)                  

-------------------------
data MXAttr = MXAttr {
  mxdivs :: Int,
  mxkey  :: MXKey,
  mxtime :: MXTime,
  mxclef :: MXClef  
  } deriving (Show, Read)

data MXNote = MXNote {
  mxpitch    :: MXPitch,
  mxduration :: Int,
  mxvoice    :: Int,
  mxnotetype :: MXNoteType
  } deriving (Show, Read)

-------------------------

data MXMeasElm = MXNoteElm MXNote | MXAttrElm MXAttr deriving (Show, Read)
type MXMeasure = [MXMeasElm]
                 
-- TODO: Parsing
--        * Note pitch annotating needs either:
--          1) Note (absolute pitch, "accidental"); Modifier (key, clef)
--             * For double accidentals (double-sharp, flat-flat)
--          2) Note (absolute pitch); Modifier (key, clef)
--             * Sufficient without oddball double accidental cases
--          3) Note (absolute pitch); Modifier (key, clef, "accidental-prop")

--------------------------------------------------                 
class ConvertBothWay a b where
  forward  :: b -> a
  backward :: a -> b 
    
data MusMXMeasState = MusMXMeasState {
  sDivs::Int,
  sBeats::Int,
  sBeatType::Int,
  sPosition::Duration
  }

instance ConvertBothWay Music MXMeasure where
  forward mxMeas = evalState (mapM f mxMeas) $ MusMXMeasState 0 0 0 (0%1)
    where f :: MXMeasElm -> State MusMXMeasState (Position, MusElm)
          f (MXNoteElm n) = do
            state <- get
            let divisionDur = mxduration n
                beatType    = sBeatType state
                divisions   = sDivs state
                noteDuration= divisionDur % (beatType * divisions)
                oldPosition = sPosition state
                newPosition = oldPosition + noteDuration
                ptch        = forward $ mxpitch n
                note        = Note noteDuration ptch []
            put $ state {sPosition = newPosition}
            return (oldPosition, NoteElm note)
          f (MXAttrElm a) = do
            state <- get
            let pos      = sPosition state
                divs     = mxdivs a
                beats    = mxbeats    $ mxtime a
                beattype = mxbeattype $ mxtime a
            put $ state {sDivs = divs, sBeats = beats, sBeatType = beattype}
            return (pos, undefined a) -- BUG!!!
  backward = undefined

-- instance ConvertBothWay MusElm MXAttr where
--  forward = 
         
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
    Hn -> "half"
    Qn -> "quarter"
    En -> "eight"
    Sn -> "16th"
    Tn -> "32nd"
    S64n->"64th"
    On -> "128th"
instance Read MXNoteType where
  readsPrec _ a = case a of
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

instance Show MXClefSign where
  show Gclef = "G"
  show Fclef = "F"

instance Read MXClefSign where
  readsPrec _ "G" = [(Gclef, "")]
  readsPrec _ "F" = [(Fclef, "")]  
