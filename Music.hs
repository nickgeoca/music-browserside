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
data DynamicType   = Crescendo | Diminuendo deriving (Show)
data NoteRelatType = Slur | Arpeggio                       deriving (Show)       
-- Types: Layer 2
data Accents       = Staccato | Tenuto deriving (Show)       -- Staccato, Tenuto, etc
type Dynamic       = NoteTie DynamicType      -- Crescendo, Diminuendo, etc
type NoteRelat     = NoteTie NoteRelatType    -- Slur, Arpeggio, etc
-- Types: Layer 3
data NoteRestMod   = ModAcc Accents | ModDyn Dynamic | ModRel NoteRelat deriving (Show)

--------------------------------------------------
-- Notes/Rest                                                      
-- Types: Layer 1
type Duration   = Ratio Int
type Pitch      = Int            -- Absolute: 0-C0; 12-C1; 24-C2

-- Types: Layer 2
data Rest  = Rest {
  restdur::Duration,
  restmods :: [NoteRestMod]
  } deriving (Show)

data Note  = Note {
  dur::Duration,
  pitch::Pitch,
  mods::[NoteRestMod]
  } deriving (Show)

--------------------------------------------------
-- Global Modifiers/Annotations
-- Types: Layer 1                
data ClefSign = FClef | GClef | CClef | TabClef   
data Clef     = Clef {
                clefsign   :: ClefSign,  -- Clef sign
                clefline   :: Int,       -- Clef line
                clefoctalt :: Int        -- Octave alteration
                } deriving (Show)

data Mode = Minor | Major deriving (Show, Eq)
-------------------------
data Key = Key {
  fifths :: Int,          --  -11 to +11 (from circle of fifths)
  mode   :: Mode        -- Major, or minor
  } deriving (Show)
type Timing = Ratio Int   -- (Beats per measure % Beat division)

-- Types: Layer 2
data GlobalMod
  = ClefSym Clef |
    KeySym Key   |
    TimingSym Timing  deriving (Show)

--------------------------------------------------
-- Top level music type
data MusElm = NoteElm  Note
             | RestElm Rest
             | ModElm  [GlobalMod] deriving (Show)
type Position = Duration
type Music = [(Position, MusElm)]          -- TODO: The global modifiers/annotations probably don't need Position

----------------------------------------------------------------------------------------------------                      
-- MusicXML Type
----------------------------------------------------------------------------------------------------
data MXStep = C | CD_ | D | DE_ | E | F | FG_ | G | GA_ | A | AB_ | B
              deriving (Show, Read, Enum)

type MXClefSign = ClefSign
data MXMode = MXMinor | MXMajor deriving (Eq)
-------------------------
data MXKey = MXKey {
  mxfifths :: Int,          -- 
  mxmode   :: MXMode
  } deriving (Show, Read)

data MXTime = MXTime {
  mxbeats    :: Int,
  mxbeattype :: Int
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
                modifier = ModElm [KeySym (forward $ mxkey a),
                                   TimingSym (forward $ mxtime a),
                                   ClefSym (forward $ mxclef a)]
            put $ state {sDivs = divs, sBeats = beats, sBeatType = beattype}
            return (pos, modifier)
  backward = undefined

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
  forward  (MXTime beat beattype) = beat % beattype
  backward t                      = MXTime (numerator t) (denominator t)

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
  show FClef   = "F"
  show GClef   = "G"
  show CClef   = "C"
  show TabClef = "TAB"

instance Read MXClefSign where
  readsPrec _ "F"   = [(FClef, "")]
  readsPrec _ "G"   = [(GClef, "")]
  readsPrec _ "C"   = [(CClef, "")]
  readsPrec _ "TAB" = [(TabClef, "")]      
