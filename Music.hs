{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Music where
import Data.Ratio

----------------------------------------------------------------------------------------------------                      
-- Music Types
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
data MXStep = C | CD_ | D | DE_ | E | F | FG_ | G | GA_ | A | AB_ | B
              deriving (Show, Read, Enum)
type MXOctave   = Int
type MXAlter    = Int                  

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
    "half"    -> [(Hn, "half")]
    "quarter" -> [(Qn, "half")]
    "eight"   -> [(En, "half")]
    "16th"    -> [(Sn, "half")]
    "32nd"    -> [(Tn, "half")]
    "64th"    -> [(S64n, "half")]
    "128th"   -> [(On, "half")]
--     _         -> NilN
--   readsPrec _ _ = NilN

instance Show MXMode where
  show MXMajor = "major"
  show MXMinor = "minor"
instance Read MXMode where
  readsPrec _ "major" = [(MXMajor, "major")]
  readsPrec _ "minor" = [(MXMinor, "minor")]
  
type MXClefSign = Char
type MXClefLine = Int
type MXBeats = Int
type MXBeatType = Int               
data MXMode = MXMinor | MXMajor
type MXFifths = Int                   
-------------------------
type MXClef = (MXClefSign, MXClefLine)
type MXTime = (MXBeats, MXBeatType)
type MXKey = (MXFifths, MXMode)                      
type MXDivisions = Int
type MXAttr = (MXDivisions, MXKey, MXTime, MXClef)

type MXPitch    = (MXStep, MXOctave, Maybe MXAlter)
type MXDuration = Int
type MXVoice    = Int
data MXNoteType = Hn | Qn | En | Sn | Tn | S64n | On deriving (Enum)                  
type MXNote    = (MXPitch, MXDuration, MXVoice, MXNoteType)
-------------------------
data MXMeasElm = MXNoteElm MXNote | MXAttrElm MXAttr
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
    
-- 58: (C,5,-2) = (B,4,-1) = (AB_,4,0) = (A,4,+1) = (GA_,4,+2)
-- 54: (G,4,-1) = (FG_,4,0) = (F,4,1)

-- DEPENDS on key, xml-accidental
instance ConvertBothWay Pitch MXPitch where
  forward (s, o, Just a)  = (12 * o) + (fromEnum s) + a
  forward (s, o, Nothing) = forward (s,o,Just 0 :: Maybe Pitch)

  backward pitch = (step, octave, alter)  -- NOTE: Must consider case of octave changing if going above/below C
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

