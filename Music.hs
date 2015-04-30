{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Music where
import Data.Ratio
import Control.Monad.State.Lazy
import Data.Maybe

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
data NoteRelatType = Slur | Arpeggio        deriving (Show)       
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
data ClefSign = FClef | GClef | CClef | TabClef | PercClef | NoneClef 
data Clef     = Clef {
                clefsign   :: ClefSign,  -- Clef sign
                clefline   :: Int,       -- Clef line
                clefoctalt :: Int        -- Octave alteration
                } deriving (Show)

data Mode = Minor | Major deriving (Show, Eq)
-------------------------
data Key = Key {
  keyfifths :: Int,       --  -11 to +11 (from circle of fifths)
  keymode   :: Mode       -- Major, or minor
  } deriving (Show)
data Timing = Timing Int Int (Maybe TimeAnno) deriving (Show)  -- Beats per measure, Beat division, ..
data TimeAnno = TimeCommon

-- Types: Layer 2
data GlobalMod
  = ClefSym Clef      |
    KeySym Key        |
    AnnoTime TimeAnno |
    TimingSym Timing  deriving (Show)

--------------------------------------------------
-- Top level music type
-- TODO: Assume certain properties, or run function to verify in all cases? Such as sort parallel notes by duration. Then group, then sort by length of group
-- TODO: So far NoteElm is sorted by duration (highest count of group first), then pitch (highest first?)
-- TODO: This might be a pain in the ass. A presorter must be called during editing of music. If key changes, then the pitch sorting crap changes.
-- TODO: Easier to have lined notes on right, unlined on left. Actually, easier way is not to sort, but to group touchign notes, then figure out who goes left/right.
data MusElm = NoteElm  [Note]
             | RestElm Rest
             | ModElm  [GlobalMod] deriving (Show)
type Position = Duration
type Music = [(Position, MusElm)]          -- TODO: The global modifiers/annotations probably don't need Position


--------------------------------------------------                 
class ConvertBothWay a b where
  forward  :: b -> a
  backward :: a -> b 
    
instance Ord GlobalMod where
  compare a b = if a == b
                then EQ
                else fn a b
    where fn (TimingSym _)           _  = GT   -- Greatest
          fn (KeySym _)   (TimingSym _) = LT
          fn (ClefSym _)             _  = LT   -- Least
          fn _                       _  = GT

instance Eq GlobalMod where
  (==) (ClefSym _)   (ClefSym _)   = True
  (==) (KeySym _)    (KeySym _)    = True
  (==) (TimingSym _) (TimingSym _) = True
  (==) _ _ = False
  

instance Show TimeAnno where
  show TimeCommon = "common"

instance Read TimeAnno where
  readsPrec _ "common" = [(TimeCommon, "")]
                  

-- MusicXml uses these show/read functions
instance Show ClefSign where
  show FClef    = "F"
  show GClef    = "G"
  show CClef    = "C"
  show TabClef  = "TAB"
  show PercClef = "percussion"
  show NoneClef = "none"

instance Read ClefSign where
  readsPrec _ "F"          = [(FClef, "")]
  readsPrec _ "G"          = [(GClef, "")]
  readsPrec _ "C"          = [(CClef, "")]
  readsPrec _ "TAB"        = [(TabClef, "")]
  readsPrec _ "percussion" = [(PercClef, "")]
  readsPrec _ "none"       = [(NoneClef, "")]

-- Converts semi steps to whole steps
semiStepsToSteps semis pitch =
  let ls = cycle ([1,0,1,0,1]++[1,0,1,0,1,0,1])
      note = pitch `mod` 12
      steps = sum $
              take (abs semis) $
              drop (note+1) ls
  in if semis < 0 then (negate steps) else steps

-- Converts wholestep count, to semi-step count
stepsToSemiSteps steps pitch =
  let note  = pitch `mod` 12
      ns    = cycle ([1, 0, 1, 0, 1] ++ [1, 0, 1, 0, 1, 0, 1])
      ds    = drop (note + 1) ns
      cnt   = getCount ds (abs steps) 0     -- Unoptimized if tones over/under 7/-7... Faster, but doesn't account for negatives:  (getCount ds (tones `mod` 7) (12*(quot tones 7)))
  in  neg2IfNeg1 cnt steps
  where getCount (l:ls) decr c = if decr == 0
                                 then c
                                 else getCount ls (decr - l) (c + 1)
        neg2IfNeg1 n1 n2 = if n2 < 0
                           then negate n1
                           else n1

-- Think about using a table similar to this one for step conversion.
{-
[(12,7),(11,6),(10,5),(9,5),(8,4),(7,4),(6,3),(5,3),(4,2),(3,1),(2,1),(1,0)
,(0,0)
,(-1,0),(-2,-1),(-3,-1),(-4,-2),(-5,-3),(-6,-3),(-7,-4),(-8,-4),(-9,-5),(-10,-5),(-11,-6),(-12,-7)
]
-}        
