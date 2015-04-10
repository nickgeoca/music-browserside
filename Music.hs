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
type Music2 = [Note2]             

