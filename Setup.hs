import Haste
import Haste.Graphics.Canvas
import Haste.Perch
import Prelude hiding(id,div)
import Haste.HPlay.View
import Data.Ratio


----------------------------------------------------------------------------------------------------                      
-- Main code
----------------------------------------------------------------------------------------------------  
-- | Then you grab a canvas object...
-- main :: IO ()
main = do
  body <- getBody                         -- getBody :: IO Elem
  (flip build) body $                     -- build :: Elem -> Client Elem
     center $ canvas ! id "canvas" 
                     ! Haste.Perch.style "border: 1px solid black;" 
                     ! atr "width" "900" 
                     ! height "4000"
                     $ noHtml
  Just can <- getCanvasById "canvas"
  runBody mouse
  -- return ()
  -- render can $ do
  render can $ do 
    sequence_ [ translate ( 10 + x_ * 120, 10 + y_ * 80) $ do 
      staffShape (0,0) 100 
      qnShape (0, 20); qnShape (10, 0); qnShape (20, 25); qnShape (30, 0) ; qnShape (40, 40); qnShape (50, 20)  
      | x_ <-[0..5], y_ <-[0..50]]
  -- animate can 0 255
  
  -- show mouse events
mouse :: Widget ()
mouse= do
    wraw (div  ! Haste.Perch.style "height:100px;background-color:lightgreen;position:relative" $ h1 "Mouse events here")
                            `fire` OnMouseOut
                            `fire` OnMouseOver
                            `fire` OnMouseDown
                            `fire` OnMouseMove
                            `fire` OnMouseUp
                            `fire` OnClick
                            `fire` OnDblClick
                            `fire` OnKeyPress
                            `fire` OnKeyDown
                            `fire` OnKeyUp
    evdata <- getEventData
    wraw $ p << ( (evName evdata) ++" "++ show (evData evdata))

-- | A 40*40 square with a 20*20 square inside of it and a line running
--   through it.
squareShape :: Shape ()
squareShape = do
  rect (-20, -20) (20, 20)
  rect (-10, -10) (10, 10)
  line (-20, -20) (20, 20)

-- | You can stroke any shape to get a "wireframe" version of them.
square :: Picture ()
square = stroke squareShape

-- | Or you can fill them.
filledSquare :: Picture ()
filledSquare = fill squareShape



-- staffShape :: Shape ()
staffShape (x,y) dx = stroke $ do
  let dy = staffLineDy gSGS -- ScoreGraphicSettings
  Prelude.mapM_ (fn x dx) [y+ yy * dy | yy <- [0..4]]
  where fn = \x d y -> line (x, y) (x + d, y) -- (x1, y1) (x2, y2)
  
  -- Graphics Master Settings
data ScoreGraphicSettings = ScoreGraphicSettings
                            { staffLineDy :: Double  
                            , qnSize :: Double
                            , measureHeight :: Double
                     } deriving (Show)   

gSGS = ScoreGraphicSettings 10 5 40

-- qN
qnShape (x,y) = fill $ do circle (x,y) (qnSize gSGS); line (x-29,y-50) (x+20,y+20)

musDur n d = n % d :: Ratio Int -- % :: Integral a => a -> a -> Ratio a infixl 7

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
data Note  = Note Duration Pitch [NoteRestMod]  

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



--------------------------------------------------
-- Example             
-- qnE4Slur1: quarter note; pitch e4; slur 1/2 (2 note slur)
qnE4Slur1 :: Note
qnE4Slur1 = Note (musDur 1 4) 52 [(ModRel
                                   (0,      -- 'hashkey' of 0
                                    2,      -- Two notes total
                                    Slur))]             

-- qnG4Slur2: quarter note; pitch g4; slur 2/2 (2 note slur)
qnG4Slur2 :: Note
qnG4Slur2 = Note (musDur 1 4) 55 [(ModRel
                                   (0,      -- 'hashkey' of 0
                                    2,      -- Two notes total
                                    Slur))]                         

-- enF4: eigth note; pitch f4
enF4 :: Note
enF4 = Note (musDur 1 8) 53 []
trebClef :: GlobalMod
trebClef = ClefSym Treble
keyC :: GlobalMod
keyC = KeySym 0

musicTest :: Music       
musicTest = [(musDur 0 0, ModElm  keyC),
             (musDur 0 0, ModElm  trebClef),
             (musDur 0 0, NoteElm qnE4Slur1),
             (musDur 0 0, NoteElm enF4),
             (musDur 1 4, NoteElm qnG4Slur2),
             (musDur 2 4, NoteElm enF4),
             (musDur 3 4, NoteElm enF4)]

qnF4 :: Note -- Quarter note. F4
qnF4 =  Note (musDur 1 4) 53 []
        
-- Four successive F quarter notes. Assume treble clef & 4/4 time.
-- Measure will look like link below. Without treble clef and 4/4 timing.
-- http://stringstudies.com/wp-content/uploads/2013/09/5.gif        
musicSimple :: Music       
musicSimple = [(musDur 0 0, NoteElm qnF4),
               (musDur 1 4, NoteElm qnF4),
               (musDur 2 4, NoteElm qnF4),
               (musDur 3 4, NoteElm qnF4)]           


type DeltaDist = Double                 -- Dx,Dy, etc. Attempts to add clarity
data MeasureLocation = BottomOfStaff |  -- Treble clef looks aligned this way
                       NoteLocDy MeasureLocation DeltaDist  -- Dy is each note distance up staff


type Perc   = Float                       
type DimXY  = (Float, Float)
type PercXY = (Float, Float)     -- Percent (x,y)                
data Annotation = Annotation { dimensions :: DimXY,
                               placePoint :: PercXY,  -- (0,0) is upper left most. TODO: While whole note & quarter note base looks the same, the line upward from quaternote changes its dimensions. There are tradeoffs
                               measLoc    :: MeasureLocation,
                               bufferX    :: Perc    -- Buffers to next annotation. Percent of X dimension
                             }
              
-- TODO: Is best way of tieing constant dimensions with a global? E.g. staff measurements.
--         This could change in future, if staff measurements were to be dynamic.                   
noteAnno = Annotation (5,5) (0.5, 0.5) (NoteLocDy BottomOfStaff ((measureHeight gSGS) / 8)) 2

-- drawMusic mus staff = case mus of ((_, ModElm  m):xs) ->
                                    
--                                   ((d, NoteElm n):xs) -> result  
--                                   ((d, RestElm r):xs) -> result
--                                   (_) ->
--                                     where 
                      

----------------------------------------------------------------------------------------------------                      
-- Notes, ideas, etc. Might be out of date
----------------------------------------------------------------------------------------------------
--
-- Duration depends on time signature. Ex: 4/4 will be 64
-- musicScoreChunkify (MusElem elm scr) xss xs dur = xs
-- musicScoreChunkify End xss xs dur = xss -- TODO: fill in rest notes for remaining duration

-- TODO: Maybe have musicsequence be complete (in sense it can play regardless of knowing key, timing etc). Then have an annotation type Timing, Key, etc
-- What creating note/rests, etc is roughly going to look like
--------------------------------------------------------------
-- Music :: qqqq hqq eheee eeeeqq eReReeqq
-- Bite off measure at time
-- Measure :: qqqq
-- Return (picture, staff distance)?
--  * Figure out grouping
--  * Grouping :: g1qq (eeeeqq)
--  * Figure out how far off staff (may need line thu note, or above/below it)
--  * Figure out sharp/flat. May need to be before grouping
--  * Draw
