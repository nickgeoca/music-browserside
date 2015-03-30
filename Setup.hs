-- Adapted from the canvas-simple from the Haste repository at:
-- https://github.com/valderman/haste-compiler/blob/master/examples
-- In this version the canvas tag has been created dinamically with the Perch library


import Haste
import Haste.Graphics.Canvas
import Haste.Perch
import Prelude hiding(id,div)
import Haste.HPlay.View
import Data.Ratio

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
data ScoreGraphicSettings = ScoreGraphicSettings { staffLineDy :: Double  
                     , qnSize :: Double
                     } deriving (Show)   

gSGS = ScoreGraphicSettings 10 5

-- qN
qnShape (x,y) = fill $ do circle (x,y) (qnSize gSGS); line (x-29,y-50) (x+20,y+20)

musDur n d = n % d :: Ratio Int -- % :: Integral a => a -> a -> Ratio a infixl 7


--------------------------------------------------
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
qnE4Slur1 = Note (musDur 1 4) 52 [(ModRel
                                   (0,      -- 'hashkey' of 0
                                    2,      -- Two notes total
                                    Slur))]             

-- qnG4Slur2: quarter note; pitch g4; slur 2/2 (2 note slur)
qnG4Slur2 = Note (musDur 1 4) 55 [(ModRel
                                   (0,      -- 'hashkey' of 0
                                    2,      -- Two notes total
                                    Slur))]                         

-- enF4: eigth note; pitch f4
enF4 = Note (musDur 1 8) 53 []
trebClef = ClefSym Treble
keyC = KeySym 0
       
musicTest = [(musDur 0 0, ModElm  keyC),
             (musDur 0 0, ModElm  trebClef),
             (musDur 0 0, NoteElm qnE4Slur1),
             (musDur 0 0, NoteElm enF4),
             (musDur 1 4, NoteElm qnG4Slur2),
             (musDur 2 4, NoteElm enF4),
             (musDur 3 4, NoteElm enF4)]


            
-- drawMusic mus staff = case mus of ((_, ModElm  m):xs) ->
                                    
--                                   ((d, NoteElm n):xs) -> result  
--                                   ((d, RestElm r):xs) -> result
--                                   (_) ->
--                                     where 
                      
            
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



----------------------------------------------------------------------------------------------------




-- data MusicSymbols  = NoteRestSym (NextSymbol, []) | ModifierSym []
-- type Music2       = [(NextSymbol, [])]
-- data MusicElem= ElemNote [Note] | ElemRest Rest
-- data MeasProps= MeasProps {measNum::Int, measTiming::Timing, measClef::Clef, measKey::MusKey}
-- type MusicSeq = [MusicElem]
-- type Measures = [MeasProps]
-- type Music    = (Measures, MusicSeq)                    

{-
type Timing   = (Int, Int)
type MusKey   = Int               -- Major Keys: C=0; G=1; D=2... clockwise order
data Clef     = Treble | Bass | Alto | Tenor
type Duration = Ratio Int         -- 
type Pitch    = Int               -- Absolute: C0=~16.35Hz,0; C1,12; C2,24
data Rest     = Rest Duration
data Note     = Note Duration Pitch
data MusicElem= ElemNote [Note] | ElemRest Rest
data MeasProps= MeasProps {measNum::Int, measTiming::Timing, measClef::Clef, measKey::MusKey}
type MusicSeq = [MusicElem]
type Measures = [MeasProps]
type Music    = (Measures, MusicSeq)

type Music2   = ()


qn = musDur 1 4
qnC4 = ElemNote [Note qn 48]
qnBb4 = ElemNote [Note qn 46]
musSeq = [qnC4, qnBb4, qnC4, qnC4]
musMeas = [MeasProps 1 (4,4) Treble 0] -- [qnC4, qnC4, qnC4, qnC4]
musicTest = (musMeas, musSeq)
-}

-- measure2 = MusicMeas (4,4) Treble 0 -- [qnBb4,qnBb4,qnBb4,qnBb4]
-- musicScoreTest = [measure1, measure2]

-- musicScoreTest = (MusElem (ElemNote [(Note (1,0) 0), (Note (2,0) 2)])
--                    (MusElem (ElemNote [(Note (1,0) (-3))]) 
--                      (MusElem (ElemRest (Rest (1,0)))
--                        (MusElem (ElemNote [(Note (1,0) (-5))])  
--                          (MusElem (ElemNote [(Note (1,0) (3))])  
--                            End))))) :: MusicScore
--getElement

             
             
{-
Cool! If it goes somewhere then we could split the company/profits, or if it doesn't then we can split the source code :)  Perhaps it is best to work with out too many expectations though!

Initially I thought it would be a good idea to do everything at once, but it is probably best to work towards a proof of concept. That would keep the project small and manageable, and if we wanted to continue the product "full time" it would be easier to get venture capital. It is also probably best to implement some of what distinguishes from jellynote- not to be competitive necessarily, but this is more practical (not reinventing the wheel & venture capitalist are more likely to fund since its a different product). It is all a good learning experience too.

So the objective is to do simple proof of concept and include some of what distinguishes from jellynote. If we could maybe do a few things:
 1) Upload a song to the site in MusicXML format
 2) Sever parses the song from MusicXML to the browser readable format
 3) Store it to database so it is searchable again (not sure which format is better, or both)
 4) Search for it
 5) Render it browser side when user goes to link
 6) Playback. When playing back, the notes get highlighted and there is midi sound and adjustable metronome. The thing to distinguish the site is the ability to sync the score with other band mates so they can see their notes. Meaning it plays back all at once for everyone on their own computers/devices. And pauses for everyone when someone pauses. 

If we can do these things on a very simple level, then the site would have a pretty good start I'd say. 

Here is an overview of what is going on.
 * The layout has been started, but it is still rough. I'll share the private repo with you. The server is using Yesod.
 * I just started on the hplayground-based browser code. Creating a "music" type is challenging, just because music doesn't always follow sensible rules (I'll explain this all later). I think I figured out a reasonably sufficient type for now.

It would be very helpful if you could help with browser rendering of the notes. So if you had the data/type for the music notes, you could go through and render it. I'll try to finish the music type today (friday) or tomorrow. There is a lot that goes into note rendering--such as horizontal note spacing--so a simple subset is good. The notes don't have to be the right shape or right spacing horizontally. Then from there we could 1) play the music, and it highlights notes as it goes; 2) play it for each person in a group trying to practice.

Once the music type is done, then I can try to parse simple MusicXML 

The last part of what I have to say is that I remember making a music app in the past once, and it was pretty cool. It was able show absolute pitch over time, so playing like a trumpet, it was intended to help out some how. It was useful because I realized I was confusing volume with pitch- not too important if it doesn't make sense. However, overall I found the app wasn't too helpful for me practicing. The lesson I learned (I found it important, but this might not be so helpful for you), is that technology doesn't always help out. So let us not put too much expectations on how far we can help other people :)

--         ...........................................................................

Cool! If it goes somewhere then we could split the company/profits, or if it doesn't then we can split the source code :)  Perhaps it is best to work with out too many expectations though!

Initially I thought it would be a good idea to do everything at once, but it is probably best to work towards a proof of concept. That would keep the project small and manageable, and if we wanted to continue the product "full time" it would be easier to get venture capital. It is also probably best to implement some of what distinguishes from jellynote- not to be competitive necessarily, but this is more practical (not reinventing the wheel & venture capitalist are more likely to fund since its a different product). It is all a good learning experience too.

So the objective is to do simple proof of concept and include some of what distinguishes from jellynote. If we could maybe do a few things:
 1) Upload a song to the site in MusicXML format
 2) Sever parses the song from MusicXML to the browser readable format
 3) Store it to database so it is searchable again (not sure which format is better, or both)
 4) Search for it
 5) Render it browser side when user goes to link
 6) Playback. When playing back, the notes get highlighted and there is midi sound and adjustable metronome. The thing to distinguish the site is the ability to sync the score with other band mates so they can see their notes. Meaning it plays back all at once for everyone on their own computers/devices. And pauses for everyone when someone pauses. 

If we can do these things on a very simple level, then the site would have a pretty good start I'd say. 

Here is an overview of what is going on.
 * The layout has been started, but it is still rough. I'll share the private repo with you. The server is using Yesod.
 * I just started on the hplayground-based browser code. Creating a "music" type is challenging, just because music doesn't always follow sensible rules (I'll explain this all later). I think I figured out a reasonably sufficient type for now.

It would be very helpful if you could help with browser rendering of the notes. So if you had the data/type for the music notes, you could go through and render it. I'll try to finish the music type today (friday) or tomorrow. There is a lot that goes into note rendering--such as horizontal note spacing--so a simple subset is good. The notes don't have to be the right shape or right spacing horizontally. Then from there we could 1) play the music, and it highlights notes as it goes; 2) play it for each person in a group trying to practice.

Once the music type is done, then I can try to parse simple MusicXML 

The last part of what I have to say is that I remember making a music app in the past once, and it was pretty cool. It was able show absolute pitch over time, so playing like a trumpet, it was intended to help out some how. It was useful because I realized I was confusing volume with pitch- not too important if it doesn't make sense. However, overall I found the app wasn't too helpful for me practicing. The lesson I learned (I found it important, but this might not be so helpful for you), is that technology doesn't always help out. So let us not put too much expectations on how far we can help other people :)
-}
