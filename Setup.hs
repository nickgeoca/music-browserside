{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Haste
import Haste.Graphics.Canvas
import Haste.Perch
import Prelude hiding(id,div)
import Haste.HPlay.View
import GHC.Float
import Data.Ratio
-- import Music -- BUG

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
                     ! atr "width" "600" 
                     ! height "400"
                     $ noHtml
  Just can <- getCanvasById "canvas"
  -- return ()
  -- render can $ do
  render can $ do
    translate (20,20) $ do
    staffShape (0,0) 100
    sequence_ $ drawCanvas musicTest
  --  sequence_ [ translate ( 10 + x_ * 120, 10 + y_ * 80) $ do 
  --    staffShape (0,0) 100 
  --    qnShape (0, 20); qnShape (10, 0); qnShape (20, 25); qnShape (30, 0) ; qnShape (40, 40); qnShape (50, 20)  
--      | x_ <-[0..5], y_ <-[0..50]]


----------------------------------------------------------------------------------------------------                      
-- Old Graphics (still sort of used)
----------------------------------------------------------------------------------------------------  

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
-- Music Graphic Types
----------------------------------------------------------------------------------------------------

type DeltaDist = Double                 -- Dx,Dy, etc. Attempts to add clarity
data MeasureLocation = BottomOfStaff |  -- Treble clef looks aligned this way
                       NoteLocDy MeasureLocation DeltaDist  -- Dy is each note distance up staff
noteAnnoDy nAn = fn $ measLoc nAn
                where fn (NoteLocDy _ dy) = dy

type Perc   = Double                       
type DimXY  = (Double, Double)
type PercXY = (Double, Double)     -- Percent (x,y)                
data Annotation = Annotation { dimensions :: DimXY,
                               centerPoint:: PercXY,  -- (0,0) is upper left most. TODO: While whole note & quarter note base looks the same, the line upward from quaternote changes its dimensions. There are tradeoffs
                               measLoc    :: MeasureLocation,
                               annoDx  :: Double              -- Displacement to next annotation. (uses percent of X dimension to scale)
                             }

sAnnoDx a = State (\s -> ((), s {sXDisp = (sXDisp s) + (annoDispX + a)}))            


              
-- TODO: Is best way of tieing constant dimensions with a global? E.g. staff measurements.
--         This could change in future, if staff measurements were to be dynamic.
noteAnno = Annotation
           (5,5)
           (0.5, 0.5)
           (NoteLocDy BottomOfStaff ((measureHeight gSGS) / 8))
           (4 * (fst $ dimensions noteAnno))

clefAnno = Annotation
           (5,5)
           (0.5, 0.5)
           BottomOfStaff
           (4 * (fst $ dimensions clefAnno))   -- BUG: This depends on the state. Clefs are larger at beginning of measure, but smaller in middle of measure. Related to whether or not it is beginning of new line.
           
keyAnno = Annotation
          (5,5)
          (0.5, 0.5)
          BottomOfStaff
          (4 * (fst $ dimensions keyAnno))

timingAnno = Annotation
             (5,5)
             (0.5, 0.5)
             BottomOfStaff
             (4 * (fst $ dimensions timingAnno))

class ToPict a where
  toPic :: a -> Point -> Picture ()

instance ToPict NoteElm where
  toPic n (x,y)
    = let (num, denom) = (numerator $ dur n, denominator $ dur n)
      in case (num, denom) of
          (1,4) -> fill $ do circle (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)
          _     -> fill $ do circle (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)

instance ToPict ClefSym where
  toPic c (x,y) = fill $ do rect (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)

instance ToPict KeySym where
  toPic k (x,y) = fill $ do rect (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)

instance ToPict TimingSym where
  toPic t (x,y) = fill $ do rect (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)

clefDy c = 15   -- BUG
keyDy c = 15   -- BUG
timingDy c = 15   -- BUG

class Dy a where
  noteDy :: Clef -> a -> Double
instance Dy Note where
  noteDy clef note  = noteDy clef (pitch note)
instance Dy Pitch where
  noteDy (Clef sign line octaveAlt) pitch
    = let sn = case sign of
                   FClef    -> 2
                   GClef    -> 0
                   CClef    -> (-2)
                   TabClef  -> 0
                   PercClef -> 0
                   NoneClef -> 0
          ln = line * 2
          on = octaveAlt * 7

          d1 = 26 + ln + on + sn
          d2 = semiStepsToSteps pitch 0

          dCount = fromIntegral $ d2 - d1
          dy = noteAnnoDy noteAnno
      in dy * dCount
  

instance Num Point where
  (x1,y1) + (x2,y2) = (x1+x2, y1+y2)

drawCanvas :: Music -> [Picture ()]
drawCanvas m = drawCanvas_ m [] 0


data RendState = RendState 
  { sXDisp  :: Double
  , sClef   :: Clef
  , sKey    :: Key
  , sTiming :: Timing
  }
-- Key  keyfifths :: Int,       --  -11 to +11 (from circle of fifths)
--      keymode   :: Mode       -- Major, or minor



-- TODO: create monad function then map over it. Default state of clef is "none" clef                 
drawCanvas' m = evalState (f' []) 0
f' :: Music -> [Picture ()] -> State RendState [Picture ()]
f' ((pos, elm): mus) pics =
  do state <- get
     return $ f' mus  (ps ++ pics)
       where ps = case elm of 
              NoteElm n -> let c  = sClef state
                               x' = sXDisp state
                               y' = (measureHeight gSGS) - (noteDy c n)
                               ps'= [toPic n (x', y')]
                           in do sAnnoDx noteAnno
                                 ps'
              RestElm r -> undefined
              ModElm  m -> do ps' <- mapM modifiersToGraphics m
                              ps'

modifiersToGraphics :: ModElm -> State RendState (Picture ())
modifiersToGraphics l = do
  state <- get
  return $ case l of 
          ClefSym c   -> let x' = sXDisp state
                             y' = (measureHeight gSGS) - (clefDy c)
                             p  = toPic c (x', y')
                         in do put $ state {sClef = c}
                               sAnnoDx clefAnno
                               p
          KeySym k    -> let x' = sXDisp state
                             y' = (measureHeight gSGS) - (keyDy k)
                             p  = toPic k (x', y')
                         in do put $ state {sKey = k}
                               sAnnoDx keyAnno
                               p
          TimingSym t -> let x' = sXDisp state
                             y' = (measureHeight gSGS) - (timingDy t)
                             p  = toPic t (x', y')
                         in do put $ state {sTiming = t}
                               sAnnoDx timingAnno
                               p

drawCanvas_ :: Music -> [Picture ()] -> Double -> [Picture ()]              
drawCanvas_ []          pics _        = pics  
drawCanvas_ ((p,e):mus) pics xDispAcc
  = drawCanvas_ mus (pic:pics) (xDispAcc + xDispNew)
  where (pic, xDispNew) = case e of 
                           NoteElm n -> let coor1 = (0, (measureHeight gSGS) - (noteDy (Clef GClef 2 0) n)) 
                                            coor2 = coor1 + (xDispAcc, 0)
                                        in (notePic n coor2, (bufferX noteAnno))
                           -- RestElm r -> 2
                           -- ModElm  ms ->  
        
----------------------------------------------------------------------------------------------------                      
-- Examples
----------------------------------------------------------------------------------------------------  

musicTest :: Music       
musicTest = [ (0 % 1,ModElm [ ClefSym (Clef {clefsign = GClef, clefline = 2, clefoctalt = 0})
                            , KeySym (Main.Key {keyfifths = 0, keymode = Major})
                            , TimingSym (Timing 4 4 (Just TimeCommon))
                            ])
            , (0 % 1,NoteElm (Note {dur = 1 % 4, pitch = 53, mods = []}))
            , (0 % 1,NoteElm (Note {dur = 1 % 4, pitch = 55, mods = []}))              
            , (1 % 4,NoteElm (Note {dur = 1 % 4, pitch = 55, mods = []}))
            , (1 % 2,NoteElm (Note {dur = 1 % 4, pitch = 57, mods = []}))
            , (3 % 4,NoteElm (Note {dur = 1 % 4, pitch = 59, mods = []}))
            , (1 % 1,NoteElm (Note {dur = 1 % 4, pitch = 48, mods = []}))
            , (5 % 4,NoteElm (Note {dur = 1 % 4, pitch = 50, mods = []}))
            , (3 % 2,NoteElm (Note {dur = 1 % 4, pitch = 52, mods = []}))
            , (7 % 4,NoteElm (Note {dur = 1 % 4, pitch = 53, mods = []}))
            ]
            
                       
----------------------------------------------------------------------------------------------------                      
-- Notes, ideas, etc. Might be out of date
----------------------------------------------------------------------------------------------------






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
data Timing = Timing Int Int (Maybe TimeAnno)   -- Beats per measure, Beat division, ..
data TimeAnno = TimeCommon

-- Types: Layer 2
data GlobalMod
  = ClefSym Clef      |
    KeySym Key        |
    AnnoTime TimeAnno |
    TimingSym Timing  deriving (Show)

--------------------------------------------------
-- Top level music type
data MusElm = NoteElm  Note
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
    where fn (ClefSym _)   _ = GT
          fn (KeySym _)    _ = GT
          fn (TimingSym _) _ = GT
          fn _ _ = LT

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




{-
(0 % 1,ModElm [ AnnoTime common,
              , KeySym (Key {keyfifths = 0, keymode = Major}),
              , TimingSym (1 % 1),
              , ClefSym (Clef {clefsign = G, clefline = 2, clefoctalt = 0})])
(0 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(1 % 1,ModElm [ClefSym (Clef {clefsign = C, clefline = 3, clefoctalt = 0})])
(1 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(2 % 1,ModElm [ClefSym (Clef {clefsign = C, clefline = 4, clefoctalt = 0})])
(2 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(3 % 1,ModElm [ClefSym (Clef {clefsign = F, clefline = 4, clefoctalt = 0})])
(3 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(4 % 1,ModElm [ClefSym (Clef {clefsign = percussion, clefline = 0, clefoctalt = 0})])
(4 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(5 % 1,ModElm [ClefSym (Clef {clefsign = G, clefline = 2, clefoctalt = -1})])
(5 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(6 % 1,ModElm [ClefSym (Clef {clefsign = F, clefline = 4, clefoctalt = -1})])
(6 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(7 % 1,ModElm [ClefSym (Clef {clefsign = F, clefline = 3, clefoctalt = 0})])
(7 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(8 % 1,ModElm [ClefSym (Clef {clefsign = G, clefline = 1, clefoctalt = 0})])
(8 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(9 % 1,ModElm [ClefSym (Clef {clefsign = C, clefline = 5, clefoctalt = 0})])
(9 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(10 % 1,ModElm [ClefSym (Clef {clefsign = C, clefline = 2, clefoctalt = 0})])
(10 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(11 % 1,ModElm [ClefSym (Clef {clefsign = C, clefline = 1, clefoctalt = 0})])
(11 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(12 % 1,ModElm [ClefSym (Clef {clefsign = percussion, clefline = 0, clefoctalt = 0})])
(12 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(13 % 1,ModElm [ClefSym (Clef {clefsign = G, clefline = 2, clefoctalt = 1})])
(13 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(14 % 1,ModElm [ClefSym (Clef {clefsign = F, clefline = 4, clefoctalt = 1})])
(14 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(15 % 1,ModElm [ClefSym (Clef {clefsign = TAB, clefline = 5, clefoctalt = 0})])
(15 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(16 % 1,ModElm [ClefSym (Clef {clefsign = none, clefline = 0, clefoctalt = 0})])
(16 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
(17 % 1,ModElm [ClefSym (Clef {clefsign = G, clefline = 2, clefoctalt = 0})])
(17 % 1,NoteElm (Note {dur = 1 % 1, pitch = 48, mods = []}))
-}
            



gClefPic = [shamlet|
      <script type="text/javascript"> 
   
    
      function drawShape(){ 
        var canvas = document.getElementById('cssxCoordinates'); 
        var ctx = canvas.getContext('2d'); 
  	
        ctx.beginPath(); 
        ctx.moveTo(159,3); 
        ctx.quadraticCurveTo(129,50,117,93); 
        ctx.quadraticCurveTo(107,126,102,167); 
        ctx.quadraticCurveTo(101,192,102,210); 
        ctx.quadraticCurveTo(107,255,116,297); 
        ctx.quadraticCurveTo(63,351,44,375); 
        ctx.quadraticCurveTo(24,401,15,429); 
        ctx.quadraticCurveTo(2,464,3,503); 
        ctx.quadraticCurveTo(5,540,20,575); 
        ctx.quadraticCurveTo(29,596,48,615);  
        ctx.quadraticCurveTo(62,630,87,645);  
        ctx.quadraticCurveTo(113,660,150,666); 
        ctx.quadraticCurveTo(177,668,194,665);  
        ctx.quadraticCurveTo(204,720,213,776); 
        ctx.quadraticCurveTo(216,795,216,813); 
        ctx.quadraticCurveTo(203,849,158,857); 
        ctx.quadraticCurveTo(132,857,120,842); 
        ctx.quadraticCurveTo(152,845,166,813); 
        ctx.quadraticCurveTo(165,821,168,802);  
        ctx.quadraticCurveTo(166,775,151,765);  
        ctx.quadraticCurveTo(132,750,107,758); 
        ctx.quadraticCurveTo(86,768,78,789); 
        ctx.quadraticCurveTo(71,818,90,840); 
        ctx.quadraticCurveTo(105,857,129,865);  
        ctx.quadraticCurveTo(149,872,177,865);  
        ctx.quadraticCurveTo(194,860,209,846); 
        ctx.quadraticCurveTo(231,828,230,803); 
        ctx.quadraticCurveTo(221,735,207,662); 
        ctx.quadraticCurveTo(248,650,267,626); 
        ctx.quadraticCurveTo(293,599,296,566); 
        ctx.quadraticCurveTo(300,527,285,494); 
        ctx.quadraticCurveTo(270,462,234,444); 
        ctx.quadraticCurveTo(215,435,189,435); 
        ctx.quadraticCurveTo(177,435,164,438); 
        ctx.quadraticCurveTo(155,396,146,354); 
        ctx.quadraticCurveTo(183,315,203,275); 
        ctx.quadraticCurveTo(219,243,222,210); 
        ctx.quadraticCurveTo(227,167,221,137); 
        ctx.quadraticCurveTo(213,93,192,51); 
        ctx.quadraticCurveTo(180,29,159,3); 
               
        ctx.fill(); 
               
        ctx.fillStyle = "white"; 
        ctx.beginPath(); 
        ctx.moveTo(191,93); 
        ctx.quadraticCurveTo(179,83,171,93); 
        ctx.quadraticCurveTo(126,162,131,281); 
        ctx.quadraticCurveTo(188,239,203,188); 
        ctx.quadraticCurveTo(209,162,204,135); 
        ctx.quadraticCurveTo(200,111,191,93);	  
        ctx.fill(); 
               
        ctx.fillStyle = "white"; 
        ctx.beginPath(); 
        ctx.moveTo(171,473); 
        ctx.quadraticCurveTo(188,555,206,648);  
        ctx.quadraticCurveTo(237,639,255,620); 
        ctx.quadraticCurveTo(283,588,283,558); 
        ctx.quadraticCurveTo(285,525,269,501); 
        ctx.quadraticCurveTo(252,476,216,470); 
        ctx.quadraticCurveTo(194,465,171,473); 
               
        ctx.fill(); 
               
        ctx.fillStyle = "white"; 
        ctx.beginPath();	
        ctx.moveTo(147,446); 
        ctx.quadraticCurveTo(141,411,132,369); 
        ctx.quadraticCurveTo(90,401,68,435); 
        ctx.quadraticCurveTo(45,467,39,503); 
        ctx.quadraticCurveTo(30,540,45,576); 
        ctx.quadraticCurveTo(60,612,92,633); 
        ctx.quadraticCurveTo(123,651,161,654); 
        ctx.quadraticCurveTo(174,654,188,653); 
        ctx.fill(); 
               
        ctx.fillStyle = "black"; 
        ctx.beginPath(); 
        ctx.moveTo(147,444); 
        ctx.quadraticCurveTo(120,456,101,480); 
        ctx.quadraticCurveTo(83,504,84,536); 
        ctx.quadraticCurveTo(86,567,107,588); 
        ctx.quadraticCurveTo(114,597,126,605); 
        ctx.quadraticCurveTo(116,593,107,581); 
        ctx.quadraticCurveTo(95,560,99,537); 
        ctx.quadraticCurveTo(105,509,132,491); 
        ctx.quadraticCurveTo(143,482,164,476); 
               
        ctx.fill();       
      } 
      </script>
|]
