{-# LANGUAGE TypeSynonymInstances, 
             MultiParamTypeClasses,
             FlexibleInstances,
             DeriveDataTypeable,
             ForeignFunctionInterface,
             GeneralizedNewtypeDeriving  #-}

import Haste
import Haste.Graphics.Canvas hiding (Ctx, Shape)
import Haste.Perch hiding (map, head)
import Prelude hiding(id,div)
import Haste.HPlay.View hiding (map, head)
import GHC.Float
import Data.Ratio
import Data.Typeable
import Control.Monad.IO.Class
import Haste.Foreign
import Haste.Prim (toJSStr)
import System.IO.Unsafe
import Unsafe.Coerce  
import Debug.Trace
import Data.List
import Data.Ord (comparing)
-- import Music -- BUG

foreign import ccall jsMoveTo :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsQuadraticCurveTo :: Ctx
                           -> Double -> Double
                           -> Double -> Double
                           -> IO ()

newtype Ctx = Ctx JSAny
            deriving (Pack, Unpack)
newtype Shape a = Shape {unS :: Ctx -> IO a}
(!>) = flip trace
infixr 0 !>             
  
----------------------------------------------------------------------------------------------------                      
-- Main code
----------------------------------------------------------------------------------------------------  
main = do
  addHeader jsHeader
  runBody $ do
  wraw $ do center $ canvas ! id "canvas" 
                     ! Haste.Perch.style "border: 1px solid black;" 
                     ! atr "width" "600" 
                     ! height "400"
                     $ noHtml
  
  center <<< wbutton "render" "render" 

  Just can <- liftIO $ getCanvasById "canvas" 

  pic <- drawCanvas musicTest
  render can $ do
    translate (20,20) $ do
    staffShape (0,0) 100
    sequence_ pic

-- jsHeader :: Perch
jsHeader = script ! atr "type" "text/javascript" ! src "https://rawgit.com/nickgeoca/js_misc/master/misc.js" $ noHtml
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

instance ToPict Note where
  toPic n (x,y)
    = let (num, denom) = (numerator $ dur n, denominator $ dur n)
      in case (num, denom) of
          (1,4) -> fill $ do circle (x,y) (fst $ dimensions noteAnno) -- ; line (x-29,y-50) (x+20,y+20)
          _     -> fill $ do circle (x,y) (fst $ dimensions noteAnno) -- ; line (x-29,y-50) (x+20,y+20)

instance ToPict Clef where
  toPic c (x,y) = fill $ do rect (x,y) (dimensions clefAnno)

instance ToPict Main.Key where
  toPic k (x,y) = fill $ do rect (x,y) (dimensions keyAnno)

instance ToPict Timing where
  toPic t (x,y) = fill $ do (unsafeCoerce quadraticCurve) (20,20) (40,20) (30,40) -- fill $ do rect (x,y) (dimensions timingAnno)

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

          dCount = fromIntegral $ d2 - d1     -- BUG: Also need to account for the key here.
          dy = noteAnnoDy noteAnno
      in dy * dCount
  

instance Num Point where
  (x1,y1) + (x2,y2) = (x1+x2, y1+y2)

-- undefined
-- TODO: Generate partially curried functions, before drawing.
-- Then calculate how many full measures in a line. Then more calculating..
-- then sequence_ functions with some values to fit pictures in the page width.
-- Retain sequence in event user zooms.
diffDurNoteOffsetDx = 2 * adjacentNoteOffsetDx
adjacentNoteOffsetDx = fst $ dimensions noteAnno
sUpdAnnoDx    a = do s <- getSData ; setSData $ s {sXDisp = (sXDisp s) + (annoDx a)}
-- sUpdPos       p = do s <- getSData ; setSData $ s {sPos   = (sPos   s) + p}              
-- Update dx following parallel notes of same duration.
-- sUpdParNoteDx   = do s <- getSData ; setSData $ s {sXDisp = (sXDisp s) + (fst $ dimensions noteAnno)}   -- TODO: Create global constant for update amount, outside of this function
data RendState = RendState 
  { sXDisp  :: Double
  , sClef   :: Clef
  , sKey    :: Key
  , sTiming :: Timing
--  , sPos    :: Position  
  } deriving (Typeable)


{-
noteRule1 ns = let n' = groupBy (\x y -> if dur x == dur y then True else False) n
                           fm = if length n' > 1
                                then do sUpdParNoteDx; return ()
                                else do return () 
                           do vss <- mapM (\ls -> do v <- mapM notesToGraphics ls; fm; return v) n'
                              sUpdAnnoDx noteAnno
                              return $ concat vss
-}                   
             

drawCanvas m = do setSData $ RendState 0 (Clef NoneClef 0 0) (Main.Key 0 Major) (Timing 4 4 Nothing)    -- TODO: create monad function then map over it. Default state of clef is "none" clef
                  drawCanvas' m []
-- CONTINUE
-- BUG: Note rendering rule is wrong. Diff duration should be on same dx if not adjacent. Exception being end of notel?               
drawCanvas' [] pics = return pics
drawCanvas' ((pos, elm): mus) pics =
  do state <- getSData :: View Perch IO RendState
     r <- case elm of 
           NoteElm ns -> do rs <- mapM notesToGraphics $ zip (cycle [0]) ns
                            sUpdAnnoDx noteAnno
                            return rs
           RestElm r  -> do return [] -- BUG: Update Rest Element in drawCanvas
           ModElm  m  -> do mapM modifiersToGraphics m
     drawCanvas' mus (pics ++ r)
               
notesAdjacent k n1 n2 = let fifths = keyfifths k
                            mode   = keymode k
                        in if (semiStepsToSteps (pitch n1) (pitch n2)) <= 1 
                           then True
                           else False

notesToGraphics (dx,n) = do
  state <- getSData
  let c  = sClef state
      x' = sXDisp state
      y' = (measureHeight gSGS) - (noteDy c n)
    in return $ toPic n (x' + dx, y')

-- modifiersToGraphics :: GlobalMod -> Picture ()
modifiersToGraphics e = do
  state <- getSData
  r <- case e of 
          ClefSym c   -> let x' = sXDisp state
                             y' = (measureHeight gSGS) - (clefDy c)
                             p  = toPic c (x', y')
                         in do setSData $ state {sClef = c}
                               sUpdAnnoDx clefAnno
                               return p
          KeySym k    -> let x' = sXDisp state
                             y' = (measureHeight gSGS) - (keyDy k)
                             p  = toPic k (x', y')
                         in do setSData $ state {sKey = k}
                               sUpdAnnoDx keyAnno
                               return p
          TimingSym t -> let x' = sXDisp state
                             y' = (measureHeight gSGS) - (timingDy t)
                             p  = toPic t (x', y')
                         in do setSData $ state {sTiming = t}
                               sUpdAnnoDx timingAnno
                               return p
  return r
                
        
----------------------------------------------------------------------------------------------------                      
-- Examples
----------------------------------------------------------------------------------------------------  

musicTest :: Music       
musicTest = [ (0 % 1,ModElm [ ClefSym (Clef {clefsign = GClef, clefline = 2, clefoctalt = 0})
                            , KeySym (Main.Key {keyfifths = 0, keymode = Major})
                            , TimingSym (Timing 4 4 (Just TimeCommon))
                            ])
            , (0 % 1,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})
                             , (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (1 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (1 % 2,NoteElm [ (Note {dur = 1 % 4, pitch = 57, mods = []})])
            , (3 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 59, mods = []})])
            , (1 % 1,NoteElm [ (Note {dur = 1 % 4, pitch = 48, mods = []})])
            , (5 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 50, mods = []})])
            , (3 % 2,NoteElm [ (Note {dur = 1 % 4, pitch = 52, mods = []})])
            , (7 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})])
            ]
            
                       
----------------------------------------------------------------------------------------------------                      
-- Notes, ideas, etc. Might be out of date
----------------------------------------------------------------------------------------------------


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

-- BUG: This must factor in the music key too
-- Converts semi steps to whole steps
semiStepsToSteps semis pitch =                 -- TODO: create optimized version. If semis above/below 12/-12, then doing extra work
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
      cnt   = getCount ds (abs steps) 0     -- TODO: Unoptimized if tones over/under 7/-7... Faster, but doesn't account for negatives:  (getCount ds (tones `mod` 7) (12*(quot tones 7)))
  in  neg2IfNeg1 cnt steps
  where getCount (l:ls) decr c = if decr == 0
                                 then c
                                 else getCount ls (decr - l) (c + 1)
        neg2IfNeg1 n1 n2 = if n2 < 0
                           then negate n1
                           else n1


-- Think about using a table similar to this one for step conversion.
-- [(12,7),(11,6),(10,5),(9,5),(8,4),(7,4),(6,3),(5,3),(4,2),(3,1),(2,1),(1,0),(0,0),(-1,0),(-2,-1),(-3,-1),(-4,-2),(-5,-3),(-6,-3),(-7,-4),(-8,-4),(-9,-5),(-10,-5),(-11,-6),(-12,-7)]        

quadraticCurve :: Point -> Point -> Point -> Shape ()
quadraticCurve (x1,y1) (x2,y2) (cpx,cpy) = Shape $ \ctx -> do
  jsMoveTo ctx x1 y1
  jsQuadraticCurveTo ctx cpx cpy x2 y2
                           
{-
gClefPic = ctx.beginPath(); 
ctx.moveTo(159,3); 
[(129,50,117,93),(107,126,102,167),(101,192,102,210),(107,255,116,297),(63,351,44,375),(24,401,15,429),(2,464,3,503),(5,540,20,575),(29,596,48,615),(62,630,87,645),(113,660,150,666),(177,668,194,665),(204,720,213,776),(216,795,216,813),(203,849,158,857),(132,857,120,842),(152,845,166,813),(165,821,168,802),(166,775,151,765),(132,750,107,758),(86,768,78,789),(71,818,90,840),(105,857,129,865),(149,872,177,865),(194,860,209,846),(231,828,230,803),(221,735,207,662),(248,650,267,626),(293,599,296,566),(300,527,285,494),(270,462,234,444),(215,435,189,435),(177,435,164,438),(155,396,146,354),(183,315,203,275),(219,243,222,210),(227,167,221,137),(213,93,192,51),(180,29,159,3)]
ctx.fill(); 
ctx.fillStyle = "white"; 

ctx.beginPath(); 
ctx.moveTo(191,93); 
[(179,83,171,93),(126,162,131,281),(188,239,203,188),(209,162,204,135),(200,111,191,93)] 
ctx.fill(); 
ctx.fillStyle = "white"; 

ctx.beginPath(); 
ctx.moveTo(171,473); 
[(188,555,206,648),(237,639,255,620),(283,588,283,558),(285,525,269,501),(252,476,216,470),(194,465,171,473)] 
ctx.fill(); 
ctx.fillStyle = "white"; 

ctx.beginPath();
ctx.moveTo(147,446); 
[(141,411,132,369),(90,401,68,435),(45,467,39,503),(30,540,45,576),(60,612,92,633),(123,651,161,654),(174,654,188,653)] 
ctx.fill(); 
ctx.fillStyle = "black"; 

ctx.beginPath(); 
ctx.moveTo(147,444); 
[(120,456,101,480),(83,504,84,536),(86,567,107,588),(114,597,126,605),(116,593,107,581),(95,560,99,537),(105,509,132,491),(143,482,164,476)] 
ctx.fill();       
-}
