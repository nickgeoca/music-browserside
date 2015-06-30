{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Web.Graphic ( Ctx , Shape ) where

import Haste.Foreign
import Haste.Graphics.Canvas hiding (Ctx, Shape)
import Types.Music
import Data.Ord(compare, comparing)

foreign import ccall jsClearRect :: Ctx -> Int -> Int -> Int -> Int -> IO ()
foreign import ccall jsMoveTo :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsQuadraticCurveTo :: Ctx -> Double -> Double -> Double -> Double -> IO ()

newtype Ctx = Ctx JSAny deriving (Pack, Unpack) -- TODO: Should be in library, then remove from files "hiding (Ctx, Shape)"
newtype Shape a = Shape {unS :: Ctx -> IO a}    -- TODO: Should be in library, then remove from files "hiding (Ctx, Shape)"

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

data Annotation = Annotation { 
  dimensions  :: DimXY,
  centerPoint :: PercXY,  -- (0,0) is upper left most. TODO: While whole note & quarter note base looks the same, the line upward from quaternote changes its dimensions. There are tradeoffs
  measLoc     :: MeasureLocation,
  annoDx      :: Double              -- Displacement to next annotation. (uses percent of X dimension to scale)
  }

-- TODO: Is best way of tieing constant dimensions with a global? E.g. staff measurements.
--         This could change in future, if staff measurements were to be dynamic.
noteAnno :: Annotation
noteAnno =  Annotation
           (5,5)
           (0.5, 0.5)
           (NoteLocDy BottomOfStaff ((measureHeight gSGS) / 8))
           (4 * (fst $ dimensions noteAnno))

clefAnno :: Annotation
clefAnno =  Annotation
           (5,5)
           (0.5, 0.5)
           BottomOfStaff
           (4 * (fst $ dimensions clefAnno))   -- BUG: This depends on the state. Clefs are larger at beginning of measure, but smaller in middle of measure. Related to whether or not it is beginning of new line.
           
keyAnno :: Annotation
keyAnno =  Annotation
          (5,5)
          (0.5, 0.5)
          BottomOfStaff
          (4 * (fst $ dimensions keyAnno))

timingAnno :: Annotation
timingAnno =  Annotation
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

instance ToPict Main.Key where  -- FIX: Main.Key => Music.Key
  toPic k (x,y) = fill $ do rect (x,y) (dimensions keyAnno)

instance ToPict Timing where
  toPic t (x,y) = fill $ do rect (x,y) (dimensions timingAnno)
{-
  toPic t (x,y) = let x' = (fst $ dimensions timingAnno) 
                      y' = (snd $ dimensions timingAnno) 
                  in fill $ do (unsafeCoerce quadraticCurve) (x,y) (x',y') (x,y') 
-}

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
{-
sUpdAnnoDx    a = do s <- getSData 
                     setSData $ s {sXPos = (sXPos s) + (annoDx a)}
-}
sUpdAnnoDx :: Annotation -> Widget HgltBorder
sUpdAnnoDx a = do s <- getSData
                  let x  = sXPos s
                      dx = annoDx a
                      y  = sYPos s
                  -- Update state with new x/y positions
                  ((x1', x2'), y') <- typeWriterFn x dx y
                  setSData $ s { sXPos = x2'
                               , sYPos = y'
                               }
                  -- Return coordinates for highlighting box
                  return $ BoxCoor (round x1') (round y') (round x2') (round (y' + measureHeight gSGS))

-- sUpdPos       p = do s <- getSData ; setSData $ s {sPos   = (sPos   s) + p}              
-- Update dx following parallel notes of same duration.
-- sUpdParNoteDx   = do s <- getSData ; setSData $ s {sXPos = (sXPos s) + (fst $ dimensions noteAnno)}   -- TODO: Create global constant for update amount, outside of this function
data RendState = RendState { 
  sXPos  :: Double,
  sYPos  :: Double,
  sClef   :: Clef,
  sKey    :: Key,
  sTiming :: Timing
--  , sPos    :: Position  
  } deriving (Typeable)


{-
noteRule1 ns = let n' = groupBy (\x y -> if dur x == dur y then True else False) n
                           fm = if length n' > 1
                                then do sUpdParNoteDx; return ()
                                else do return () 
                           do vss <- mapM (\ls -> do v <- mapM notesToGraphics ls; fm; return v) n'
                              sUpdAnnoDx noteAnno
                              return $ join vss
-}                   
             
drawCanvas :: Music -> (Double, Double) -> Widget [ScoreRenderElm]   
drawCanvas m (x,y) = do setSData $ RendState x y (Clef NoneClef 0 0) (Main.Key 0 Major) (Timing 4 4 Nothing)   -- TODO: create monad function then map over it. Default state of clef is "none" clef
                        drawCanvas' m

-- BUG: Note rendering rule is wrong. Diff duration should be on same dx if not adjacent. Exception being end of note?
drawCanvas' :: Music -> Widget [ScoreRenderElm]                  
drawCanvas' [] = return []
drawCanvas' ((pos, elm): mus) =
  do state <- (getSData :: Widget RendState) 
     r <- case elm of  
           NoteElm ns -> do pics <- mapM notesToGraphics $ zip (cycle [0]) ns
                            let midinotes = map forward ns
                                midiPics  = zip midinotes pics
                            hglt <- sUpdAnnoDx noteAnno  -- NOTE: This should be last line of monad code! (beside return)
                            return $ [ScoreRendNotes hglt pos midiPics]
           RestElm r  -> do return []    -- BUG: Update Rest Element in drawCanvas'
           ModElm  m  -> do mapM modifiersToGraphics m
     liftM2 (++) (return r) (drawCanvas' mus)

instance ConvertBothWay MidiNote Note where
  forward note = MidiNote (dur note) (pitch note)

notesAdjacent k n1 n2 = let fifths = keyfifths k
                            mode   = keymode k
                        in if (semiStepsToSteps (pitch n1) (pitch n2)) <= 1 
                           then True
                           else False

notesToGraphics (dx,n) = do
  state <- getSData
  let c  = sClef state
      x' = sXPos state
      y' = (sYPos state) - (noteDy c n) -- BUG: most definetly. 
    in return $ toPic n (x' + dx, y')

-- modifiersToGraphics :: GlobalMod -> Picture ()
modifiersToGraphics e = do
  state <- getSData
  r <- case e of 
          ClefSym c   -> let x = sXPos state
                             y = sYPos state - clefDy c
                             p = toPic c (x, y)
                         in do setSData $ state {sClef = c}
                               sUpdAnnoDx clefAnno
                               return p
          KeySym k    -> let x = sXPos state
                             y = (measureHeight gSGS) - (keyDy k)
                             p = toPic k (x, y)
                         in do setSData $ state {sKey = k}
                               sUpdAnnoDx keyAnno
                               return p
          TimingSym t -> let x = sXPos state
                             y = (measureHeight gSGS) - (timingDy t)
                             p = toPic t (x, y)
                         in do setSData $ state {sTiming = t}
                               sUpdAnnoDx timingAnno
                               return p
  return $ ScoreRendMod r
                

typeWriterFn :: Double -> Double -> Double -> Widget ((Double, Double), Double)
typeWriterFn x dx y = do -- canvasWidth <- liftM sgsCanvasWidth (getSData :: Widget SGSettingsDynamic) -- BUG
                         canvasWidth <- liftM (\x->x) (return 500)
                         let dy   = measureHeight gSGS
                             nwln = x + dx > canvasWidth
                             -- 
                             y'   = if nwln then y + dy else y
                             x1'  = if nwln then 0      else x
                             x2'  = x1' + dx
                         return ((x1', x2'), y')

quadraticCurve :: Point -> Point -> Point -> Shape ()
quadraticCurve (x1,y1) (x2,y2) (cpx,cpy) = Shape $ \ctx -> do
  jsMoveTo ctx x1 y1
  jsQuadraticCurveTo ctx cpx cpy x2 y2


data Canvas' = Canvas' !Ctx !Elem
clearRect :: Canvas -> Int -> Int -> Int -> Int -> IO ()  -- TODO: Remove unsafe coerce
clearRect c x y w h = let (Canvas' ctx _) = unsafeCoerce c :: Canvas'
                      in jsClearRect ctx x y w h


----------------------------------------------------------------------------------------------------                      
-- Random types. Better organize
----------------------------------------------------------------------------------------------------  
data ScoreRenderElm = ScoreRendNotes HgltBorder Position    [(MidiNote, Picture ())] |
                      ScoreRendRest  HgltBorder Position     (Picture ())            |
                      ScoreRendMod                           (Picture ())

-- NOTE: Double vs Int, speed/canvas-errors
-- NOTE: Watch int errors here
data BoxCoor = BoxCoor {
  x1Coor :: Int, y1Coor :: Int,
  x2Coor :: Int, y2Coor :: Int
  } deriving (Show,Read)

type HgltBorder = BoxCoor

data MeasurePos = MeasurePos 
 { measNum :: Int
 , xPixPos :: Int  
 }

instance Eq MeasurePos where
  (==) mp1 mp2 = let cmp1 = comparing measNum mp1 mp2
                     cmp2 = comparing xPixPos mp1 mp2
                 in cmp1 == EQ && cmp2 == EQ

instance Ord MeasurePos where
  compare mp1 mp2 = let cmp1 = comparing measNum mp1 mp2
                        cmp2 = comparing xPixPos mp1 mp2
                    in if cmp1 == EQ 
                       then use cmp2
                       else use cmp1
    where use = (\x->x) -- id
  (<=) mp1 mp2 = let cmp1 = comparing measNum mp1 mp2
                     cmp2 = comparing xPixPos mp1 mp2
                 in if cmp1 == LT || (cmp1 == EQ && cmp2 == EQ)
                       then True
                       else False

instance Functor (ListTree MeasurePos) where fmap f lt = fmapK2 f lt


----------------------------------------------------------------------------------------------------                      
-- Old Graphics (still sort of used)
----------------------------------------------------------------------------------------------------  

-- staffShape :: Shape ()
staffShape (x,y) dx = stroke $ do
  let dy = staffLineDy gSGS -- ScoreGraphicSettings
  Prelude.mapM_ (fn x dx) [y+ yy * dy | yy <- [0..4]]
  where fn = \x d y -> line (x, y) (x + d, y) -- (x1, y1) (x2, y2)
  
data SGSettingsDynamic = SGSettingsDynamic {
  sgsCanvasWidth :: Double
  } deriving (Show)

-- Graphics Master Settings
data ScoreGraphicSettings = ScoreGraphicSettings
                            { staffLineDy :: Double  
                            , qnSize :: Double
                            , measureHeight :: Double
                     } deriving (Show)   

gSGS = ScoreGraphicSettings 10 5 40

-- qN
qnShape (x,y) = fill $ do circle (x,y) (qnSize gSGS); -- line (x-29,y-50) (x+20,y+20)

musDur n d = n % d :: Ratio Int -- % :: Integral a => a -> a -> Ratio a infixl 7

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
