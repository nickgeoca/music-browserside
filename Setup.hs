{-# LANGUAGE TypeSynonymInstances, 
             MultiParamTypeClasses,
             FlexibleInstances,
             DeriveDataTypeable,
             ForeignFunctionInterface  #-}

import Haste
import Haste.Graphics.Canvas
import Haste.Perch
import Prelude hiding(id,div)
import Haste.HPlay.View
import GHC.Float
import Data.Ratio
import Control.Monad.IO.Class
import Haste.Foreign
import Haste.Prim (toJSStr)
import Debug.Trace
import Data.Typeable  
import Data.List
-- import Music -- BUG

----------------------------------------------------------------------------------------------------                      
-- Main code
----------------------------------------------------------------------------------------------------  
main = runBody $ do
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

instance ToPict Key where
  toPic k (x,y) = fill $ do rect (x,y) (dimensions keyAnno)

instance ToPict Timing where
  toPic t (x,y) = fill $ do rect (x,y) (dimensions timingAnno)

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

undefined -- Comment this line out
-- TODO: Generate partially curried functions, before drawing.
-- Then calculate how many full measures in a line. Then more calculating..
-- then sequence_ functions with some values to fit pictures in the page width.
-- Retain sequence in event user zooms.
sUpdAnnoDx    a = do s <- getSData ; setSData $ s {sXDisp = (sXDisp s) + (annoDx a)}
sUpdPos       p = do s <- getSData ; setSData $ s {sPos   = (sPos   s) + p}              
-- Update dx following parallel notes of same duration.
sUpdParNoteDx   = do s <- getSData ; setSData $ s {sXDisp = (sXDisp s) + (fst $ dimensions noteAnno)}   -- TODO: Create global constant for update amount, outside of this function
data RendState = RendState 
  { sXDisp  :: Double
  , sClef   :: Clef
  , sKey    :: Key
  , sTiming :: Timing
  , sPos    :: Position  
  } deriving (Typeable)


drawCanvas m = do setSData $ RendState 0 (Clef NoneClef 0 0) (Main.Key 0 Major) (Timing 4 4 Nothing)    -- TODO: create monad function then map over it. Default state of clef is "none" clef
                  drawCanvas' m []

drawCanvas' [] pics = return pics
drawCanvas' ((pos, elm): mus) pics =
  do state <- getSData :: View Perch IO RendState
     r <- case elm of 
           NoteElm n -> let n' = groupBy (\x y -> if dur x == dur y then True else False) n
                            fm = if length n' > 1
                                 then do sUpdParNoteDx; return ()
                                 else do return ()
                        in do ns <- mapM (\ls -> do v <- mapM notesToGraphics ls; fm; return v) n'
                              sUpdAnnoDx noteAnno
                              return $ concat ns
           RestElm r -> do return undefined
           ModElm  m -> do mapM modifiersToGraphics m
     drawCanvas' mus (pics ++ r)

notesToGraphics n = do
  state <- getSData
  let c  = sClef state
      x' = sXDisp state
      y' = (measureHeight gSGS) - (noteDy c n)
    in return $ toPic n (x', y')

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

