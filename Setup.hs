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
import Music BUG

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
                               bufferX    :: Double   -- Displacement to next annotation. (uses percent of X dimension to scale)
                             }
              
-- TODO: Is best way of tieing constant dimensions with a global? E.g. staff measurements.
--         This could change in future, if staff measurements were to be dynamic.
noteAnno = Annotation (5,5)    (0.5, 0.5) (NoteLocDy BottomOfStaff ((measureHeight gSGS) / 8)) (4 * (fst $ dimensions noteAnno))

notePic n (x,y) = let (num, denom) = (numerator $ dur n, denominator $ dur n)
                  in case (num, denom) of
                      (1,4) -> fill $ do circle (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)
                      _     -> fill $ do circle (x,y) (fst $ dimensions noteAnno); line (x-29,y-50) (x+20,y+20)

           
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

drawCanvas_ :: Music -> [Picture ()] -> Double -> [Picture ()]              
drawCanvas_ []          pics _        = pics  
drawCanvas_ ((p,e):mus) pics xDispAcc = drawCanvas_ mus (pic:pics) (xDispAcc + xDispNew)
  where (pic, xDispNew) = case e of 
                           NoteElm n -> let coor1 = (0, (measureHeight gSGS) - (noteDy (Clef GClef 2 0) n)) 
                                            coor2 = coor1 + (xDispAcc, 0)
                                        in (notePic n coor2, (bufferX noteAnno))
                          -- RestElm r -> 2
                          -- ModElm  m -> 3
        
----------------------------------------------------------------------------------------------------                      
-- Examples
----------------------------------------------------------------------------------------------------  

musicTest :: Music       
musicTest = [ (0 % 1, ModElm [AnnoTime TimeCommon,
                              KeySym (Main.Key {keyfifths = 0, keymode = Major}),
                              TimingSym (1 % 1),
                              ClefSym (Clef {clefsign = GClef, clefline = 2, clefoctalt = 0})])
            , (0 % 1, NoteElm (Note {dur = 1 % 4, pitch = 53, mods = []}))
            , (1 % 4, NoteElm (Note {dur = 1 % 4, pitch = 55, mods = []}))
            , (1 % 2, NoteElm (Note {dur = 1 % 4, pitch = 57, mods = []}))
            , (3 % 4, NoteElm (Note {dur = 1 % 4, pitch = 59, mods = []}))
            , (1 % 1, NoteElm (Note {dur = 1 % 4, pitch = 48, mods = []}))
            , (5 % 4, NoteElm (Note {dur = 1 % 4, pitch = 50, mods = []}))
            , (3 % 2, NoteElm (Note {dur = 1 % 4, pitch = 52, mods = []}))
            , (7 % 4, NoteElm (Note {dur = 1 % 4, pitch = 53, mods = []}))
            ]
           
