{-# LANGUAGE TypeSynonymInstances, 
             MultiParamTypeClasses,
             FlexibleInstances,
             DeriveDataTypeable,
             ForeignFunctionInterface,
             GeneralizedNewtypeDeriving,
             GADTs  #-}
-- Key words: TODO, BUG, NOTE, CONTINUE
--  $ ==> <$> <*> **> <** ==> <|> ==> <<< -> ++> <++ ==> <<

-- NOTE: Worth having highlight region as (coordinates,pic), instead of just the coordinates? Memory vs time trade off. When playing notes, may be better to also include the picture.
-- TODO: Note/rest highlight regions might need to incorporate the modifiers too. Otherwise one can't select a region by clicking on a clef since that is not a note/rest region. Otherwise treating the clef as a note/rest region would fix the problem.

import Haste
import Haste.Graphics.Canvas hiding (Ctx, Shape)
import Haste.Perch hiding (map, head)
import Haste.Concurrent hiding ((!), Key)
import Haste.HPlay.View hiding (map, head)
import Haste.Foreign
import Haste.Prim (toJSStr)  
import Data.Ratio
import Data.Typeable
import Data.Maybe
import Data.Monoid  
import Data.List
import Data.Ord (comparing)
import Data.Function (on)
-- import Data.Foldable hiding (sequence_, sum)
import System.IO.Unsafe
import Unsafe.Coerce  
import Debug.Trace
import Control.Monad.IO.Class
import Control.Monad
import Prelude hiding(id,div)
import GHC.Float
-- import Music -- BUG

foreign import ccall jsMoveTo :: Ctx -> Double -> Double -> IO ()
foreign import ccall jsQuadraticCurveTo :: Ctx -> Double -> Double -> Double -> Double -> IO ()
foreign import ccall jsMidiLoadPlugin :: IO ()
foreign import ccall jsMidiNoteOn :: Int -> Int -> Int -> Float -> IO ()
foreign import ccall jsMidiNoteOff :: Int -> Int -> Float -> IO ()

newtype Ctx = Ctx JSAny                   -- TODO: Should be in library
            deriving (Pack, Unpack)

newtype Shape a = Shape {unS :: Ctx -> IO a} -- TODO: Should be in library

(!>) = flip trace
infixr 0 !>             

----------------------------------------------------------------------------------------------------                      
-- Main code
----------------------------------------------------------------------------------------------------
main = do addHeader jsHeader
          runBody $ do
            center <<< wbutton "delay1" "delay1"
            liftIO midiLoadPlugin
            center <<< wbutton "render" "render"
            scoreWidget musicTest
            -- return $ midiPlayNote 0 $ MidiNote 50 127 .25


scoreWidget :: Music -> Widget ()
scoreWidget score =
  do -- Initialize dynamic data
     -- setSData $ SGSettingsDynamic 600  BUG
     -- canvasWidth <- liftM sgsCanvasWidth (getSData :: Widget SGSettingsDynamic) BUG
     canvasWidth <- liftM (\x->x) (return 500)
     scoreData <- drawCanvas score (0,30)                     -- Widget [ScoreRenderElm]  -- TODO: Use offsetX/Y here instead

     let offsetX  = 0 :: Double
         offsetY  = 30 :: Double
         rndr     = scoreToPics scoreData :: [Picture ()]
--         updData  = map (offsetHglt (offsetX, offsetY)) scoreData
         mgds     = scoreToMGDS scoreData :: MidiHgltDS
         scoreCanvasId = "canvas1"
         hgltCanvasId  = "canvas2"
         canvasHeight  = 400

     do (appCanvas (canvasWidth, canvasHeight) (offsetX, offsetY) rndr scoreCanvasId hgltCanvasId) `fire` OnClick
     -- **********// Widget Event //**********
     Just hgltCanv <- liftIO $ getCanvasById hgltCanvasId  -- BUG: Handle nothing case
     Just (x,y) <- liftM clickCoor getEventData   -- BUG: Handle nothing case
     playMusicRegion mgds (x, y) hgltCanv

     -- wraw $ p << ((show x) ++" "++ (show y))
     -- let Just (_,mgds') = search (x,y) mgds 
     -- wraw $ p << (fmap show mgds')

     -- Return
     return ()

     -------------------- 
       where pred x (coor,_) = x >= x1Coor coor && x < x2Coor coor
             clickCoor :: EventData -> Maybe (Int, Int)
             clickCoor d = case evData d of
                            Click _ (x,y) -> Just (x,y)
                            _             -> Nothing
             toHighlight (x:xs) = case x of
                                   GMNoteElm h _ _ -> h : toHighlight xs
                                   GMRestElm h _   -> h : toHighlight xs
                                   GMCtxtElm _     ->     toHighlight xs
             toHighlight [] = []
             toHgltPics xs = let redC  = RGBA 255 0   0 0.3
                                 yelC  = RGBA 255 255 0 0.3
                             in zipWith rendHighlight (cycle [redC,yelC]) xs
             toList LTNil = []
             toList (LTList d ds)                         = d : toList ds
             toList (LTTree (Branch l r (LTNode k ds d))) = d : toList ds
{-
             offsetHglt :: (Double, Double) -> ScoreRenderElm -> ScoreRenderElm
             offsetHglt xy e = case e of 
                                ScoreRendNotes h p ls  -> ScoreRendNotes (upd xy h) p ls
                                ScoreRendRest  h p pic -> ScoreRendRest  (upd xy h) p pic 
                                ScoreRendMod   p       -> ScoreRendMod   p     
               where upd (x,y) (BoxCoor x1 y1 x2 y2) = let x' = round x
                                                           y' = round y 
                                                       in BoxCoor (x'+x1) (y'+y1) (x'+x2) (y'+y2)

     {-
     -- Highlighting
     let xAdj = x - (round offsetX)
     renderOnTop hgltCanv $ do
       translate (offsetX, offsetY) $ do
         sequence_ $ toHgltPics $ toHighlight $ toList mgds
     -}

-}
appCanvas :: (Int, Int) -> (Double, Double) -> [Picture ()] -> String -> String -> Widget ()
appCanvas (sizeX,sizeY) (offsetX,offsetY) ps sId hId = 
  do wraw (do div ! id "canvasesdiv" 
                   ! canvasdivStyle sizeX sizeY
                   $ noHtml
                   `child` (canvasHtml sizeX sizeY sId 1)
                   `child` (canvasHtml sizeX sizeY hId 2))
     -- Score canvas
     -- center <<< wbutton "Draw Score" "Draw Score"         -- Play button
     Just scoreCanvas <- liftIO $ getCanvasById sId  -- BUG: Handle nothing case
     render scoreCanvas $ do 
       translate (offsetX, offsetY) $ do 
         staffShape (0,0) 100
         sequence_ ps
           where canvasdivStyle x y   = Haste.Perch.style ("position: relative; width: " ++ (show x) ++ "; height: " ++ (show y) ++ ";") 
                 canvasHtml x y cId z = canvas ! id cId
                                               ! Haste.Perch.style ("border: 1px solid black; z-index: "++ (show z) ++"; position: absolute; left: 0px; top: 0px;")
                                               ! atr "width" (show x)
                                               ! height (show y)
                                               $ noHtml

scoreToPics :: [ScoreRenderElm] -> [Picture ()]
scoreToPics (x:xs) = case x of
                          ScoreRendNotes _ _ ls -> extract ls ++ scoreToPics xs
                          ScoreRendRest  _ _ p  -> p          :  scoreToPics xs
                          ScoreRendMod       p  -> p          :  scoreToPics xs
  where extract (x:xs) = snd x : extract xs
        extract []     = []
scoreToPics []     = []

-- 1) Remove rests on identical beat as notes, but not the rests that are on same beat as modification elmements.
-- 2) Keep Position in ScoreRenderElm. This will be used for midi timings.
-- 3) Keep MidiNote.
-- 4) Build ListTree. Use ScoreRendMod as the tree nodes, and notes/rests as the list elements        
scoreToMGDS :: [ScoreRenderElm] -> MidiHgltDS
scoreToMGDS xs = let xs1 = removeRestWhenSameBeatAsNote xs
                     xs2 = toGMList xs1  
                     ds1 = createTree xs2 20
                 in ds1
  where toGMList :: [ScoreRenderElm] -> [GraphicMusicElm]
        toGMList (x:xs) = case x of
                           ScoreRendNotes h p ls -> GMNoteElm h p (extract ls) : toGMList xs
                           ScoreRendRest  h p _  -> GMRestElm h p              : toGMList xs
                           ScoreRendMod       _  -> toGMList xs  -- BUG: Want to keep some modifiers, but removing all for now
          where extract (x:xs) = fst x : extract xs
                extract []     = []
        toGMList []     = []

createTree :: [GraphicMusicElm] -> Int -> MidiHgltDS
createTree xs n = let addNodes = dsAddNodes n n (MidiContext 127 60) (BoxCoor 0 0 0 0) 
                  in dsLinkNodes $ addNodes $ listToLT xs

-- NOTE: This builds a tree for every 20 note/rests. Otherwise it is possible to have conditions where there are 30 modifiers and the key is the same between two tree nodes. Work around is to have a list of modifiers, instead of a modifier element.
dsAddNodes _ _ midi lastHglt LTNil = LTNil
dsAddNodes n m midi lastHglt (LTList d ds) =  
  let newHglt = fromMaybe lastHglt $ getHgltBorder d
      newMidi = updateMidi midi d
      m'      = m + if notMidiCtxt d then 1 else 0  -- Here is where skipping is done
      go      = dsAddNodes n 0 newMidi newHglt (LTList d ds)
  in if n == m 
     then LTTree $ Branch Nothing Nothing $ LTNode newHglt go (GMCtxtElm newMidi) -- newMidi isn't needed b/c it will be on next node, but w/e. This is for LTTree case
     else LTList d $ dsAddNodes n m' newMidi newHglt ds


search :: (Int,Int) -> MidiHgltDS -> Maybe (MidiContext, MidiHgltDS)
search key ds = search' key ds (MidiContext 127 60)  -- TODO: Make a default midicontext here. Or some alterantive method

-- TODO: Need to pick up the local contexts in this function
search' :: (Int,Int) -> MidiHgltDS -> MidiContext -> Maybe (MidiContext, MidiHgltDS)
search' key (LTTree (Branch _ _(LTNode h ds d))) ctxt = let ctxt' = fromMaybe ctxt (getMidiContext d) 
                                                        in if pointInBox key h  
                                                           then Just (ctxt', ds)
                                                           else search' key ds ctxt'
search' key (LTList d ds) ctxt = case getHgltBorder d of
                                  Just h  -> if pointInBox key h  
                                             then Just (ctxt, ds)
                                             else search' key ds ctxt
                                  Nothing ->      search' key ds ctxt
search' _ LTNil _ = Nothing

pointInBox (x,y) (BoxCoor x1 y1 x2 y2) =
  if x >= x1 && x < x2 && y >= y1 && y < y2 then True else False

dsLinkNodes ds = ds  -- TODO: Needs work

-- BUG: Needs work right here. Instead of updating with context, shoudl be local context.
updateMidi midi (GMCtxtElm m) = m
updateMidi midi _             = midi

removeRestWhenSameBeatAsNote x = x        -- BUG: Complete this function.

rendHighlight :: Color -> BoxCoor -> Picture ()    
rendHighlight c bc = color c $ fill $
                      do rect (fromIntegral $ x1Coor bc, fromIntegral $ y1Coor bc) 
                              (fromIntegral $ x2Coor bc, fromIntegral $ y2Coor bc) 
-- removeHighlight :: Color -> BoxCoor -> Picture ()    
removeHighlight canv bc = render canv $ fill $     -- TODO: Use js clearRect() instead
                      do rect (0, 0) 
                              (0, 0) 
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
----------------------------------------------------------------------------------------------------                      
-- Random types. Better organize
----------------------------------------------------------------------------------------------------  
data ScoreRenderElm = ScoreRendNotes HgltBorder Position    [(MidiNote, Picture ())] |
                      ScoreRendRest  HgltBorder Position     (Picture ())            |
                      ScoreRendMod                        (Picture ())

-- NOTE: Double vs Int, speed/canvas-errors
-- NOTE: Watch int errors here
data BoxCoor = BoxCoor {
  x1Coor :: Int, y1Coor :: Int,
  x2Coor :: Int, y2Coor :: Int
  } deriving (Show,Read)

type HgltBorder = BoxCoor

 
----------------------------------------------------------------------------------------------------                      
-- List-Tree Data Structure
----------------------------------------------------------------------------------------------------
data BinaryTree a = Branch {
  tL :: Maybe (BinaryTree a),
  tR :: Maybe (BinaryTree a),
  tD :: a  -- 'a' is at end, so it makes printing easier in the application.
  } deriving (Show, Read)

data LTNode k ds d = LTNode {
  ltKey      :: k,
  ltNodeDS   :: ds,
  ltNodeData :: d
  } deriving (Show, Read)

data ListTree key lElm where
  LTTree     :: BinaryTree (LTNode key (ListTree key lElm) lElm) -> ListTree key lElm
  LTList     :: lElm  -> ListTree key lElm -> ListTree key lElm
  LTNil      :: ListTree key lElm
  deriving (Show, Read)

listToLT :: [a] -> ListTree key a
listToLT []     = LTNil
listToLT (x:xs) = LTList x (listToLT xs)

getLTDataRestMaybe ds = 
  case ds of
   LTNil                                -> Nothing
   LTList d ds'                         -> Just (d,ds') 
   LTTree (Branch _ _ (LTNode _ ds' d)) -> Just (d,ds')

{-
-- NOTE: Should this be strict?
instance Foldable (ListTree HgltBorder) where
  foldMap f LTNil         = mempty
  foldMap f (LTList d ds) = f d `mappend` foldMap f ds
  foldMap f (LTTree (Branch l r (LTNode k ds d)))
                          = f d `mappend` foldMap f ds
  foldr f b = go
    where go LTNil         = b
          go (LTList d ds) = d `f` go ds 
          go (LTTree (Branch l r (LTNode k ds d)))
                           = d 'f' go ds 
-}

instance Functor (ListTree HgltBorder) where
  fmap f LTNil         = LTNil   -- TODO: Rebuild tree here?
  fmap f (LTList d ds) = LTList (f d) (fmap f ds)
  fmap f (LTTree (Branch l r (LTNode k  ds          d   )))
       = (LTTree (Branch Nothing Nothing (LTNode k (fmap f ds) (f d))))


----------------------------------------------------------------------------------------------------                      
-- Graphic/Midi Types
----------------------------------------------------------------------------------------------------
data MidiNote = MidiNote {
  mDur   :: Duration,
  mPitch :: Int
  } deriving (Show, Read)

data MidiLocalContext = MidiLocalContext {
  mNil :: ()
  } deriving (Show, Read)

data MidiContext = MidiContext {
  mVol  :: Int,  -- Volume (0-127)
  mBpm  :: Int   -- BPM
  } deriving (Show, Read)

data GraphicMusicElm = GMNoteElm HgltBorder Position [MidiNote] |
                       GMRestElm HgltBorder Position            |
                       GMCtxtElm MidiContext deriving (Show, Read)

type MidiHgltDS = ListTree HgltBorder GraphicMusicElm


getHgltBorder (GMNoteElm h _ _) = Just h
getHgltBorder (GMRestElm h _  ) = Just h
getHgltBorder _ = Nothing

getMidiContext (GMNoteElm _ _ _) = Nothing
getMidiContext (GMRestElm _ _  ) = Nothing
getMidiContext (GMCtxtElm m) = Just m 

-- type GraphicMusic    = [GraphicMusicElm]

data PlaybackState = PlaybackState {
  pbVol  :: Int,
  pbBpm  :: Int,
  pbStop :: Bool
  }

notMidiCtxt (GMCtxtElm _) = False
notMidiCtxt _ = True



-- playMusicRegion :: MidiHgltDS -> (Int, Int) -> Widget ()
-- playMusicRegion ds xy = playMusic ds $ MidiContext 127 60
 --BUG: Add the comment back in
playMusicRegion :: MidiHgltDS -> (Int, Int) -> Canvas -> Widget ()
playMusicRegion ds xy hgltCanv = -- playMusic ds $ MidiContext 127 60
  case search xy ds of  
   Just (ctxt, ds') -> do liftIO $ concurrent $ do hgltInfo <- newEmptyMVar
                                                   forkIO $ hgltNotesRests hgltCanv hgltInfo
                                                   playMusic hgltInfo ds' ctxt
   Nothing          -> return ()

-- playMusic :: MidiHgltDS -> MidiContext -> Widget ()
playMusic hgltInfo ds ctxt = 
  case getLTDataRestMaybe ds of
   Just (d', ds') -> do ctxt' <- playHgltUpd hgltInfo ctxt d'
                        playMusic hgltInfo ds' ctxt'
   Nothing        -> do putMVar hgltInfo (True, BoxCoor 0 0 0 0) 
                        return ()

-- TODO: 1) Add delay between notes 2) Factor in bpm change and delay (absolute time stamp changes if bpm changes) 3) highlight on/off 4) Concurrency
-- playHgltUpd :: MidiContext -> GraphicMusicElm -> Widget MidiContext
playHgltUpd hgltInfo ctxt d = 
  case d of
   GMNoteElm h p ns -> do wait 250
                          liftIO $ midiPlayNotes 0 ctxt ns   -- NOTE: Might be faster to noteOn all notes, then noteOff them, instead of alternating
                          putMVar hgltInfo (False, h) 
                          return ctxt
   GMRestElm h p    -> do wait 250
                          putMVar hgltInfo (False, h)  -- TODO: Make this maybe type
                          return ctxt
   GMCtxtElm ctxt'  -> return ctxt'

hgltNotesRests :: Canvas -> MVar (Bool, HgltBorder) -> CIO ()
hgltNotesRests hgltCanv mHgltInfo = 
  do mHgltOld <- newMVar $ BoxCoor 0 0 0 0 
     go hgltCanv mHgltInfo mHgltOld 
       where go hgltCanv mHgltInfo mHgltOld = 
               do (exit, hgltNew) <- takeMVar mHgltInfo
                  hgltOld         <- takeMVar mHgltOld
                  putMVar mHgltOld hgltNew
                  if exit 
                    then do removeHighlight hgltCanv hgltOld 
                            return () 
                    else do removeHighlight hgltCanv hgltOld 
                            renderOnTop hgltCanv $ rendHighlight (RGBA 255 0 0 0.3) hgltNew
                            go hgltCanv mHgltInfo mHgltOld


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
        
----------------------------------------------------------------------------------------------------                      
-- Examples
----------------------------------------------------------------------------------------------------  
musicTest :: Music       
musicTest = [ {- (0 % 1,ModElm [ ClefSym (Clef {clefsign = GClef, clefline = 2, clefoctalt = 0})
                            , KeySym (Main.Key {keyfifths = 0, keymode = Major})
                            , TimingSym (Timing 4 4 (Just TimeCommon))
                            ]) -}
              (0 % 1,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})
                             , (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (1 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (1 % 2,NoteElm [ (Note {dur = 1 % 4, pitch = 57, mods = []})])
            , (3 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 59, mods = []})])
--             , (1 % 1,ModElm  [ TimingSym (Timing 4 4 (Just TimeCommon))])
            , (1 % 1,NoteElm [ (Note {dur = 1 % 4, pitch = 48, mods = []})])
            , (5 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 50, mods = []})])
            , (3 % 2,NoteElm [ (Note {dur = 1 % 4, pitch = 52, mods = []})])
            , (7 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})])
-- {-
            , (8 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (9 % 4,NoteElm [ (Note {dur = 1 % 4, pitch = 57, mods = []})])
            , (10% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 59, mods = []})])
            , (11% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 48, mods = []})])
            , (12% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 50, mods = []})])
            , (13% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 52, mods = []})])
            , (14% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})])
            , (15% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (16% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 57, mods = []})])
            , (17% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 59, mods = []})])
            , (18% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 48, mods = []})])
            , (19% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 50, mods = []})])
            , (20% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 52, mods = []})])
            , (21% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 30, mods = []})])
            , (22% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})])
            , (23% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})])
            , (24% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 53, mods = []})])
            , (25% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 55, mods = []})])
            , (26% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 57, mods = []})])
            , (27% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 59, mods = []})])
            , (28% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 48, mods = []})])
            , (29% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 50, mods = []})])
            , (30% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 52, mods = []})])
            , (31% 4,NoteElm [ (Note {dur = 1 % 4, pitch = 30, mods = []})])

-- -}
            ]
            
                       
----------------------------------------------------------------------------------------------------                      
-- Notes, ideas, etc. Might be out of date
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------                      
-- Misc
----------------------------------------------------------------------------------------------------                      

----------------------------------------------------------------------------------------------------                      
-- Javascript API
----------------------------------------------------------------------------------------------------
midiPlayNote :: Int -> MidiContext -> MidiNote -> IO ()
midiPlayNote chnl (MidiContext vol bpm) (MidiNote dur pitch) -- TODO: dynamic bpm integration 
  = let toFlt f a b = f (fromIntegral a) (fromIntegral b) 
        durFlt = (toFlt (/) 60  bpm) * (toFlt (/) (numerator dur) (denominator dur))
    in do midiNoteOn  chnl pitch vol 0  -- TODO: Add delay before this line of code!
          midiNoteOff chnl pitch durFlt


midiPlayNotes :: Int -> MidiContext -> [MidiNote] -> IO ()  -- NOTE: This function is going to need additional work. Such as notes on same beat, but different volume.
midiPlayNotes chnl (MidiContext vol bpm) ns -- TODO: dynamic bpm integration 
  = let playData = toPlayData ns bpm -- TODO: Make playData variable strict to improve latency
    in do mapM (\(d,p) -> do midiNoteOn  chnl p vol 0) playData  -- TODO: Add delay before this line of code!
          mapM (\(d,p) -> do midiNoteOff chnl p d)     playData
          return ()
            where toPlayData :: [MidiNote] -> Int -> [(Float, Int)]
                  toPlayData ((MidiNote d p):ns) bpm = (durFlt d bpm, p) : toPlayData ns bpm
                  toPlayData [] _ = []
                  toFlt f a b = f (fromIntegral a) (fromIntegral b) 
                  durFlt dur bpm = (toFlt (/) 60  bpm) * (toFlt (/) (numerator dur) (denominator dur))

midiLoadPlugin :: IO ()
midiLoadPlugin
  = do jsMidiLoadPlugin

midiNoteOn :: Int -> Int -> Int -> Float -> IO ()
midiNoteOn  channel pitch velocity delay
  = do jsMidiNoteOn channel pitch velocity delay

midiNoteOff :: Int -> Int -> Float -> IO ()
midiNoteOff channel pitch delay
  = do jsMidiNoteOff channel pitch delay
        
quadraticCurve :: Point -> Point -> Point -> Shape ()
quadraticCurve (x1,y1) (x2,y2) (cpx,cpy) = Shape $ \ctx -> do
  jsMoveTo ctx x1 y1
  jsQuadraticCurveTo ctx cpx cpy x2 y2

-- Javascript headers
jsHeader :: Perch
jsHeader = (   script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/inc/shim/Base64.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/inc/shim/Base64binary.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/inc/shim/WebAudioAPI.js" $ noHtml)           
           --
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/audioDetect.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/gm.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/loader.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/plugin.audiotag.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/plugin.webaudio.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/plugin.webmidi.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/midi/player.js" $ noHtml)           
           --
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/util/dom_request_xhr.js" $ noHtml)
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/mudcube/MIDI.js/master/js/util/dom_request_script.js" $ noHtml)                                 
           --
           <> (script ! atr "type" "text/javascript" ! src "https://rawgit.com/nickgeoca/js_misc/master/misc.js" $ noHtml)              -- API to plug into

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


{-
E: Midi Playing note
 * E1: Refused to get unsafe header "Content-Length-Raw"
       dom_request_xhr.js:95
       var rawBytes = parseInt(xhr.getResponseHeader('Content-Length-Raw'));
 * E2: Uncaught TypeError: undefined is not a function
       misc.js:16
       MIDI.noteOn(channel, note, velocity, delay);
-}
