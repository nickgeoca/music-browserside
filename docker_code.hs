{-# LANGUAGE TypeSynonymInstances, 
             MultiParamTypeClasses,
             FlexibleInstances,
             DeriveDataTypeable,
             ForeignFunctionInterface,
             GeneralizedNewtypeDeriving,
             GADTs  #-}
-- Key words: TODO, BUG, NOTE, CONTINUE, FIX
--  $ ==> <$> <*> **> <** ==> <|> ==> <<< -> ++> <++ ==> <<

-- NOTE: Worth having highlight region as (coordinates,pic), instead of just the coordinates? Memory vs time trade off. When playing notes, may be better to also include the picture.
-- TODO: Note/rest highlight regions might need to incorporate the modifiers too. Otherwise one can't select a region by clicking on a clef since that is not a note/rest region. Otherwise treating the clef as a note/rest region would fix the problem.

import Haste hiding (Canvas)
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
import Control.Exception (evaluate)
import Control.Monad.Trans.Maybe
import Prelude hiding(id,div)
import GHC.Float
-- import Music -- BUG

foreign import ccall jsMidiLoadPlugin :: IO ()
foreign import ccall jsMidiNoteOn :: Int -> Int -> Int -> Float -> IO ()
foreign import ccall jsMidiNoteOff :: Int -> Int -> Float -> IO ()
foreign import ccall jsPerformanceNow :: IO Double


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
     mHgltCanv <- liftIO $ getCanvasById hgltCanvasId
     mXY <- liftM clickCoor getEventData
     case (mHgltCanv, mXY) of
      (Nothing,       Nothing)     -> return ()
      (Just hgltCanv, Just (x, y)) -> do playMusicRegion mgds (x, y) hgltCanv

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

appCanvas :: (Int, Int) -> (Double, Double) -> [Picture ()] -> String -> String -> Widget ()
appCanvas (sizeX,sizeY) (offsetX,offsetY) ps sId hId = 
  do wraw (do div ! id "canvasesdiv" 
                   ! canvasdivStyle sizeX sizeY
                   $ noHtml
                   `child` (canvasHtml sizeX sizeY sId 1)
                   `child` (canvasHtml sizeX sizeY hId 2))

     Just scoreCanvas <- liftIO $ getCanvasById sId
     render scoreCanvas $ do translate (offsetX, offsetY) $ do 
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
removeHighlight canv (BoxCoor x1 y1 x2 y2) = clearRect canv x1 y1 (x2-x1) (y2-y1)
-- render canv $ fill $     -- TODO: Use js clearRect() instead
--                      do rect (0, 0) 
--                              (0, 0) 

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

----------------------------------------------------------------------------------------------------                      
-- Play/Highlight score
----------------------------------------------------------------------------------------------------

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
   Nothing        -> do putMVar hgltInfo Nothing -- BUG: There needs to be delay here somehow. Otherwise the last note does not get highlighted.
                        return ()   

-- TODO: 1) Add delay between notes 2) Factor in bpm change and delay (absolute time stamp changes if bpm changes) 3) highlight on/off 4) Concurrency
-- playHgltUpd :: MidiContext -> GraphicMusicElm -> Widget MidiContext
playHgltUpd hgltInfo ctxt d = 
  case d of
   GMNoteElm h p ns -> do t <- liftIO $ liftM (250+) getTimeNow
                          midiPlayNotes t 0 ctxt ns   -- NOTE: Might be faster to noteOn all notes, then noteOff them, instead of alternating
                          putMVar hgltInfo (Just h) 
                          return ctxt
   GMRestElm h p    -> do wait 250
                          putMVar hgltInfo (Just h)
                          return ctxt
   GMCtxtElm ctxt'  -> return ctxt'

hgltNotesRests :: Canvas -> MVar (Maybe HgltBorder) -> CIO ()
hgltNotesRests hgltCanv mHgltInfo = 
  do mHgltOld <- newMVar $ BoxCoor 0 0 0 0 
     go hgltCanv mHgltInfo mHgltOld 
       where go hgltCanv mHgltInfo mHgltOld = do maybeHgltNew <- takeMVar mHgltInfo
                                                 hgltOld      <- takeMVar mHgltOld
                                                 liftIO $ removeHighlight hgltCanv hgltOld 
                                                 case maybeHgltNew of
                                                  Nothing      -> do return () 
                                                  Just hgltNew -> do putMVar mHgltOld hgltNew
                                                                     liftIO $ renderOnTop hgltCanv $ rendHighlight (RGBA 255 0 0 0.3) hgltNew
                                                                     go hgltCanv mHgltInfo mHgltOld

      
----------------------------------------------------------------------------------------------------                      
-- Javascript API
----------------------------------------------------------------------------------------------------
midiPlayNote :: Int -> MidiContext -> MidiNote -> IO ()
midiPlayNote chnl (MidiContext vol bpm) (MidiNote dur pitch) -- TODO: dynamic bpm integration 
  = let toFlt f a b = f (fromIntegral a) (fromIntegral b) 
        durFlt = (toFlt (/) 60  bpm) * (toFlt (/) (numerator dur) (denominator dur))
    in do midiNoteOn  chnl pitch vol 0  -- TODO: Add delay before this line of code!
          midiNoteOff chnl pitch durFlt


-- TODO: Make this a multi channel function for the sake of reducing latency
midiPlayNotes :: Double -> Int -> MidiContext -> [MidiNote] -> CIO ()  -- NOTE: This function is going to need additional work. Such as notes on same beat, but different volume.
midiPlayNotes t chnl (MidiContext vol bpm) ns -- TODO: dynamic bpm integration 
  = do playData  <- liftIO $ evaluate $ toPlayData ns bpm   -- Reduce latency. evaluate forces it to weak head normal form
       playNotes <- liftIO $ evaluate $ map (\(d,p) -> do midiNoteOn  chnl p vol 0) playData
       stopNotes <- liftIO $ evaluate $ map (\(d,p) -> do midiNoteOff chnl p d)     playData
       t'        <- liftIO $ evaluate t

       usecWait t'
       liftIO $ sequence_ playNotes
       liftIO $ sequence_ stopNotes
       return ()

         where toPlayData :: [MidiNote] -> Int -> [(Float, Int)]
               toPlayData ((MidiNote d p):ns) bpm = (durFlt d bpm, p) : toPlayData ns bpm
               toPlayData [] _ = []
               toFlt f a b = f (fromIntegral a) (fromIntegral b) 
               durFlt :: Ratio Int -> Int -> Float
               durFlt dur bpm = (toFlt (/) 60  bpm) * (toFlt (/) (numerator dur) (denominator dur))

usecWait :: Double -> CIO ()
usecWait tPlay = -- TODO: Add check here. If stopped playing or bpm change, then break? Or maybe at bottom of fnct.
  do now <- liftIO $ getTimeNow     -- TODO: Not ideal yet. Ideally burns no more than <1ms of CPU time, if none at all. Use better method?
     if (tPlay - now) < 1   -- Sub ms
       then do burnTime tPlay
       else let diff           = floor $ max 0 (tPlay - now)
                noMoreThan50ms = min 50 diff
            in do wait noMoreThan50ms
                  usecWait tPlay
  where burnTime :: Double -> CIO ()           -- TODO: Maybe use this in feature based on suggestion from IRC chat: Window.requestAnimationFrame()
        burnTime tPlay = do now <- liftIO getTimeNow  
                            if now > tPlay then return ()
                              else do burnTime tPlay

getTimeNow :: IO Double
getTimeNow
  = do jsPerformanceNow

midiLoadPlugin :: IO ()
midiLoadPlugin
  = do jsMidiLoadPlugin

midiNoteOn :: Int -> Int -> Int -> Float -> IO ()
midiNoteOn  channel pitch velocity delay
  = do jsMidiNoteOn channel pitch velocity delay

midiNoteOff :: Int -> Int -> Float -> IO ()
midiNoteOff channel pitch delay
  = do jsMidiNoteOff channel pitch delay
        

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
