{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.XML.HXT.Core
import Text.XML.HXT.HTTP
import Data.Ratio
import Control.Monad.State.Lazy
import Music               -- This is to import the Music type
import Data.Default  

main = do
  d <- runX $ xunpickleDocument xpMusic            -- TODO: Return Music2 instead of [Music2]
    [withValidate no
--    ,withTrace 2
    ,withRemoveWS yes
    ,withPreserveComment no] "demo-score.xml"             
    -- >>> arrIO (\x -> do {print x; return x})
  let d2 = postProcessing (head d)                     
  mapM (putStrLn.show) d2
  return ()
  where postProcessing m = evalState (mapM fixPositions m) (0%1)

----------------------------------------------------------------------------------------------------                      
-- Post Processing
----------------------------------------------------------------------------------------------------
fixPositions :: ((Position, Note2) -> State Position (Position, Note2))
fixPositions (_, note) = do
  let duration = dur2 note
  position <- get             -- Get position for this note
  put (position + duration)   -- Put position for next note
  return (position, note) 
               
  
----------------------------------------------------------------------------------------------------                      
-- Music Type Pickling
----------------------------------------------------------------------------------------------------
instance XmlPickler Music2   where  xpickle = xpMusic
instance XmlPickler Note2    where  xpickle = xpNote2
instance XmlPickler Position where  xpickle = xpPrim :: PU (Ratio Int)

instance (Default a, Eq a) => Default (PU a) where def = xpDefault (def::a) $ xpZero ""

xpMusic :: PU Music2
xpMusic
  = startPt $
    xpList $
    xpPair def xpickle 
  where startPt a = xpElem "score-partwise" $                -- Select
                    keepElem "part"    $ xpElem "part"    $  -- Fitler, select
                    keepElem "measure" $ xpElem "measure" $  -- Filter, select
                    keepElem "note" $ a                      -- Filter

xpNote2 :: PU Note2
xpNote2
  = xpElem "note" $                       -- Create list out of selected note elems
    keepElems ["pitch", "durationTest"] $          -- Ignore other elems
    xpWrap (uncurry3 Note2,                        -- 
            \t -> (dur2 t, pitch2 t, mods2 t)) $
    xpTriple (xpElem "durationTest" xpickle)       -- 
             (xpElem "pitch" $ keepElem "octave" $ xpElem "octave" xpickle)
             def
        
----------------------------------------------------------------------------------------------------                      
-- Helper functions
----------------------------------------------------------------------------------------------------
keepElems :: [String] -> PU a -> PU a
keepElems ls = let msum' = foldr (<+>) zeroArrow  -- (hasName "a") <+> (hasName "b") ... 
               in xpFilterCont $ msum' (map hasName ls)

keepElem :: String -> PU a -> PU a
keepElem x = xpFilterCont (hasName x)

----------------------------------------------------------------------------------------------------                      
-- Example, figuring stuff out
----------------------------------------------------------------------------------------------------
type Title = String
  
instance XmlPickler Title where
  xpickle = xpTitle

-- xpTitle :: PU Title
xpTitle
  = xpElem "score-partwise" $
    xpFilterCont (hasName "movement-title") $
    xpElem "movement-title" xpText

{-
  = xpElem "score-partwise" $ xpFilterCont (hasName "abc123") $ xpAttr "title" xpText    
<score-partwise title="Pitches and accidentals">
  <identification>
    <miscellaneous>
-}
    
-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- instance Monad m => Monad (MaybeT m) where
--   return = MaybeT . return . Just
--   x >>= f = MaybeT $ do y <- runMaybeT x
--                         Case y of Nothing -> return Nothing
--                                   Maybe z -> runMaybeT $ f z)

-- M a -> (a -> M b) -> M b   
