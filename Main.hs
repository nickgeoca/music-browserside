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
  where postProcessing = fixPositions 

----------------------------------------------------------------------------------------------------                      
-- Post Processing
----------------------------------------------------------------------------------------------------
fixPositions :: [(Position, Note2)] -> Music2
fixPositions m = evalState (mapM f m) (0%1)
  where f (_, note) = do
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
instance XmlPickler MXmlStep where  xpickle = xpPrim

instance (Default a, Eq a) => Default (PU a) where
  def = xpLift (def::a)

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
             (xpElem "pitch" xpPitch)
             def

f9 a s o = fromMXmlPitch s a o

--               (b -> a) -> PU a -> (a -> PU b) -> PU b
-- xpLift2 :: (a -> b -> c) -> PU a -> PU b -> PU c            
xpLift2 f pa pb = xpSeq
                  (\_ -> def)                   -- BUG
                  (xpPair pa pb)
                  (\a -> xpLift $ uncurry.f a)  

-- xpLift2 f pa pb = xpSeq
--                   (uncurry.f)
--                   (xpLift def)
--                   (\_ -> xpPair pa pb)  
  
-- xpLift2  :: PU a -> PU b -> PU (a, b)
-- xpLift2  
-- xpLift2 f pa pb
--     = ( xpSeq fst pa (\ a ->
--         xpSeq snd pb (\ b ->
--         xpLift $ f a b))
--       ) { theSchema = undefined }           
           
xpPitch :: PU Pitch
xpPitch
  =  xpLift2 (f9 Nothing) (xpElem "step" xpickle) (xpElem "octave" xpickle)
--   = undefined -- liftM2 (f9 Nothing ) (xpElem "step" xpickle) (xpElem "octave" xpickle)
--   = keepElem "octave" $ xpElem "octave" xpickle

-- combine' = 
    
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
