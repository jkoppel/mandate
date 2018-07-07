{-# LANGUAGE CPP #-}

module Debug (
    debugStepM
  ) where

import Debug.Trace ( traceM )

-- TODO: Why doesn't giving the flag in Stack work?
-- #define DEBUG_STEP

debugStepM :: (Monad m) => String -> m ()
#ifdef DEBUG_STEP
debugStepM = traceM
#else
debugStepM s = return ()
#endif
