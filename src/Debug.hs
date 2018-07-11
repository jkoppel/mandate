{-# LANGUAGE CPP #-}

module Debug (
    debugM
  ) where

import Debug.Trace ( traceM )

-- TODO: Why doesn't giving the flag in Stack work?
#define DEBUG_STEP

debugM :: (Monad m) => String -> m ()
#ifdef DEBUG_STEP
debugM = traceM
#else
debugM s = return ()
#endif
