{-# LANGUAGE CPP #-}

module Debug (
    debugM
  , shortNodeName
  ) where

import Debug.Trace ( traceM )

-- | Debug module
--
-- This module exports `debugM`, which emits debug output when debug mode is on.
--
-- Ideally, debug mode would be specified by a compile-flag, but I had trouble getting this working,
-- so we're making do for now by manually toggling the DEBUG_STEP #define below

---------------------------------------------------------


-- TODO: Why doesn't giving the flag in Stack work?
-- #define DEBUG_STEP

debugM :: (Monad m) => String -> m ()
#ifdef DEBUG_STEP
debugM = traceM
#else
debugM s = return ()
#endif


-- #define SHORT_NODE_NAME
shortNodeName :: Bool
#ifdef SHORT_NODE_NAME
shortNodeName = True
#else
shortNodeName = False
#endif