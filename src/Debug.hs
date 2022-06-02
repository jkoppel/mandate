{-# LANGUAGE CPP #-}

module Debug (
    isDebug
  , debugM
  , shortNodeName
  ) where

import Debug.Trace ( traceM )

---------------------------------------------------------

---- FOR DESCRIPTIONS OF FLAGS
----
---- See README.md


isDebug :: Bool
#ifdef DEBUG_STEP
isDebug = True
#else
isDebug = False
#endif

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