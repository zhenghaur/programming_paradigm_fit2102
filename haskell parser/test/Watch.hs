{-# LANGUAGE OverloadedStrings #-} -- for FilePath literals
module Watch where

import           System.FSNotify
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( forever )

conf = WatchConfig { confDebounce     = Debounce 10
                   , confUsePolling   = True
                   , confPollInterval = 1000
                   }

watch :: Action -> FilePath -> IO a
watch action targetDir = withManagerConf conf $ \mgr -> do
    -- start a watching job (in the background)
    watchDir mgr          -- manager
             targetDir        -- directory to watch
             (const True) -- predicate
             action       -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
