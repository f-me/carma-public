{-|

Stores build-time information about hash and time of top commit in the
current branch of Git repository from which CaRMa is built.

AUTO-UPDATED BY CABAL WHEN BUILDING CARMA.

-}

module GitStats (gitCommitHash, gitCommitTime) where
 
gitCommitHash :: String
gitCommitHash = "UNKNOWN"
 
gitCommitTime :: String
gitCommitTime = "UNKNOWN"
