import Distribution.Simple
import Distribution.Simple.Program.Run
import Distribution.Simple.Setup
import Distribution.PackageDescription
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import System.Process

-- | Write @gitCommitHash :: String@ and @gitCommitTime :: String@
-- variables to @src/GitStats.hs@.
gitStatsHook :: Args -> BuildFlags -> IO HookedBuildInfo
gitStatsHook _ bf = do
  let git = simpleProgramInvocation "git" [ "log"
                                          , "-n1"
                                          , "--format=format:%h %ci"
                                          ]
  out <- getProgramInvocationOutput (fromFlag $ buildVerbosity bf) git
  branch <- readProcess "sh"
                        ["-c", "git branch| grep '*'| cut -c 3-| tr -d '\n'"]
                        ""
  let (hash:timeChunks) = words out
      timeString = unwords timeChunks
      moduleName = "GitStats"
      fileName = moduleName ++ ".hs"
      hashVarId = HsIdent "gitCommitHash"
      timeVarId = HsIdent "gitCommitTime"
      branchVarId = HsIdent "gitBranch"
      stringType = HsTyCon (UnQual (HsIdent "String"))
      releaseFile = "release-" ++ branch
  -- Write Haskell source
  writeFile ("src/" ++ fileName) $
            prettyPrint $
            HsModule (SrcLoc fileName 1 1)
                     (Module moduleName)
                     (Just [ HsEVar (UnQual hashVarId)
                           , HsEVar (UnQual timeVarId)
                           , HsEVar (UnQual branchVarId)
                           ])
                     []
                     [ HsTypeSig
                       (SrcLoc fileName 2 1)
                       [hashVarId]
                       (HsQualType [] stringType)
                     , HsPatBind
                       (SrcLoc fileName 3 1)
                       (HsPVar hashVarId)
                       (HsUnGuardedRhs (HsLit (HsString hash)))
                       []
                     , HsTypeSig
                       (SrcLoc fileName 4 1)
                       [timeVarId]
                       (HsQualType [] stringType)
                     , HsPatBind
                       (SrcLoc fileName 5 1)
                       (HsPVar timeVarId)
                       (HsUnGuardedRhs (HsLit (HsString timeString)))
                       []
                     , HsTypeSig
                       (SrcLoc fileName 6 1)
                       [branchVarId]
                       (HsQualType [] stringType)
                     , HsPatBind
                       (SrcLoc fileName 7 1)
                       (HsPVar branchVarId)
                       (HsUnGuardedRhs (HsLit (HsString branch)))
                       []
                     ]
  appendFile releaseFile $ out ++ "\n"
  return emptyHookedBuildInfo


main = defaultMainWithHooks $
       simpleUserHooks{preBuild = gitStatsHook}
