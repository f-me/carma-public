
module CustomLogic.ActionAssignment
  (assignActions
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Ord (comparing)
import Data.List

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Snap.Snaplet.Auth (Role(..), AuthUser(..))


type Action = Map ByteString ByteString
type Login = Text
type RoleName = ByteString
type Duetime = Int
type Assignment = Map Login (Map Duetime [Action])


assignActions :: UTCTime -> [Action] -> Map Login AuthUser -> Map Login [Action]
assignActions now actions loggedUsers
  = freshAssignments
  where
    nowSeconds = round $ utcTimeToPOSIXSeconds now

    actionsByAssignee :: Assignment
    actionsByAssignee = foldl' (flip addActionToAssignment) Map.empty actions

    actionsToAssign :: [Action]
    actionsToAssign
      -- give a chance to priority actions to get best assignment (possibly)
      = sortBy (comparing $ Map.lookup "priority")
      -- take only actions with `duetime` in nearest future (20 minutes)
      $ concatMap (concat . Map.elems . fst . Map.split (nowSeconds+20*60))
      -- take only unassigned actions
      -- and actions assigned to the users that are not logged in
      $ Map.elems $ Map.difference actionsByAssignee loggedUsers

    usersByRole :: Map RoleName [Login]
    usersByRole = foldl' go Map.empty $ Map.elems loggedUsers
      where
        go m usr = foldl' go' m $ userRoles usr
          where
            go' m' (Role r) = Map.insertWith (++) r [userLogin usr] m'

    freshAssignments :: Map Login [Action]
    freshAssignments
      = fst
      $ foldl' go (Map.empty,actionsByAssignee) actionsToAssign
      where
        go (new,ass) act -- YEP!
          | null matchingUsers = (new,ass)
          | otherwise =
            (Map.insertWith' (++) bestUser [act'] new
            ,flip addActionToAssignment ass act'
            )
          where
            act' = Map.insert "duetime" (B8.pack $ show duetime')
                 $ Map.insert "assignedTo" (T.encodeUtf8 bestUser)
                 $ act
            role = Map.findWithDefault "(error)" "targetGroup" act
            matchingUsers = Map.findWithDefault [] role usersByRole
            usersTimeline usr = Map.findWithDefault Map.empty usr ass

            duetime = maybe 0 fst $ Map.lookup "duetime" act >>= B8.readInt
            -- patch duetime for expired actions
            duetime' = if duetime > nowSeconds
              then duetime
              else nowSeconds + 10*60

            bestUser = maximumBy
              (comparing $ heuristic nowSeconds duetime' . usersTimeline)
              matchingUsers


heuristic :: Int -> Duetime -> Map Duetime [Action] -> Double
heuristic now due timeline
  = log (due' - prevAct) + 2*log (nextAct - due')
  where
    due' = fromIntegral due
    -- find free time interval containing the action
    (actsBefore,actsAfter) = Map.split due timeline
    prevAct = fromIntegral
      $ maybe now (fst.fst) $ Map.maxViewWithKey actsBefore
    nextAct = fromIntegral
      $ maybe (maxBound :: Int) (fst.fst) $ Map.minViewWithKey actsAfter

----------------------------------------------------------------------

addActionToAssignment :: Action -> Assignment -> Assignment
addActionToAssignment act
  = Map.insertWith' (Map.unionWith (++)) assignee singleton
  where
    assignee = maybe "" T.decodeUtf8 $ Map.lookup "assignedTo" act
    duetime  = maybe 0 fst $ Map.lookup "duetime" act >>= B8.readInt
    singleton= Map.singleton duetime [act]
