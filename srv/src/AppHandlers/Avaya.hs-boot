module AppHandlers.Avaya

where

import Data.Model.Patch as Patch
import Carma.Model.Usermeta

import DMCC

import Application

setAgentState :: SettableAgentState -> Patch.Patch Usermeta -> AppHandler ()