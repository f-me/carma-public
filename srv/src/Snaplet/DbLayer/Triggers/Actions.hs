{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes #-}

module Snaplet.DbLayer.Triggers.Actions (actions)

where

import Control.Monad
import Control.Monad.Trans
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Read as T
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.String (fromString)

------------------------------------------------------------------------------
import WeatherApi (getWeather', tempC)
-----------------------------------------------------------------------------

import Snap (gets)
import qualified Snap.Snaplet.PostgresqlSimple as PG
import Snap.Snaplet.PostgresqlSimple ((:.)(..), Only(..))
import Database.PostgreSQL.Simple.SqlQQ
import Snaplet.DbLayer.Types
import Snaplet.DbLayer.Triggers.Types
import Snaplet.DbLayer.Triggers.Dsl
import Snaplet.DbLayer.Triggers.MailToDealer

import           Data.Model

import qualified Carma.Model.Case as Case
import qualified Carma.Model.Contract as Contract
import qualified Carma.Model.ContractCheckStatus as CCS
import qualified Carma.Model.Program as Program
import qualified Carma.Model.SubProgram as SubProgram

import Util as U


add :: (Ord k1, Ord k2) =>
       k1 -> k2 -> [a]
    -> Map.Map k1 (Map.Map k2 [a])
    -> Map.Map k1 (Map.Map k2 [a])
add model field tgs = Map.unionWith (Map.unionWith (++)) $ Map.singleton model (Map.singleton field tgs)

actions :: MonadTrigger m b => Map.Map ModelName (Map.Map FieldName [ObjectId -> FieldValue -> m b ()])
-- actions :: TriggerMap a
actions
    = Map.fromList
      [("case", Map.fromList
          [("city", [setWeather])
          ,("contract", [\objId val ->
                         unless (T.null val) $
                           fillFromContract val objId >>= \case
                             Loaded -> set objId "vinChecked" $
                                       identFv CCS.base
                             Expired -> set objId "vinChecked" $
                                        identFv CCS.vinExpired
                             None -> return ()
                        ])
          ,("psaExportNeeded",
            [\caseRef val -> when (val == "1") $ tryRepTowageMail caseRef])
          ])
        ]

-- | Mapping between contract and case fields.
contractToCase :: [(FA Contract.Contract, FA Case.Case)]
contractToCase =
    [ (FA Contract.name, FA Case.contact_name)
    , (FA Contract.vin, FA Case.car_vin)
    , (FA Contract.make, FA Case.car_make)
    , (FA Contract.model, FA Case.car_model)
    , (FA Contract.seller, FA Case.car_seller)
    , (FA Contract.plateNum, FA Case.car_plateNum)
    , (FA Contract.makeYear, FA Case.car_makeYear)
    , (FA Contract.color, FA Case.car_color)
    , (FA Contract.buyDate, FA Case.car_buyDate)
    , (FA Contract.lastCheckDealer, FA Case.car_dealerTO)
    , (FA Contract.transmission, FA Case.car_transmission)
    , (FA Contract.engineType, FA Case.car_engine)
    , (FA Contract.engineVolume, FA Case.car_liters)
    , (FA Contract.carClass, FA Case.car_class)
    , (FA Contract.subprogram, FA Case.subprogram)
    ]


data ContractFillResult = None
                        -- ^ No contract found.
                        | Loaded
                        -- ^ Contract loaded from database.
                        | Expired
                        -- ^ Contract loaded and is expired.


fillFromContract :: MonadTrigger m b =>
                    ObjectId
                 -- ^ Contract id.
                 -> ObjectId
                 -> m b ContractFillResult
fillFromContract contract objId = do
  let cid :: IdentI Contract.Contract
      cid = case T.decimal contract of
          Right (i,_) -> Ident i
          _           -> error "Could not read contract id"
      contractTable = tableQT Contract.ident
      programTable = tableQT Program.ident
      subProgramTable = tableQT SubProgram.ident
  res <- liftDb $ PG.query
         (fromString $ unwords
          [ "SELECT"
          -- 2 * M arguments, where M is the length of contractToCase
          , intercalate "," $ map (const "?.?::text") contractToCase
          -- 1: program id field
          , ", p.?::text"
            -- 1 argument: Contract table name
          , "FROM ? c"
          -- 3 more parameters: SubProgram table name, Contract
          -- subprogram field, subprogram id field.
          , "JOIN ? s ON c.? = s.?"
          -- 3 more parameters: Program table name, SubProgram parent
          -- field, program id field.
          , "JOIN ? p ON s.? = p.?"
            -- 2 more arguments: contract id field, contract id value
          , "WHERE c.? = ?;"
          ]) $
         -- Selected fields
         ToRowList
         (map (\f -> (PT "c", PT $ fieldNameE $ fst f)) contractToCase)
         -- 2
         :. (Only $ fieldPT Program.ident)
         :. (Only contractTable)
         -- 3
         :. (Only subProgramTable)
         :. (fieldPT Contract.subprogram,
             fieldPT SubProgram.ident)
         -- 3
         :. Only programTable
         :. (fieldPT SubProgram.parent, fieldPT Program.ident)
         -- 2
         :. (fieldPT Contract.ident, cid)
  case res of
    [] -> return None
    [row] -> do
      -- Replace only empty fields of case
      let setIfEmpty oid nm val = get oid nm >>= \case
              "" -> set objId nm val
              _  -> return ()
      zipWithM_ (maybe (return ()) . setIfEmpty objId)
                (map (fieldNameE . snd) $
                 contractToCase ++ [(undefined, FA Case.program)])
                row
      resExp <- liftDb $ PG.query
                [sql|SELECT ((now() < ?) or (? < now())) FROM ? WHERE ? = ?;|]
                ( fieldPT Contract.validSince
                , fieldPT Contract.validUntil
                , contractTable
                , fieldPT Contract.ident
                , cid)
      return $ case resExp of
                 [Only (Just True)] -> Expired
                 _                  -> Loaded
    _ -> error "fillFromContract: Contract primary key is broken"


setWeather :: MonadTrigger m b => ObjectId -> Text -> m b ()
setWeather objId city = do
  conf    <- liftDb $ gets weather
  weather <- liftIO $ getWeather' conf $ T.unpack $ T.filter (/= '\'') city
  case weather of
    Right w   -> do
      syslogJSON Debug "trigger/weather"
        [ "objId" .= objId
        , "city"  .=  city
        , "res"   .= show w
        ]
      set objId "temperature" $ T.pack $ show $ tempC w
    Left  err -> do
      set objId "temperature" ""
      syslogJSON Debug "trigger/weather"
        [ "objId" .= objId
        , "city"  .= city
        , "error" .= show err
        ]
