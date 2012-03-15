
import           Control.Exception (finally)
import           System.Environment (getArgs)
import           Database.Redis as Redis

import Partners
import VINs


main = do
  dir:_ <- getArgs
  rConn <- Redis.connect Redis.defaultConnectInfo
  Partners.loadFiles dir rConn
    >> VINs.loadFiles dir rConn
    `finally` runRedis rConn quit



