{-# LANGUAGE OverloadedStrings #-}

module RESTLoader where

import Control.Monad.State (get)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU (fromString, toString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M

import qualified Data.Aeson as Aeson

import Network.HTTP
import Network.Browser
import Network.URI (parseURI)

import RedsonTypes

type BrowserSt = BrowserState (HandleStream LB.ByteString)


login :: IO BrowserSt
login = browse $ do
    setAllowRedirects True
    setDebugLog Nothing
    setOutHandler $ const $ return ()
    let Just loginUri = parseURI "http://localhost:8000/login"
    let rq = mkRequest' POST loginUri "login=admin&password="
    (_,resp) <- assertStatus (2,0,0) $ request rq
    get --BrowserState


create :: BrowserSt
       -> ModelName           -- ^ Model name
       -> Commit              -- ^ Key-values of instance data
       -> IO (Either LB.ByteString InstanceId)
create h modelName commit = browse $ withBrowserState h $ do
  let baseUri = "http://localhost:8000/_/" ++ BU.toString modelName
  let Just uri = parseURI baseUri
  (_,rsp) <- assertStatus (2,0,1) $
               request $ mkRequest' POST uri "{}"

  let caseId = Aeson.decode (rspBody rsp)
               >>= M.lookup ("id" :: B.ByteString)
  case caseId of
    Nothing    -> return $ Left $ rspBody rsp
    Just ident -> do
        let Just uri = parseURI $ baseUri
                     ++ "/" ++ BU.toString ident
        let rq   = mkRequest' PUT uri $ Aeson.encode commit
        (_,rsp) <- assertStatus (2,0,4) $ request rq
        return $ Right ident


assertStatus code f = f >>= \(uri,rsp) ->
  if rspCode rsp == code
      then return (uri,rsp)
      else fail $ "unexpected HTTP status:\n" ++ show rsp



mkRequest' method uri body
  = replaceHeader HdrContentType "application/json"
  . replaceHeader HdrContentLength (show $ LB.length body)
  $ Request { rqURI = uri
            , rqMethod = method
            , rqHeaders = []
            , rqBody = body
            }
