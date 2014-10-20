{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module Salesforce
( Config(..)
, runSalesforceT
, login
, mkRuntimeState
) where

import Control.Exception (SomeException, Handler(..), catches)
import Control.Monad (liftM)
import Control.Monad.Catch (MonadThrow(..), Exception(..))
import Control.Monad.Catch.Pure (CatchT(..))
import Control.Monad.Reader (ReaderT(..), asks)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.String.Here (i)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Conduit as H
import Network.HTTP.Types (statusCode)
import Salesforce.Token (Token(..))

data Config = Config { baseUrl :: String }

data Error = TimeoutError
           | NoResourceError
           | BadRequestError String
           | InternalError String
           | UnknownError String
             deriving (Typeable, Show)

instance Exception Error

data RuntimeState = RuntimeState
                      { config            :: Config
                      , connectionManager :: H.Manager
                      , token             :: Token
                      }

type SalesforceT m = ReaderT RuntimeState (CatchT m)

runSalesforceT :: Monad m => RuntimeState -> SalesforceT m a -> m (Either Error a)
runSalesforceT c f = runCatchT (runReaderT f c) >>= return . (either (Left . exceptionToError) Right)
  where exceptionToError :: SomeException -> Error
        exceptionToError e = fromMaybe (UnknownError $ show e) $ fromException e

login :: (MonadIO m, MonadThrow m) => BS.ByteString -> BS.ByteString -> SalesforceT m ()
login username password = do
  let body = [ ("grant_type", "password")
             , ("client_id", "3MVG9y6x0357Hlef_t5.O2puWQwv6.U9fuSvygsduJclfnczbowHK_ol6_SjMAbj4NPvl9aqmDTiTLB4Co0fj")
             , ("client_secret", "4629088528601793123")
             , ("username", username)
             , ("password", password)
             ]
  request <- H.urlEncodedBody body `liftM` parseUrl "https://login.salesforce.com/services/oauth2/token"
  runRequest request

runRequest :: (MonadIO m, MonadThrow m)
           => H.Request -> SalesforceT m ()
runRequest request = do
  let request' = request { checkStatus = \_ _ _ -> Nothing
                         }
  manager <- asks connectionManager

  response <- handleExceptionsAndResult $ H.httpLbs request' manager

  let sc = statusCode (responseStatus response)

  case () of _
              | sc `elem` [200..299] -> return ()
              | sc == 400 -> throwM $ BadRequestError ""
              | sc == 404 -> throwM $ NoResourceError
              | sc == 409 -> return ()
              | sc == 502 -> throwM $ TimeoutError
              | sc `elem` [502..599] -> throwM $ InternalError ""
              | otherwise -> throwM $ UnknownError ""

handleExceptionsAndResult :: (MonadIO m, MonadThrow m) => IO a -> SalesforceT m a
handleExceptionsAndResult monad = do
  res <- liftIO $ (fmap Right monad) `catches` [ Handler handleHttpException
                                               , Handler handleOtherExceptions
                                               ]
  case res of
    Left v -> throwM v
    Right x -> return x

handleHttpException :: HttpException -> IO (Either Error a)
handleHttpException ResponseTimeout = return $ Left $ TimeoutError
handleHttpException e               = return $ Left $ UnknownError $ show e

handleOtherExceptions :: SomeException -> IO (Either Error a)
handleOtherExceptions e = return $ Left $ UnknownError $ show e

mkRuntimeState :: Config -> IO RuntimeState
mkRuntimeState config = do
  manager <- H.newManager tlsManagerSettings
  return $ RuntimeState config manager (Token "" "" "" "" "" "")
