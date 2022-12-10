{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module App.HttpIdService
  ( HttpIdService (..),
  )
where

import Control.Monad.Logger
  ( MonadLogger,
    logErrorN,
    logInfoN
  )
import Network.HTTP.Simple (HttpException, JSONException, getResponseBody, httpJSON)
import Repos (IdService (nextCargoId), IdServiceError (SomeIdServiceErr))
import Types (CargoId (CargoId))
import UnliftIO (MonadUnliftIO, try, tryAny)

newtype HttpIdService m a = HttpIdService (m a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      -- , MonadReader AppEnv
      MonadIO,
      MonadUnliftIO,
      MonadLogger
    )

instance (MonadUnliftIO m, MonadLogger m) => IdService (HttpIdService m) where
  nextCargoId = do
    logInfoN "Test"
    response <- try $ getResponseBody <$> httpJSON "http://localhost:3000/new-uid"
    case response of
      Right uid -> pure $ Right $ CargoId uid
      Left e
        | Just (_ :: HttpException) <- fromException e ->
          errWithLog "HTTP request failed" e
      Left (fromException -> Just (e :: JSONException)) ->
        errWithLog "UID from JSON paring failed" e
      Left unknown -> errWithLog "Unknown error" unknown
    where
      errWithLog msg e = do
        logErrorN  ("Error getting ID: " <> show e)
        pure $ Left $ SomeIdServiceErr msg
    
  {-# INLINE nextCargoId #-}