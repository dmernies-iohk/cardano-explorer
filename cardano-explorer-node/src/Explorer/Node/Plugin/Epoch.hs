{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Plugin.Epoch
  ( updateEpochNum
  ) where

import           Cardano.BM.Trace (Trace, logError)

import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.),
                    asc, count, from, just, limit, max_, min_, on, orderBy, select, sum_, val, where_)

import           Database.Persist.Class (repsert)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (Epoch (..), EpochId, EntityField (..), isJust, listToMaybe, maybeToEither)
import           Explorer.Node.Error

epochPluginOnStartup :: Trace IO Text -> ReaderT SqlBackend (NoLoggingT IO) ()
epochPluginOnStartup trce =
    maybe (pure ()) loop =<< queryLatestBlockEpochNo
  where
    loop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    loop epochNum = do
      ee <- updateEpochNum epochNum
      case ee of
        Left err -> liftIO . logError trce $ "epochPluginOnStartup: " <> renderExplorerNodeError err
        Right () -> do
          if epochNum >= 1
            then loop (epochNum - 1)
            else pure ()

-- -------------------------------------------------------------------------------------------------

type ValMay a = Value (Maybe a)

updateEpochNum :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError ())
updateEpochNum epochNum = do
    maybe (pure $ Right ()) update =<< queryEpochId epochNum
  where
    update :: MonadIO m => EpochId -> ReaderT SqlBackend m (Either ExplorerNodeError ())
    update epochId = do
      eEpoch <- queryEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left err
        Right epoch -> Right <$> repsert epochId epoch

queryEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe EpochId)
queryEpochId epochNum = do
  res <- select . from $ \ epoch -> do
            where_ (epoch ^. EpochNo ==. val epochNum)
            pure $ (epoch ^. EpochId)
  pure $ unValue <$> (listToMaybe res)

queryEpochEntry :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError Epoch)
queryEpochEntry epochNum = do
    res <- select . from $ \ (tx `InnerJoin` blk) -> do
              on (tx ^. TxBlock ==. blk ^. BlockId)
              where_ (blk ^. BlockEpochNo ==. just (val epochNum))
              pure $ (sum_ (tx ^. TxOutSum), count (tx ^. TxOutSum), min_ (blk ^. BlockTime), max_ (blk ^. BlockTime))
    case listToMaybe res of
      Nothing -> pure $ Left (ENEEpochLookup epochNum)
      Just x -> pure $ convert x
  where
    convert :: (ValMay Word64, Value Word64, ValMay UTCTime, ValMay UTCTime) -> Either ExplorerNodeError Epoch
    convert tuple =
      case tuple of
        (Value (Just outSum), Value txCount, Value (Just start), Value (Just end)) ->
            Right $ Epoch outSum txCount epochNum start end
        _other -> Left $ ENEEpochLookup epochNum


queryLatestBlockEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestBlockEpochNo = do
  res <- select . from $ \ blk -> do
            where_ (isJust (blk ^. BlockEpochNo))
            orderBy [asc (blk ^. BlockEpochNo)]
            limit 1
            pure $ (blk ^. BlockEpochNo)
  pure $ join (unValue <$> listToMaybe res)

queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [asc (epoch ^. EpochNo)]
            limit 1
            pure $ (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res
