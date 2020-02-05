{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Plugin.Epoch
  ( epochPluginInsertBlock
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (newExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.),
                    count, from, just, max_, min_, on, select, sum_, val, where_)
                    -- asc, count, from, just, limit, max_, min_, on, orderBy, select, sum_, val, where_)

import           Database.Persist.Class (repsert)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (Epoch (..), EpochId, EntityField (..), listToMaybe, maybeToEither)
import           Explorer.Node.Error

epochPluginInsertBlock

epochPluginInsertBlock :: MonadIO m => Word64 -> ExceptT ExplorerNodeError (ReaderT SqlBackend m) ()
epochPluginInsertBlock epochNum = do
   epochId <- newExceptT $ queryEpochId epochNum
   epoch <- newExceptT $ queryEpochEntry epochNum
   newExceptT $ do
      repsert epochId epoch
      pure $ Right ()

type ValMay a = Value (Maybe a)


queryEpochId :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError EpochId)
queryEpochId epochNum = do
  res <- select . from $ \ epoch -> do
            where_ (epoch ^. EpochNo ==. val epochNum)
            pure $ (epoch ^. EpochId)
  pure $ maybeToEither (ENEEpochLookup epochNum) unValue (listToMaybe res)

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
queryLatestBlockEpochNo
  res <- select . from $ \ epoch -> do
            where_ (isJust (blk ^. BlockEpochNo))
            orderBy [blk ^. BlockEpochNo)]
            limit 1
            pure $ (blk ^. BlockEpochNo)
  pure $ unValue <$> listToMaybe res

queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [asc (epoch ^. EpochNo)]
            limit 1
            pure $ (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res
