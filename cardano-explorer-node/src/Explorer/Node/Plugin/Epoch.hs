{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Plugin.Epoch
  ( epochPluginOnStartup
  , epochPluginInsertBlock
  ) where

import           Cardano.BM.Trace (Trace, logError, logInfo)

import qualified Cardano.Chain.Block as Ledger

import           Control.Monad (join, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.),
                    asc, count, from, just, limit, max_, min_, on, orderBy, select, sum_, val, where_)

import           Database.Persist.Class (repsert)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (Epoch (..), EpochId, EntityField (..), isJust, listToMaybe)
import qualified Explorer.DB as DB
import           Explorer.Node.Error
import           Explorer.Node.Util

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..))
import           Ouroboros.Network.Block (BlockNo (..))


epochPluginOnStartup :: Trace IO Text -> ReaderT SqlBackend (NoLoggingT IO) ()
epochPluginOnStartup trce = do
    liftIO . logInfo trce $ "epochPluginOnStartup: Checking"
    maybe (pure ()) loop =<< queryLatestBlockEpochNo
  where
    loop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    loop epochNum = do
      liftIO . logInfo trce $ "epochPluginOnStartup: Inserting epoch table for epoch " <> textShow epochNum
      either (liftIO . reportError) (const $ nextLoop epochNum) =<< updateEpochNum epochNum

    nextLoop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    nextLoop epochNum
      | epochNum > 0 = loop (epochNum - 1)
      | otherwise = pure ()

    reportError :: ExplorerNodeError -> IO ()
    reportError err =
      logError trce $ "epochPluginOnStartup: " <> renderExplorerNodeError err


epochPluginInsertBlock :: Trace IO Text -> ByronBlock -> BlockNo -> ReaderT SqlBackend (NoLoggingT IO) (Either ExplorerNodeError ())
epochPluginInsertBlock trce rawBlk tipBlkNo =
    case byronBlockRaw rawBlk of
      Ledger.ABOBBoundary bblk -> do
        let newEpoch = Ledger.boundaryEpoch (Ledger.boundaryHeader bblk)
        -- As long as the current epoch is > 0 we update the epcoch table on a boundary block.
        if newEpoch > 0
          then do
            liftIO . logInfo trce $ "epochPluginInsertBlock: about to update table updated for epoch " <> textShow (newEpoch - 1)
            res <- updateEpochNum (newEpoch - 1) -- Update the last epoch.
            liftIO . logInfo trce $ "epochPluginInsertBlock: Epoch table updated for epoch " <> textShow (newEpoch - 1)
            pure res
          else pure $ Right ()
      Ledger.ABOBBlock blk -> do
        if blockNumber blk > unBlockNo tipBlkNo - 10
          then do
            eMeta <- DB.queryMeta
            case eMeta of
              Left err -> do
                logErr $ Left (ENELookup "epochPluginInsertBlock" err)

              Right meta -> do
                let slotsPerEpoch = 10 * DB.metaProtocolConst meta
                updateEpochNum (slotNumber blk `div` slotsPerEpoch)
          else do
            liftIO . logError trce $ "epochPluginInsertBlock: Ledger.ABOBBlock " <> textShow (blockNumber blk)
            pure $ Right ()
  where
    logErr :: MonadIO m => Either ExplorerNodeError () -> ReaderT SqlBackend m (Either ExplorerNodeError ())
    logErr res = do
      case res of
        Left err -> liftIO . logError trce $ "epochPluginInsertBlock: " <> renderExplorerNodeError err
        Right () -> pure ()
      pure res

-- -------------------------------------------------------------------------------------------------

type ValMay a = Value (Maybe a)

updateEpochNum :: MonadIO m => Word64 -> ReaderT SqlBackend m (Either ExplorerNodeError ())
updateEpochNum epochNum = do
    maybe insertEpoch updateEpoch =<< queryEpochId epochNum
  where
    updateEpoch :: MonadIO m => EpochId -> ReaderT SqlBackend m (Either ExplorerNodeError ())
    updateEpoch epochId = do
      eEpoch <- queryEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left err
        Right epoch -> Right <$> repsert epochId epoch

    insertEpoch :: MonadIO m => ReaderT SqlBackend m (Either ExplorerNodeError ())
    insertEpoch = do
      eEpoch <- queryEpochEntry epochNum
      case eEpoch of
        Left err -> pure $ Left err
        Right epoch -> do
          void $ DB.insertEpoch epoch
          pure $ Right ()

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

{-
queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [asc (epoch ^. EpochNo)]
            limit 1
            pure $ (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res
-}
