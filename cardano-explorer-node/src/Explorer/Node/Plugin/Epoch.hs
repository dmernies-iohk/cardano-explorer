{-# LANGUAGE OverloadedStrings #-}

module Explorer.Node.Plugin.Epoch
  ( epochPluginOnStartup
  , epochPluginInsertBlock
  ) where

import           Cardano.BM.Trace (Trace, logError)

import qualified Cardano.Chain.Block as Ledger

import           Control.Monad (join, when)
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
import           Explorer.Node.Error

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..))
import           Ouroboros.Network.Block (BlockNo (..))


epochPluginOnStartup :: Trace IO Text -> ReaderT SqlBackend (NoLoggingT IO) ()
epochPluginOnStartup trce =
    maybe (pure ()) loop =<< queryLatestBlockEpochNo
  where
    loop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    loop epochNum = do
      either (liftIO . reportError) (const $ nextLoop epochNum) =<< updateEpochNum epochNum

    nextLoop :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
    nextLoop epochNum
      | epochNum > 0 = loop (epochNum - 1)
      | otherwise = pure ()

    reportError :: ExplorerNodeError -> IO ()
    reportError err =
      logError trce $ "epochPluginOnStartup: " <> renderExplorerNodeError err


epochPluginInsertBlock :: Trace IO Text -> ByronBlock -> BlockNo -> ReaderT SqlBackend (NoLoggingT IO) ()
epochPluginInsertBlock trce blk tipBlkNo =
  case byronBlockRaw blk of
    Ledger.ABOBBoundary bblk -> do
      let newEpoch = Ledger.boundaryEpoch (Ledger.boundaryHeader bblk)
      -- As long as the current epoch is > 0 we update the epcoch table on a boundary block.
      when (newEpoch > 0) $ do
        liftIO . logError trce $ "epochPluginInsertBlock: Updating epoch table for epoch " <> textShow (newEpoch - 1)
        res <- updateEpochNum (newEpoch - 1) -- Update the last epoch.
        case res of
          Left err -> liftIO . logError trce $ "epochPluginInsertBlock: " <> renderExplorerNodeError err
          Right () -> pure ()
    Ledger.ABOBBlock blk ->
      slotsPerEpoch <- 10 * DB.metaProtocolConst <$> liftLookupFail "insertABlock" DB.queryMeta
      unless (blockNumber blk > unBlockNo tipBlkNo - 10) $ do

    let  =  meta

    slid <- lift . DB.insertSlotLeader $ mkSlotLeader blk
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = blockHash blk
                    , DB.blockEpochNo = Just $  `div` slotsPerEpoch

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

{-
queryLatestEpochNo :: MonadIO m => ReaderT SqlBackend m (Maybe Word64)
queryLatestEpochNo = do
  res <- select . from $ \ epoch -> do
            orderBy [asc (epoch ^. EpochNo)]
            limit 1
            pure $ (epoch ^. EpochNo)
  pure $ unValue <$> listToMaybe res
-}
