-- Create all the views.

-- The views are always created because all views are deleted at the start ot the
-- migration process.

-- Conventions:
--  * VIEWs use TitleCase table names and camelCase columns for easy differentiation from tables.

-- The standard utxo view which shows all unspent transaction outputs.
create view "Utxo" as select
  tx_out.*
  from tx_out left outer join tx_in
  on tx_out.tx_id = tx_in.tx_out_id and tx_out.index = tx_in.tx_out_index
  where tx_in.tx_in_id is null;

create view "TransactionOutput" as
select
  address,
  tx.hash as "txId",
  index
from
  tx
  inner join tx_out on tx.id = tx_out.tx_id;

create view "TransactionInput" as
select
  address,
  tx.hash as "sourceTxId",
  tx_in.tx_out_index as "sourceTxIndex",
  value
from
  tx
  inner join tx_out on tx.id = tx_out.tx_id
  inner join tx_in on tx_in.tx_out_id = tx.id;

create view "BlockMeta" as
-- The common table expression isn't strictly speaking needed,
-- but it does clean up this view quite a lot
with block_meta_cte as (
  select
    id,
    block_no,
    slot_no,
    quote_literal(block.slot_no * (select slot_duration from meta) * 0.001) as time_since_start,
    (select start_time from meta) as start_time
  from block
)
select
  id,
  block_no as "blockNo",
  slot_no as "slotNo",
  time_since_start as "secondsSinceGenesis",
  case when slot_no >= 0
    then start_time + cast (time_since_start as interval)
    else start_time
  end as "createdAt" 
from block_meta_cte;

create view "Block" as
select
  "BlockMeta"."createdAt",
  -- TODO - Needs to be modelled
  -- epoch
  -- TODO - Needs to be modelled
  -- slot
  -- TODO: Optimise
  (select sum(fee) from tx where tx.block = block.id) as "fees"
  block."hash" as id,
  block.merkel_root as "merkelRootHash",
  block.block_no as number,
  previous_block."hash" as "previousBlock",
  block.size as size
from block
left outer join block as previous_block
  on block.previous = previous_block.id
left outer join tx
  on tx.block = block.id
inner join "BlockMeta"
  on "BlockMeta".id = block.id;

create view "Transaction" as
select
  tx.block,
  tx.fee,
  tx.hash as id,
  "BlockMeta"."createdAt" as "includedAt",
  -- TODO: Optimise
  (select sum("value") from tx_out where tx_id = tx.id) as "totalOutput"
from
  tx
inner join "BlockMeta"
  on "BlockMeta".id = tx.block;