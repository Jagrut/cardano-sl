{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Abstract definition of a wallet
module Wallet.Abstract (
    -- * Abstract definition of a wallet
    IsWallet(..)
  , Ours
  , Pending
    -- * Abstract definition of rollback
  , Rollback(..)
    -- * Inductive wallet definition
  , Inductive(..)
  , interpret
    -- ** Invariants
  , Invariant
  , invariant
    -- ** Testing
  , walletInvariants
  , walletEquivalent
    -- ** Generation
    -- $generation
  , genFromBlockchain
  , genFromBlockchainPickingAccounts
    -- * Auxiliary operations
  , balance
  , txIns
  , txOuts
  , updateUtxo
  , updatePending
  , utxoRestrictToOurs
  ) where

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Universum

import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Text.Buildable
import           Formatting (bprint, build, sformat, (%))
import           Pos.Util (HasLens', lensOf')
import           Pos.Util.QuickCheck.Arbitrary (sublistN)
import           Test.QuickCheck

import           Util
import           Util.Validated
import           UTxO.Context
import           UTxO.Crypto
import           UTxO.DSL
import           UTxO.PreChain

{-------------------------------------------------------------------------------
  Wallet type class
-------------------------------------------------------------------------------}

-- | Check if an address is ours
type Ours a = a -> Maybe SomeKeyPair

-- | Pending transactions
type Pending h a = Set (Transaction h a)

-- | Abstract definition of a wallet
class (Hash h a, Ord a) => IsWallet w h a where
  utxo       :: w h a -> Utxo h a
  ours       :: w h a -> Ours a
  applyBlock :: Block h a -> w h a -> w h a

  -- Operations with default implementations

  pending :: w h a -> Pending h a
  default pending :: HasLens' (w h a) (Pending h a)
                  => w h a -> Pending h a
  pending w = w ^. lensOf'

  newPending :: Transaction h a -> w h a -> Maybe (w h a)
  default newPending :: HasLens' (w h a) (Pending h a)
                     => Transaction h a -> w h a -> Maybe (w h a)
  newPending tx w = do
      guard $ trIns tx `Set.isSubsetOf` utxoDomain (available w)
      return $ w & lensOf' %~ Set.insert tx

  availableBalance :: IsWallet w h a => w h a -> Value
  availableBalance = balance . available

  totalBalance :: IsWallet w h a => w h a -> Value
  totalBalance = balance . total

  available :: IsWallet w h a => w h a -> Utxo h a
  available w = utxoRemoveInputs (txIns (pending w)) (utxo w)

  change :: IsWallet w h a => w h a -> Utxo h a
  change w = utxoRestrictToOurs (ours w) (txOuts (pending w))

  total :: IsWallet w h a => w h a -> Utxo h a
  total w = available w `utxoUnion` change w

-- | Variation on 'newPending' which simply ignores any transactions
-- that do not belong to us
newPending' :: IsWallet w h a => Transaction h a -> w h a -> w h a
newPending' tx w = fromMaybe w $ newPending tx w

{-------------------------------------------------------------------------------
  Rollback
-------------------------------------------------------------------------------}

class IsWallet w h a => Rollback w h a where
  rollback :: w h a -> w h a

{-------------------------------------------------------------------------------
  Interlude: "functor" over different wallet types (internal use only)
-------------------------------------------------------------------------------}

data Wallets :: [(* -> *) -> * -> *] -> (* -> *) -> * -> * where
  One :: IsWallet w h a
      => w h a -> Wallets '[w] h a

  Two :: (IsWallet w h a, IsWallet w' h a)
      => w h a -> w' h a -> Wallets '[w,w'] h a

walletsMap :: (forall w. IsWallet w h a => w h a -> w h a)
           -> Wallets ws h a -> Wallets ws h a
walletsMap f (One w)    = One (f w)
walletsMap f (Two w w') = Two (f w) (f w')

{-------------------------------------------------------------------------------
  Inductive wallet definition
-------------------------------------------------------------------------------}

-- | Inductive definition of a wallet
--
-- TODO: We should generate random 'Inductive's and then verify the
-- invariants.
data Inductive h a =
    WalletEmpty
  | ApplyBlock (Block       h a) (Inductive h a)
  | NewPending (Transaction h a) (Inductive h a)
  deriving Eq

-- | Interpreter for 'Inductive'
--
-- Given (one or more) empty wallets, evaluate an 'Inductive' wallet, checking
-- the given property at each step.
interpret :: forall ws h a.
             Wallets ws h a                         -- ^ Empty wallet
          -> (Wallets ws h a -> Validated Text ())  -- ^ Predicate to check
          -> Inductive h a -> Validated Text (Wallets ws h a)
interpret es p = go
  where
    go :: Inductive h a -> Validated Text (Wallets ws h a)
    go WalletEmpty      = verify es
    go (ApplyBlock b w) = go w >>= verify . walletsMap (applyBlock b)
    go (NewPending t w) = go w >>= verify . walletsMap (newPending' t)

    verify :: Wallets ws h a -> Validated Text (Wallets ws h a)
    verify ws = p ws >> return ws

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Wallet invariant
--
-- A wallet invariant is a property that is preserved by the fundamental
-- wallet operations, as defined by the 'IsWallet' type class and the
-- definition of 'Inductive'.
--
-- In order to evaluate the inductive definition we need the empty wallet
-- to be passed as a starting point.
type Invariant h a = Inductive h a -> Validated Text ()

-- | Lift a property of flat wallet values to an invariant over the wallet ops
invariant :: IsWallet w h a => Text -> w h a -> (w h a -> Bool) -> Invariant h a
invariant err e p = void . interpret (One e) p'
  where
    p' (One w) = unless (p w) $ throwError err

{-------------------------------------------------------------------------------
  Specific invariants
-------------------------------------------------------------------------------}

walletInvariants :: IsWallet w h a => w h a -> Invariant h a
walletInvariants e w = sequence_ [
      pendingInUtxo          e w
    , utxoIsOurs             e w
    , changeNotAvailable     e w
    , changeNotInUtxo        e w
    , changeAvailable        e w
    , balanceChangeAvailable e w
    ]

pendingInUtxo :: IsWallet w h a => w h a -> Invariant h a
pendingInUtxo e = invariant "pendingInUtxo" e $ \w ->
    txIns (pending w) `Set.isSubsetOf` utxoDomain (utxo w)

utxoIsOurs :: IsWallet w h a => w h a -> Invariant h a
utxoIsOurs e = invariant "utxoIsOurs" e $ \w ->
    all (isJust . ours w . outAddr) (utxoRange (utxo w))

changeNotAvailable :: IsWallet w h a => w h a -> Invariant h a
changeNotAvailable e = invariant "changeNotAvailable" e $ \w ->
    utxoDomain (change w) `disjoint` utxoDomain (available w)

changeNotInUtxo :: IsWallet w h a => w h a -> Invariant h a
changeNotInUtxo e = invariant "changeNotInUtxo" e $ \w ->
    utxoDomain (change w) `disjoint` utxoDomain (utxo w)

changeAvailable :: IsWallet w h a => w h a -> Invariant h a
changeAvailable e = invariant "changeAvailable" e $ \w ->
    change w `utxoUnion` available w == total w

balanceChangeAvailable :: IsWallet w h a => w h a -> Invariant h a
balanceChangeAvailable e = invariant "balanceChangeAvailable" e $ \w ->
    balance (change w) + balance (available w) == balance (total w)

{-------------------------------------------------------------------------------
  Compare different wallet implementations
-------------------------------------------------------------------------------}

walletEquivalent :: forall w w' h a. (IsWallet w h a, IsWallet w' h a)
                 => w h a -> w' h a -> Invariant h a
walletEquivalent e e' = void . interpret (Two e e') p
  where
    p :: Wallets '[w,w'] h a -> Validated Text ()
    p (Two w w') = sequence_ [
          cmp "pending"          pending
        , cmp "utxo"             utxo
        , cmp "availableBalance" availableBalance
        , cmp "totalBalance"     totalBalance
        , cmp "available"        available
        , cmp "change"           change
        , cmp "total"            total
        ]
      where
        cmp :: Eq b
            => Text
            -> (forall w''. IsWallet w'' h a => w'' h a -> b)
            -> Validated Text ()
        cmp err f = unless (f w == f w') $ throwError err

{-------------------------------------------------------------------------------
  Auxiliary operations
-------------------------------------------------------------------------------}

balance :: Utxo h a -> Value
balance = sum . map outVal . Map.elems . utxoToMap

txIns :: (Hash h a, Foldable f) => f (Transaction h a) -> Set (Input h a)
txIns = Set.unions . map trIns . Fold.toList

txOuts :: (Hash h a, Foldable f) => f (Transaction h a) -> Utxo h a
txOuts = utxoUnions . map trUtxo . Fold.toList

updateUtxo :: forall h a. Hash h a
           => Ours a -> Block h a -> Utxo h a -> Utxo h a
updateUtxo p b = remSpent . addNew
  where
    addNew, remSpent :: Utxo h a -> Utxo h a
    addNew   = utxoUnion (utxoRestrictToOurs p (txOuts b))
    remSpent = utxoRemoveInputs (txIns b)

updatePending :: forall h a. Hash h a => Block h a -> Pending h a -> Pending h a
updatePending b = Set.filter $ \t -> disjoint (trIns t) (txIns b)

utxoRestrictToOurs :: Ours a -> Utxo h a -> Utxo h a
utxoRestrictToOurs p = utxoRestrictToAddr (isJust . p)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- $generation
--
-- The 'Inductive' data type describes a potential history of a wallet's
-- view of an existing blockchain. This means that there are many possible
-- 'Inductive's for any given blockchain -- any set of addresses can belong
-- to the 'Inductive' that the wallet is for, and there are many possible
-- sequences of actions that adequately describe the view of the
-- blockchain.

-- | A monad for generating inductive chains.
newtype InductiveGen h a
    = InductiveGen
    { unInductiveGen :: ReaderT (InductiveCtx h) Gen a
    } deriving (Functor, Applicative, Monad, MonadReader (InductiveCtx h))

runInductiveGen :: FromPreChain h -> InductiveGen h a -> Gen a
runInductiveGen fpc ig = runReaderT (unInductiveGen ig) (initializeCtx fpc)

newtype InductiveCtx h
    = InductiveCtx
    { icFromPreChain :: FromPreChain h
    }

initializeCtx :: FromPreChain h -> InductiveCtx h
initializeCtx fpc@FromPreChain{..} = InductiveCtx{..}
  where
    icFromPreChain = fpc

getFromPreChain :: InductiveGen h (FromPreChain h)
getFromPreChain = asks icFromPreChain

getBlockchain :: InductiveGen h (Chain h Addr)
getBlockchain = fpcChain <$> getFromPreChain

getLedger :: InductiveGen h (Ledger h Addr)
getLedger = fpcLedger <$> getFromPreChain

-- | The 'Inductive' data type is isomorphic to a linked list of this
-- 'Action' type. It is more convenient to operate on this type, as it can
-- vary the sequence representation and reuse sequence functions.
data Action h a
    = ApplyBlock' (Block h a)
    | NewPending' (Transaction h a)

-- | Convert a container of 'Action's into an 'Inductive' wallet.
toInductive :: (Container t, Element t ~ Action h a) => t -> Inductive h a
toInductive = foldl' k WalletEmpty
  where
    k acc (ApplyBlock' a) = ApplyBlock a acc
    k acc (NewPending' a) = NewPending a acc

-- | Given a 'Set' of addresses that will represent the addresses that
-- belong to the generated 'Inductive' wallet and the 'FromPreChain' value
-- that contains the relevant blockchain, this will be able to generate
-- arbitrary views into the blockchain.
genFromBlockchain
    :: Hash h Addr
    => Set Addr
    -> FromPreChain h
    -> Gen (Inductive h Addr)
genFromBlockchain addrs fpc =
    runInductiveGen fpc (genInductiveFor addrs)

-- | Selects a random subset of addresses to be considered from the
-- blockchain in the amount given.
genFromBlockchainPickingAccounts
    :: Hash h Addr
    => Int
    -> FromPreChain h
    -> Gen (Inductive h Addr)
genFromBlockchainPickingAccounts i fpc = do
    let allAddrs = toList (ledgerAddresses (fpcLedger fpc))
        eligibleAddrs = filter (not . isAvvmAddr) allAddrs

    if null eligibleAddrs then
        error
        $ sformat
            ( "No eligible addresses!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show allAddrs))
        else pure ()

    addrs <- Set.fromList <$> sublistN i eligibleAddrs

    if null addrs then
        error
        $ sformat
            ( "No addresses!\n\n"
            % "All addresses: " % build
            ) (intercalate ", " (map show allAddrs))
        else pure ()

    genFromBlockchain addrs fpc

genInductiveFor :: Hash h Addr => Set Addr -> InductiveGen h (Inductive h Addr)
genInductiveFor addrs = do
    chain <- getBlockchain
    intersperseTransactions addrs (chainToApplyBlocks chain)

-- | The first step in converting a 'Chain into an 'Inductive' wallet is
-- to sequence the existing blocks using 'ApplyBlock' constructors.
chainToApplyBlocks :: Chain h a -> [Action h a]
chainToApplyBlocks = toList . map ApplyBlock' . chainBlocks

-- | Once we've created our initial @['Action' h 'Addr']@, we want to
-- insert some 'Transaction's in appropriate locations in the list. There
-- are some properties that the inserted events must satisfy:
--
-- * The transaction must be after all of the blocks that confirm inputs to
--   the transaction.
-- * The transaction must be before the block that confirms it, if any
--   blocks confirm it. It is not necessary that the transaction gets
--   confirmed eventually!
--
-- See Note [Intersperse]
intersperseTransactions
    :: Hash h Addr
    => Set Addr
    -> [Action h Addr]
    -> InductiveGen h (Inductive h Addr)
intersperseTransactions addrs actions = do
    if null addrs then error "what the fuck" else pure ()
    chain <- getBlockchain
    let ourTxns = findOurTransactions addrs chain
    let allTxnCount = length ourTxns

    -- we weight the frequency distribution such that most of the
    -- transactions will be represented by this wallet. this can be
    -- changed or made configurable later.
    --
    -- also, weirdly, sometimes there aren't any transactions on any of the
    -- addresses that belong to us. that seems like an edge case.
    txnToDisperseCount <- if allTxnCount == 0
        then pure 0
        else liftGen
            . frequency
            . zip [1 .. allTxnCount]
            . map pure
            $ [1 .. allTxnCount]

    txnsToDisperse <- liftGen $ sublistN txnToDisperseCount ourTxns

    ledger <- getLedger

    let txnsWithRange =
            mapMaybe
                (\(i, t) -> (,,) t i <$> transactionMaxIndex addrs t chain ledger)
                txnsToDisperse

    txnsWithIndex <-
        forM txnsWithRange $ \(t, hi, lo) ->
            (,) t <$> liftGen (choose (lo, hi))

    pure
        . toInductive
        . conssect
        . foldr
            (\(t, i) -> IntMap.insertWith (<>) i [NewPending' t])
            (dissect actions)
        $ txnsWithIndex

-- | Construct an 'IntMap' consisting of the index of the element in the
-- input list pointing to a singleton list of the element the original
-- list.
dissect :: [a] -> IntMap [a]
dissect = IntMap.fromList . zip [0..] . map pure

-- | Reverse the operation of 'dissect'. Given an 'IntMap' originally
-- representing the original index in the list pointing to the list of new
-- items at that starting index, collapse that into a single list of
-- elements.
conssect :: IntMap [a] -> [a]
conssect = concatMap snd . IntMap.toList

-- | Given a 'Set' of addresses and a 'Chain', this function returns a list
-- of the transactions with outputs belonging to any of the addresses and
-- the index of the block that the transaction is confirmed in.
findOurTransactions :: Ord a => Set a -> Chain h a -> [(Int, Transaction h a)]
findOurTransactions addrs =
    concatMap k . zip [0..] . toList . chainBlocks
  where
    k (i, block) =
        map ((,) i)
            . filter (any ((`Set.member` addrs) . outAddr) . trOuts)
            $ toList block

-- | This function identifies the index of the block that the input was
-- received in the ledger, marking the point at which it may be inserted as
-- a 'NewPending' transaction.
blockReceivedIndex :: Hash h Addr => Input h Addr -> Chain h Addr -> Maybe Int
blockReceivedIndex i =
    List.findIndex (any ((inpTrans i ==) . hash)) . toList . chainBlocks

-- | For each 'Input' in the 'Transaction' that belongs to one of the
-- 'Addr'esses in the 'Set' provided, find the index of the block in the
-- 'Chain' that confirms that 'Input'. Take the maximum index and return
-- that -- that is the earliest this transaction may appear as a pending
-- transaction.
transactionMaxIndex
    :: Hash h Addr
    => Set Addr
    -> Transaction h Addr
    -> Chain h Addr
    -> Ledger h Addr
    -> Maybe Int
transactionMaxIndex addrs txn chain ledger =
    let inps = Set.filter inputInAddrs (trIns txn)
        inputInAddrs i =
            case inpSpentOutput i ledger of
                Just o  -> outAddr o `Set.member` addrs
                Nothing -> False
        indexes = Set.map (\i -> blockReceivedIndex i chain) inps
     in foldl' max Nothing indexes

liftGen :: Gen a -> InductiveGen h a
liftGen = InductiveGen . lift

{- Note [Intersperse]
~~~~~~~~~~~~~~~~~~~~~
Given a list of blocks

> [ applyBlock_0, applyBlock_1, applyBlock_2, applyBlock_3, applyBlock_4 ]

we construct an 'IntMap' out of them where the index in the intmap is the
original index of that block in the chain:

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1]
> , 2 -> [applyBlock_2]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

Then, when we select an index between @lo@ (the latest block to confirm an input
in the transaction) and @hi@ (the index of the block that confirms the
transaction itself), we can 'insertWith' at that index. Suppose we have a
transaction @t@ where the input was provided in block 1 and is confirmed in
block 3. That means we can have @NewPending t@ inserted into either index 1 or
2:

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1] <> [newPending t] = [applyBlock_1, newPending t]
> , 2 -> [applyBlock_2]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

or

> { 0 -> [applyBlock_0]
> , 1 -> [applyBlock_1]
> , 2 -> [applyBlock_2] <> [newPending t] = [applyBlock_2, newPending t]
> , 3 -> [applyBlock_3]
> , 4 -> [applyBlock_4]
> }

Then, when we finally go to 'conssec' the @IntMap [Action h a]@ back into a
@[Action h a]@, we get:

> [ applyBlock_0
> , applyBlock_1
> , applyBlock_2, newPending t
> , applyBlock_3
> , applyBlock_4
> ]
-}

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance (Hash h a, Buildable a) => Buildable (Inductive h a) where
    build WalletEmpty      = "WalletEmpty"
    build (ApplyBlock b n) = bprint ("ApplyBlock (" % build % ") (" % build % ")") b n
    build (NewPending t n) = bprint ("NewPending (" % build % ") (" % build % ")") t n