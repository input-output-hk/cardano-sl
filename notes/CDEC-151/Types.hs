{-# LANGAUGE GADTSyntax #-}

-- |
-- = Inter-node messages. Those related to block broadcast are omitted.

-- | A single 'Message' type for all inter-node communication.
data Message where
  MessageControl  :: Control -> Message
  MessagePayload  :: Datum -> Message
  MessageAnnounce :: Announce -> Message
  MessageRequest  :: Request -> Message

data Topic where
  TopicBlock      :: Topic
  TopicDelegation :: Topic
  TopicSsc        :: Topic
  TopicTxp        :: Topic
  TopicUpdate     :: Topic

-- | We want to be able to opt-in to certain topics so that, for instance, an
-- end-user edge node which has delegated all of its stake does not receive
-- transactions, SSC, or delegation data. They can't be slot leader, so they
-- don't need these things.
data Control where
  ControlKeepalive   :: Control
  -- Encoding/decoding a set: length limit is implicit if we use canonical
  -- CBOR: 5 valid Topic encodings, must come in ascending order, will not
  -- be canonical if there are >= 6 from the 5.
  ControlSubscribe   :: Set Topic -> Control
  ControlUnsubscribe :: Set Topic -> Control

data Datum where
  -- Block omitted, but note that we could potentially save some traffic by
  -- sending the block minus the header, which the other end probably already
  -- knows from a header announcement. Sadly, a block is not a header and body,
  -- it's a header, body, and "extra body data", which you'd think would be part of the body.
  -- Block          :: Block -> Datum
  Txp            :: TxAux -> Datum
  UpdateProposal :: UpdateProposal -> Datum
  UpdateVote     :: UpdateVote -> Datum
  SscCert        :: VssCertificate -> Datum
  SscOpening     :: Opening -> Datum
  -- Current protocol uses
  -- 'InnerSharesMap ~ Map StakeholderId (NonEmpty (AsBinary DecShare))'
  -- but here we take '(StakeholderId, AsBinary DecShare)', which is naturally
  -- limited. An 'InnerSharesMap' can be constructed from 1 or more of these.
  SscShares      :: StakeholderId -> AsBinary DecShare -> Datum
  SscCommitment  :: SignedCommitment -> Datum
  Delegation     :: ProxySKHeavy -> Datum

data Announce where
  AnnounceTxp            :: TxId -> Announce
  AnnounceUpdateProposal :: UpId -> Announce
  AnnounceUpdateVote     :: VoteId -> Announce
  -- These keys identifier the stakeholder, but not the epoch for which it's
  -- relevant.
  -- The signed data in each of these variants of course pins them to an
  -- epoch, but we may want to have that epoch be part of the key here (along
  -- with the 'StakeholderId").
  AnnounceSscCert        :: StakeholderId -> Announce
  AnnounceSscOpening     :: StakeholderId -> Announce
  AnnounceSscShares      :: StakeholderId -> Announce
  AnnounceSscCommitment  :: StakeholderId -> Announce
  -- No delegation announcement, because that's how it is now.
  -- But maybe there ought to be one? How to identify it? Hash of the whole
  -- thing? 

data Request where
  RequestTxp            :: TxId -> Request
  RequestUpdateProposal :: UpId -> Request
  RequestUpdateVote     :: VoteId -> Request
  RequestSscCert        :: StakeholderId -> Request
  RequestSscOpening     :: StakeholderId -> Request
  RequestSscShares      :: StakeholderId -> Request
  RequestSscCommitment  :: StakeholderId -> Request

-- A single bidirectional channel in which each end sends 'Message' should
-- express all that we need.
-- It permits even an edge node to serve as a relay: if it connects to 2
-- addressable nodes, data published by one can be relayed via the edge node
-- to the other.

-- | Inbound / outbound queueing
--
-- Currently the 'Diffusion' type features a set of actions to put data out,
-- but no explicit source of incoming data. Instead, it's expected to use a
-- 'Logic' value which includes callbacks to run on incoming data. This fits
-- with the long-standing "listener" model.
--
-- A simpler model might include a source and sink for relatively high-level
-- messages. This doesn't constitue the whole diffusion layer, which ought to
-- also deal with relaying, subscription states, and keepalive messages.
--
-- Programming against the following type would lend itself well to simulation
-- and testing.

data NetworkIO peer m = NetworkIO
  { nioSource :: m (peer, SourceMessage)
  , nioSink   :: Target peer -> SinkMessage -> m ()
  }

data Target peer where
  Broadcast :: Target peer
  Unicast   :: peer -> Target peer

data SourceMessage where
  Connected     :: SourceMessage
  Disconnected  :: SourceMessage
  SourceMessage :: Message -> SourceMessage

-- A user of 'NetworkIO' cannot explicitly connect to or disconnect from a peer.
-- Peer discovery and delta-Q estimation will do that under-the-hood, but it's
-- easy to believe that some decision about this may require high-level
-- information.
data SinkMessage where
  SinkMessage :: Message -> SinkMessage

-- A 'Diffusion' might look similar to a 'NetworkIO', but with a different sink
-- type, to reflect the 'send*' actions on the current definition.

data Diffusion m = Diffusion
  { -- Corresponds to the 'Logic' 'post*' callbacks.
    dSource :: m Datum
    -- This is always a broadcast. It corresponds to the 'send*' actions.
    -- Transparent to relaying.
  , dSink   :: Datum -> m ()
  }

-- These types are just suggestions for a simplified interface. It's not clear
-- at the moment exactly how blockchain broadcast will work, but in any case it
-- will certainly require access to higher-level information (a database,
-- verification tools). Once we've determined the details of blockchain
-- broadcast and selection can we pin down the 'Diffusion' interface.
