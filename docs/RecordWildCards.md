# Packages that use the `RecordWildCard` extension

## `auxx` package
| File | Types | Type Definition Location |
| --- | --- | ---|
|src/Plugin.hs|AuxxOptions| auxx/src/AuxxOptions.hs|
|src/Plugin.hs|WithCommandAction|auxx/src/Repl.hs|
|src/Plugin.hs|SendActions|infra/src/Pos/Infra/Communication/Types/Protocol.hs|
|src/Plugin.hs|ConversationActions|networking/src/Node/Conversation.hs|
|src/Lang/Lexer.hs|SourcePos| Megaparsec|
|src/Lang/Lexer.hs|BlockVersion|core/src/Pos/Core/Update/BlockVersion.hs|
|src/Lang/Lexer.hs|SoftwareVersion |core/src/Pos/Core/Update/SoftwareVersion.hs|
|src/Lang/Argument.hs|ArgumentError| Same module|
|src/Lang/Argument.hs|ProcError| Same module|
|src/Lang/DisplayError.hs|TypeError|auxx/src/Lang/Argument.hs|
|src/Lang/DisplayError.hs|ArgumentError|auxx/src/Lang/Argument.hs|
|src/Lang/DisplayError.hs|ProcError|auxx/src/Lang/Argument.hs|
|src/Lang/DisplayError.hs|Report | Earley |
|src/Lang/Interpreter.hs|CommandProc|auxx/src/Lang/Command.hs|
|src/Command/Help.hs|CommandProc|auxx/src/Lang/Command.hs|
|src/Command/Help.hs|UnavailableCommand|auxx/src/Lang/Command.hs|
|src/Command/Update.hs|ProposeUpdateParams|auxx/src/Lang/Value.hs|
|src/Command/Update.hs|ProposeUpdateSystem|auxx/src/Lang/Value.hs|
|src/Command/BlockGen.hs|GenBlocksParams|auxx/src/Lang/Value.hs|
|src/AuxxOptions.hs|AuxxOptions | Same module|
|src/Repl.hs|WithCommandAction| Same module|
|src/Command/Proc.hs|TxOut|core/src/Pos/Core/Txp/TxOutAux.hs|
|src/Command/Proc.hs|SendToAllGenesisParams|auxx/src/Command/Tx.hs|
|src/Command/Proc.hs|BlockVersionModifier|core/src/Pos/Core/Update/BlockVersionModifier.hs |
|src/Command/Proc.hs|ProposeUpdateSystem|auxx/src/Lang/Value.hs|
|src/Command/Proc.hs|SoftwareVersion|core/src/Pos/Core/Update/SoftwareVersion.hs|
|src/Command/Proc.hs|ProposeUpdateParams|auxx/src/Lang/Value.hs|
|src/Command/Proc.hs|GenBlocksParams|auxx/src/Lang/Value.hs|
|src/Command/Proc.hs|AddKeyParams |auxx/src/Lang/Value.hs
|src/Command/Proc.hs|RollbackParams|auxx/src/Lang/Value.hs|
|Main.hs|AuxxOptions |auxx/src/AuxxOptions.hs|
|Main.hs|NodeContext |lib/src/Pos/Context/Context.hs|
|Main.hs|CommonNodeArgs |lib/src/Pos/Client/CLI/NodeOptions.hs|

## `binary` package
### File path prefix: src/Pos/Binary/
| File | Types | Type Definition Location |
| --- | --- | ---|
|Class/TH.hs|DataCon| th-utilities|
|Class/TH.hs|Cons| Same module|
|Class/TH.hs|Field| Same module

### File path prefix: test/Test/Pos/

| File | Types | Type Definition Location |
| --- | --- | ---|
Binary/Helpers.hs|SizeTestConfig|Same module|

## `chain` package
### File path prefix: src/Pos/Chain/

| File | Types | Type Definition Location |
| --- | --- | ---|
|Update/Configuration.hs|UpdateConfiguration| Same module |
|Update/Poll/Failure.hs|PollVerFailure (PollTipMismatch)| Same module |
|Update/Poll/Types.hs|UndecidedProposalState| Same module |
|Update/BlockVersion.hs|BlockVersionData, BlockVersionModifier| core/src/Pos/Core/Update/BlockVersionModifier.hs |
|Update/Poll/Class.hs|SoftwareVersion | core/src/Pos/Core/Update/SoftwareVersion.hs |
|Update/Poll/Class.hs|UpdateProposal (UnsafeUpdateProposal) | core/src/Pos/Core/Update/Vote.hs |
|Block/Genesis/Instances.hs|GenericBlock b (UnsafeGenericBlock) | chain/src/Pos/Chain/Block/Blockchain.hs |
|Block/Genesis/Instances.hs|GenesisBody | chain/src/Pos/Chain/Block/Genesis/Types.hs |
|Block/Genesis/Types.hs|GenesisBody, GenesisConsensusData | Same module |
|Block/Main/Instances.hs|UnsafeGenericBlock | chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/Main/Instances.hs|MainBody |chain/src/Pos/Chain/Block/Main/Types.hs|
|Block/JsonLog.hs|JLBlock |core/src/Pos/Core/JsonLog/LogEvents.hs|
|Block/Main/Types.hs|MainBody, MainExtraHeaderData, MainProof|Same module|
|Block/Logic/Integrity.hs|VerifyBlockParams, VerifyHeaderParams |Same module|
|Block/Types.hs|Undo|Same module|
|Block/BHelpers.hs|UnsafeGenericBlock |chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/BHelpers.hs|MainBody |chain/src/Pos/Chain/Block/Main/Types.hs|
|Block/BHelpers.hs|GenericBlockHeader b (UnsafeGenericBlockHeader) |chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/BHelpers.hs|MainConsensusData|chain/src/Pos/Chain/Block/Union/Types.hs|
|Block/BHelpers.hs|MainExtraHeaderData |chain/src/Pos/Chain/Block/Main/Types.hs|
|Delegation/Types.hs|DlgUndo|chain/src/Pos/Chain/Delegation/Types.hs|
|Block/Blockchain.hs|GenericBlockHeader b (UnsafeGenericBlockHeader), GenericBlock b (UnsafeGenericBlock) | Same module
|Block/Union/Types.hs|GenericBlockHeader b (UnsafeGenericBlockHeader) |chain/src/Pos/Chain/Block/Blockchain.hs|
|Block/Union/Types.hs|GenesisConsensusData |chain/src/Pos/Chain/Block/Genesis/Types.hs|
|Block/Union/Types.hs|MainBody|chain/src/Pos/Chain/Block/Main/Types.hs|
|Block/Union/Types.hs|MainConsensusData |Same module|
|Txp/Toil/Stakes.hs|UnsafeTx |core/src/Pos/Core/Txp/Tx.hs|
|Txp/Base.hs|TxOutAux |core/src/Pos/Core/Txp/TxOutAux.hs|
|Txp/Base.hs|TxOut |core/src/Pos/Core/Txp/Tx.hs|
|Txp/Base.hs|UnsafeTxPayload |core/src/Pos/Core/Txp/TxPayload.hs|
|Txp/Toil/Logic.hs|VerifyTxUtxoRes |chain/src/Pos/Chain/Txp/Toil/Utxo/Functions.hs|
|Txp/Toil/Logic.hs|BlockVersionData |core/src/Pos/Core/Update/BlockVersionData.hs|
|Txp/Toil/Logic.hs|TxAux |core/src/Pos/Core/Txp/TxAux.hs|
|Txp/Toil/Logic.hs|AddrAttributes |core/src/Pos/Core/Common/AddrAttributes.hs|
|Security/Util.hs|SecurityParams|chain/src/Pos/Chain/Security/Params.hs|
|Txp/Toil/Failure.hs|TxOut |core/src/Pos/Core/Txp/Tx.hs|
|Ssc/Shares.hs|Commitment |core/src/Pos/Core/Ssc/Commitment.hs|
|Txp/Topsort.hs|TxAux |core/src/Pos/Core/Txp/TxAux.hs|
|Txp/Toil/Utxo/Functions.hs|VTxContext | Same module|
|Txp/Toil/Utxo/Functions.hs|TxOut, UnsafeTx |core/src/Pos/Core/Txp/Tx.hs|
|Txp/Toil/Utxo/Functions.hs|TxAux |core/src/Pos/Core/Txp/TxAux.hs|
|Ssc/Toss/Logic.hs|TossModifier |chain/src/Pos/Chain/Ssc/Toss/Types.hs|
|Ssc/Base.hs|Commitment |core/src/Pos/Core/Ssc/Commitment.hs|
|Ssc/Error/Verify.hs|SscVerifyError (TossInternalError)| Same module|
|Ssc/Seed.hs|Commitment|core/src/Pos/Core/Ssc/Commitment.hs|
|Ssc/Types.hs|SscGlobalState, SscParams |Same module|
|Ssc/VssCertData.hs|VssCertData |Same module|
|Ssc/Toss/Base.hs|Commitment|core/src/Pos/Core/Ssc/Commitment.hs|
|bench/block-bench.hs|TestSubject| Same module|

### File path prefix: test/Test/Pos/Chain

| File | Types | Type Definition Location |
| --- | --- | ---|
|Txp/CoreSpec.hs|UnsafeTx | core/src/Pos/Core/Txp/TxPayload.hs|
|Ssc/Arbitrary.hs|Commitment | core/src/Pos/Core/Ssc/Commitment.hs|
|Ssc/Arbitrary.hs|SlotId|core/src/Pos/Core/Slotting/SlotId.hs|
|Block/Arbitrary.hs|MainProof |src/Pos/Chain/Block/Main/Types.hs|
|Block/Arbitrary.hs|BodyDependsOnSlot | Same module|
|Block/Arbitrary.hs|SlotId|core/src/Pos/Core/Slotting/SlotId.hs|
|Txp/Toil/UtxoSpec.hs|UnsafeTx|core/src/Pos/Core/Txp/Tx.hs|
|Txp/Toil/UtxoSpec.hs|TxOutAux|core/src/Pos/Core/Txp/TxOutAux.hs|
|Txp/Toil/UtxoSpec.hs|ToilVerFailure (ToilWitnessDoesntMatch)|chain/src/Pos/Chain/Txp/Toil/Failure.hs|

