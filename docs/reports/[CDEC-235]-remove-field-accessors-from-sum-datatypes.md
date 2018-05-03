# [CDEC - 235] - Removing field accessors from sum data types

View the commit here: https://github.com/Jimbo4350/cardano-sl/commit/726467a2811e3eda9b23cabd0b34d32081761dae

## Issue

Record accessors on sum data types can result in code that crashes. For example consider two constructors from the `ToilVerFailure` data type (I have excluded the other data constructors from the `ToilVerFailure` type for the purpose of simplicity):



```
data ToilVerFailure = ToilKnown
                    | ToilTooLargeTx { ttltSize  :: !Byte
                                     , ttltLimit :: !Byte}

testFunc1 :: ToilVerFailure -> HeaderHash
testFunc1 x = ttltSize x

testFunc2 :: ToilVerFailure -> HeaderHash
testFunc2 x = ttltLimit x

```
Aside: The exclaimation in `!Byte` simply forces strictness.

Calling `testFunc1` or `testFunc2` with data constructor `ToilTooLargeTx` will be fine as this constructor possesses the record accessor `ttltSize` and `ttltLimit`. However, calling testFunc1 or testFunc2 with the `ToilKnown` constructor will result in an exception being thrown. This can potentially crash your program at runtime and is dangerous because calling `testFunc1` or `testFunc2` with  `ToilKnown` is type correct.

## How to remove the accessors


We first need to modify the data type as follows:

```
data ToilVerFailure
      = ToilKnown
      | ToilTooLargeTx !Byte !Byte
```
Notice the record accessors `ttltSize` and `ttltLimit` are now gone and all that remains are the two types `!Byte` and `!Byte`. We have removed the record accessor syntax for this particular constructor.

At this point you should run `stack test` in the `cardano-sl/` folder. This will build the libraries, executables, test suites and run the test suites for you. The compiler will let you know if (as a result of your change) there are any type mismatches or errors.

At this point the compiler will point out a few errors all of which should be calls to undefined functions. The undefined functions correspond to the record accessors you removed from the data constructor. Below, I walk through one of those errors using the `verifyGState` function. You can find the function in `cardano-sl/txp/Pos/Txp/Toil/Logic.hs`.

```
verifyGState ::
       BlockVersionData
    -> EpochIndex
    -> TxAux
    -> VerifyTxUtxoRes
    -> Either ToilVerFailure ()
verifyGState bvd@BlockVersionData {..} curEpoch txAux vtur = do
    verifyBootEra bvd curEpoch txAux
    let txFeeMB = vturFee vtur
    let txSize = biSize txAux
    let limit = bvdMaxTxSize
    unless (isRedeemTx $ vturUndo vtur) $ whenJust txFeeMB $ \txFee ->
        verifyTxFeePolicy txFee bvdTxFeePolicy txSize
    when (txSize > limit) $
        throwError ToilTooLargeTx {ttltSize = txSize, ttltLimit = limit}
```

The important part to focus on is the last line of the `verifyGState` function which is `throwError ToilTooLargeTx {ttltSize = txSize, ttltLimit = limit}`. The compiler will complain about this line because the record accessors (`ttltSize` and `ttltLimit`) are used to construct the `ToilTooLargeTx` data constructor. However we have just changed our datatype to `ToilTooLargeTx !Byte !Byte` and therefore the record accessors `ttltSize` and `ttltLimit` are undefined. We need to make the following change:

From: `ToilTooLargeTx {ttltSize = txSize, ttltLimit = limit}`

To: `ToilTooLargeTx txSize limit`

NB: `txSize` and `limit` are defined in let clauses directly above and are the
    appropriate type (i.e `!Byte`) because the record accessors simply wrap a
    type with a name.

Now re-run `stack test` in the `cardano-sl/` folder. The error message due to calling undefined functions (`ttltSize` and `ttltLimit`) in the `verifyGState` function should be gone. Repeat this process until there are no errors and the repo passes all tests.



