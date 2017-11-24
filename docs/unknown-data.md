# Unknown data handling

Many types in Cardano-SL are designed in an extensible way. They can
be extended via softfork in future (e. g. new data can be attached or
semantics can be changed). Examples are:
* `TxIn` has two constructors `TxInUtxo` and `TxInUnknown`. The former
  is just a reference to an unspent output, it's the only `TxIn` type
  we use currently. `TxInUnknown` contains arbitrary bytestring as
  payload. Later we may introduce another type of transaction inputs
  (e. g. to redeem fees). Then old software will parse these inputs
  as `TxInUnknown`, but new software will parse them as new type of
  input (and process them appropriately).
* `Attributes` is basically a map from 1-byte integers to arbitrary
  values. Some values can be parsed into known data types, other
  values are treated as _unparsed fields_. `Attributes` are part of
  various data types, e. g. `Tx`. In version 0 `Tx` always has empty
  attributes. But in version 1 we may add more data to `Tx` (put it
  into `Attributes`). In this case new software will parse this data
  and old software will treat it as unparsed fields.
* Unknown address type. Each valid address in Cardano-SL has a
  type. It can be one of fixed address types or unknown address type
  which can't be interpreted by current version of software.

There are few more examples, but these three should demonstrate the
general idea. If we encounter unknown data (unparsed fields, unknown
tx input type, unknown address type, etc.) in a block, there are two
possible behaviours:
1. Consider such block invalid.
2. Do as many checks as we can and ignore checks which can't be done
   because data is unknown. This behaviour depends on which type of
   data we are processing, each particular case is described in more
   details below.

The behaviour depends on two protocol versions: version used by this
software and last adopted version. We verify that data in blocks is
known if protocol version used by this software is greater than or
equal to the adopted version. That's because in this case:

1. Authors of this software are aware of the adopted version.
2. Each issued block must be formed with respect to adopted version.

Comparison is quite tricky here. Table below demonstrates it.

| Our   | Adopted | Check? |
| ----- | ------- | ------ |
| 1.2.3 |  1.2.3  | Yes    |
| 1.2.3 |  1.2.4  | No     |
| 1.2.3 |  1.2.2  | No     |
| 1.2.3 |  1.3.2  | No     |
| 1.2.3 |  1.1.1  | Yes    |
| 2.2.8 |  1.9.9  | Yes    |

If `(major, minor)` of our version is greater than of adopted
one, then check is certainly done. If it's equal, then check is
done only if `alt` component is the same as adopted one. In
other cases (i. e. when our `(major, minor)` is less than from
adopted version) check is not done.

Note: when we say that attributes must be known, it means that
unparsed fields must be empty.

## verifyAllIsKnown flag

Let's assume that version comparison as described above
Let's denote flag `verifyAllIsKnown` and assign it value:
* `True` if check is to be done, i.e. only known data should be considered valid (unknown data is prohibitied)
* `False` otherwise
