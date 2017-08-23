# Sets for CBOR

This document specifies a tag for sets in Concise Binary Object Representation (CBOR) [1].

    Tag: 258 (set)
    Data item: array
    Semantics: Mathematical finite set
    Reference: https://github.com/input-output-hk/cardano-sl/tree/master/specs/CBOR_SETS.md
    Contact: Input Output HK <operations@iohk.io>

## Introduction

In mathematics and computer science, a finite set [2] is a well-defined finite collection of distinct objects.
In particular, order has no significance, and duplicates are ignored.

## Semantics

Tag 258 can be applied to a CBOR array data item to indicate that it is a set. Sets should be handled
similarly to CBOR maps: a set that has duplicate items may be well-formed, but it is not valid. Like
CBOR map keys, data items in a set do not need to be of the same type.

The advice from the CBOR specification on map keys [3] also applies to set elements:

The encoding and decoding applications need to agree on what types of items are going to be used in sets.

If multiple types of set items are to be used, consideration should be given to how these types would be
represented in the specific programming environments that are to be used. For example, in some languages,
an item of integer 1 cannot be distinguished from an item of string "1". This means that, if integer
items are used, the simultaneous use of string items that look like numbers needs to be avoided. This
points to the conclusion that set items should be of a single CBOR type.

A CBOR-based protocol should make an intentional decision about what to do when a receiving application
does see duplicate items in a set. For consistency, this should usually be the same rule as the protocol
uses for duplicate keys in a CBOR map. And as for CBOR maps, the chosen rule should respect the CBOR data
model: it cannot prescribe a specific handling of the entries with the identical items, except that it might
have a rule that having identical items in a set indicates a malformed set and that the decoder has to
stop with an error. Duplicate items are also prohibited by CBOR decoders that are using strict mode [4].

## Rationale

CBOR has a notion of maps, but not of sets. While CBOR is first and foremost about structured data without
too much in the way of semantic hints, there is inherently some associated semantics and that is sometimes
important and useful. In particular the semantics of maps are important in the case of canonical CBOR [5]
(where unique and sorted keys are required). They are also important for interoperability since internal
data structures for maps also enforce unique keys.

Equivalent considerations can apply when using sets. Sets can clearly be represented structurally using
CBOR arrays, but applications have to know from context (e.g. schemas) to treat the value as a set.
In certain use cases it is useful for generic tools that work without schemas to be able to know that
an array should be treated as a set. Use cases include decoding sets in untyped languages, generic
transformations, or query tools.

In particular, for CBOR based protocols using canonical formats, being able to distinguish unordered sets
from ordered sequences can be useful in verifying that representations are canonical, or in testing that
decoders correctly reject non-canonical representations.

## Canonical CBOR

Similarly to maps, sets also have implications for canonical CBOR formats [5].

Like maps, the items in every set must be sorted lowest value to highest. Sorting is performed using the same
rule as for map keys. This may be the rule suggested in the CBOR specification [5] or the protocol may choose
a different rule, but it should use the same rule for set items as for maps keys.

## Examples

Given the following CBOR array data item, in CBOR diagnostic notation:

   [1,2,3]

The equivalent value as a set in CBOR diagnostic notation is

   258([1,2,3])

And its encoding is 0xd9010283010203:

   D9 0102  -- Tag 258
      83    -- Array of length 3
         01 -- 1
         02 -- 2
         03 -- 3

## References

[1] C. Bormann, and P. Hoffman. "Concise Binary Object Representation (CBOR)". RFC 7049, October 2013.
[2] https://en.wikipedia.org/wiki/Set_(mathematics)
[3] https://tools.ietf.org/html/rfc7049#section-3.7
[4] https://tools.ietf.org/html/rfc7049#section-3.10
[5] https://tools.ietf.org/html/rfc7049#section-3.9

## Authors

Duncan  Coutts    <duncan@well-typed.com>
Alfredo Di Napoli <alfredo.dinapoli@iohk.io>
Artyom  Kazak     <yom@artyom.me>
