# Sets for CBOR

This document specifies a tag for sets in Concise Binary Object Representation (CBOR) [1].

    Tag: 258 (set)
    Data item: array
    Semantics: Set
    Reference: https://github.com/input-output-hk/cardano-sl/tree/master/specs/CBOR_SETS.md
    Contact: Input Output HK <operations@iohk.io>

## Introduction

A set [2] is a finite or infinite collection of objects in which order has no significance, and multiplicity
is also ignored. Practically speaking, a set is a well-formed collection of distinct objects, without duplicates.

Tag 258 can be applied to an array to indicate that the tagged object is a set. Sets should be handled
similarly to maps: a set that has duplicate items may be well-formed, but it is not valid. Like
arrays, items in a set don't need to all be of the same type.

## Semantics

The encoding and decoding applications need to agree on what types of items are going to be used in sets.
If multiple types of items are to be used, consideration should be given to how these types would be
represented in the specific programming environments that are to be used. For example, in some languages,
an item of integer 1 cannot be distinguished from an item of string "1". This means that, if integer
items are used, the simultaneous use of string items that look like numbers needs to be avoided. This is
especially important for CBOR decoders that are using strict mode [3].

A CBOR-based protocol should make an intentional decision about what to do when a receiving application
does see multiple identical items in a set.  The resulting rule in the protocol should respect the CBOR
data model: it cannot prescribe a specific handling of the entries with the identical items, except that
it might have a rule that having identical items in a set indicates a malformed set and that the decoder
has to stop with an error. Duplicate items are also prohibited by CBOR decoders that are using strict mode.

## Rationale

CBOR has a notion of maps, but not of sets. While CBOR is first and foremost about structured data without
too much in the way of semantic hints, there is inherently some associated semantics and that is sometimes
important and useful. In particular the semantics of maps are important in the case of canonical CBOR [4]
(where unique and sorted keys are required). They are also important for interoperability since internal
data structures for maps also enforce unique keys.

Equivalent considerations can apply when using sets. Sets can clearly be represented structurally using
CBOR arrays, but applications have to know from context (e.g. schemas) to treat the value as a set.
In certain use cases it is useful for generic tools that work without schemas to be able to know that
an array should be treated as a set. Use cases include deserialising sets in untyped languages,
generic transformations, or query tools.

## Canonical CBOR

Similarly to maps, sets also have implications for canonical CBOR representations. Being able to distinguish
unordered sets from ordered sequences can be useful in verifying that representations are canonical,
or in testing that implementations correctly reject non-canonical representations.

Like maps, the items in every set must be sorted lowest value to highest. Sorting is performed on the bytes
of the representation of the encoded items without paying attention to the 3/5 bit splitting for major types.
The sorting rules are:

*  If two items have different lengths, the shorter one sorts earlier;

*  If two items have the same length, the one with the lower value in (byte-wise) lexical order sorts earlier.

## Examples

Given the following JavaScript array:

   [1,2,3]

The natural encoding of this data structure as a CBOR set would be 0xD9010283010203:

   258([1,2,3])

   D9 0102  -- Tag 258
      83    -- Array of length 3
         01 -- 1
         02 -- 2
         03 -- 3

## References

[1] C. Bormann, and P. Hoffman. "Concise Binary Object Representation (CBOR)". RFC 7049, October 2013.
[2] http://mathworld.wolfram.com/Set.html
[3] https://tools.ietf.org/html/rfc7049#section-3.10
[4] https://tools.ietf.org/html/rfc7049#section-3.9

## Authors

Duncan  Coutts    <duncan@well-typed.com>
Alfredo Di Napoli <alfredo.dinapoli@iohk.io>
Artyom  Kazak     <yom@artyom.me>
