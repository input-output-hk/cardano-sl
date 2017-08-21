# Sets for CBOR

This document specifies a tag for sets in Concise Binary Object Representation (CBOR) [1].

    Tag: 258
    Data item: array
    Semantics: Set
    Point of contact: Input Output HK <operations@iohk.io>
    Description of semantics: https://github.com/input-output-hk/cardano-sl/tree/master/specs/CBOR_SETS.md

## Semantics

Tag 258 can be applied to an array to indicate that the tagged object is a set. Sets should be handled
similarly to maps: a set that has duplicate items may be well-formed, but it is not valid. Like
arrays, items in a set don't need to all be of the same type, provided it's possible to compare them without
ambiguities to ensure no duplicates are present. In case ambiguity can't be avoided, items do necessarily
need to all be of the same type.

The encoding and decoding applications need to agree on what types of items are going to be used in sets.
If multiple types of items are to be used, consideration should be given to how these types would be
represented in the specific programming environments that are to be used.  For example, in some languages,
an item of integer 1 cannot be distinguished from an item of string "1". This means that, if integer
items are used, the simultaneous use of string items that look like numbers needs to be avoided.
Again, this leads to the conclusion that items should be of a single CBOR type.

When a CBOR-based protocol does see multiple identical items in a set it should reject the set as invalid.

## Rationale

CBOR has a notion of maps, but not of sets. While CBOR is first and foremost about structured data without
too much in the way of semantic hints, there is inherently some associated semantics and that is sometimes
important and useful. In particular the semantics of maps are important in the case of canonical CBOR [2]
(where unique and sorted keys are required). They are also important for interoperability since internal
data structures for maps also enforce unique keys.

Equivalent considerations can apply when using sets. Sets can clearly be represented structurally using
CBOR arrays, but applications have to know from context (e.g. schemas) to treat the value as a set.
In certain use cases it is useful for generic tools that work without schemas to be able to know that
an array should be treated as a set. Use cases include deserialising sets in untyped languages,
generic transformations, or query tools.

Similarly to maps, sets also have implications for canonical CBOR representations. Being able to distinguish
unordered sets from ordered sequences can be useful in verifying that representations are canonical,
or in testing that implementations correctly reject non-canonical representations.

## Examples

Given the following JavaScript array:

   [1,2,3]

The natural encoding of this data structure as a CBOR Set would be 0xD9010283010203:

   D9 0102  -- Tag 258
      83    -- Array of length 3
         01 -- 1
         02 -- 2
         03 -- 3

## References

[1] C. Bormann, and P. Hoffman. "Concise Binary Object Representation (CBOR)". RFC 7049, October 2013.
[2] https://tools.ietf.org/html/rfc7049#section-3.9

## Authors

Duncan  Coutts    <duncan@well-typed.com>
Alfredo Di Napoli <alfredo.dinapoli@iohk.io>
Artyom  Kazak     <yom@artyom.me>
