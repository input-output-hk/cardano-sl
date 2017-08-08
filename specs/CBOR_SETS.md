# Sets for CBOR

This document specifies a tag for sets in Concise Binary Object Representation (CBOR) [1].

    Tag: 258
    Data item: array
    Semantics: Set
    Point of contact: Input Output HK <info@iohk.io>
    Description of semantics: https://github.com/input-output-hk/cardano-sl/tree/master/specs/CBOR_SETS.md

## Semantics

Tag 258 can be applied to an array to indicate that the tagged object is a set.

## Rationale

CBOR has no format notion of set, intended as a well-defined collection of distinct objects. However,
application code might need to distinguish between sets and arrays based on different domain-specific requirements.

## References

[1] C. Bormann, and P. Hoffman. "Concise Binary Object Representation (CBOR)". RFC 7049, October 2013.

## Author

Input Output HK <info@iohk.io>
