/*
An overlay function providing cardano-sl packages up to cardano-sl the library.
*/
self: super:
{ cardano-sl-util = self.callPackage ./util/cardano-sl-util.nix {};
  cardano-sl-util-test = self.callPackage ./util/test/cardano-sl-util-test.nix {};
  cardano-sl-binary = self.callPackage ./binary/cardano-sl-binary.nix {};
  cardano-sl-binary-test = self.callPackage ./binary/test/cardano-sl-binary-test.nix {};
  cardano-sl-core = self.callPackage ./core/cardano-sl-core.nix {};
  cardano-sl-core-test = self.callPackage ./core/test/cardano-sl-core-test.nix {};
  cardano-sl-chain = self.callPackage ./chain/cardano-sl-chain.nix {};
  cardano-sl-chain-test = self.callPackage ./chain/test/cardano-sl-chain-test.nix {};
  cardano-sl-crypto = self.callPackage ./crypto/cardano-sl-crypto.nix {};
  cardano-sl-crypto-test = self.callPackage ./crypto/test/cardano-sl-crypto-test.nix {};
  cardano-sl-db = self.callPackage ./db/cardano-sl-db.nix {};
  cardano-sl-db-test = self.callPackage ./db/test/cardano-sl-db-test.nix {};
  cardano-sl-networking = self.callPackage ./networking/cardano-sl-networking.nix {};
  cardano-sl-infra = self.callPackage ./infra/cardano-sl-infra.nix {};
  cardano-sl-infra-test = self.callPackage ./infra/test/cardano-sl-infra-test.nix {};
  cardano-sl = self.callPackage ./lib/cardano-sl.nix {};
  /* TODO the packages which depend upon cardano-sl. */
}
