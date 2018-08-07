# New Package Dependency Policy

This is the policy for adding a new external package dependency to the Cardano SL suite of
packages. By external we mean a package that is not part of this repository and not
maintained by IOHK.

1. Whenever a new external dependency is added to an existing Cardano SL package that fact
   should be noted in the Github Pull Request (PR).

2. If the new dependency is already a direct or indirect dependency of an existing
   Cardano SL package that should be noted in the Github PR. Existing dependencies do
   not need further vetting.

3. If the new dependency is not already a direct or indirect dependency of an existing
   Cardano SL package the following information should be noted in the PR:

   * The Hackage entry for the new package.
   * What functionality the new package provides for Cardano SL.
   * The level of risk the new package provides.
   * Does the package use Template Haskell, and what does it use it for?
   * Is the package tied to a specific OS (eg Linux or Mac only)?
   * Does that package have dependencies that are not already used by Cardano SL?
   * Does the package have tests and decent test coverage?
   * Does the package have sufficient documentation?
   * Are the maintainers repsonsive to issues and PRs?
   * Are there competing packages that provide this functionality? If so, please justify
     selecting this package over the others.

Assessing the level of risk should be relatively easy. For example:

1. Is the package only used for testing/benchmarking (low risk).
2. Does the package provide cryptographic functionality or is it network or blockchain related
   (high risk).
3. Is it a widely used, well maintained package from a well known Haskeller or from an someone
   employed by IOHK (low risk).
4. Is the package obscure, not widely used from someone that is not a well known Haskeller (high
   risk).

If anyone is unsure how to assess the above factors they can always ask on the internal IOHK
#sl Slack channel (for IOHK employees) or in a Github issue for people without IOHK Slack
access.
