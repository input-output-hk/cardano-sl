0.14.2
======

 - createCheckpoint now cuts a new events file (bug #74)

0.14.1
======

 - fix bug in archiveLog that resulted in logs being moved prematurely (bug #22)
 - tweaks for GHC 8.0.x, template-haskell 2.11.x
 - fix compilation of benchmarks

0.14.0
======

 - fixes for cereal 0.5 while maintaining cereal 0.4
   compatibility. IMPORTANT: cereal 0.5 / safecopy 0.9 change the
   serialization format of Float/Double. Migration should be performed
   automatically on old data. However, you should be aware that once
   you upgrade to safecopy 0.9 / cereal 0.5, your data will be
   migrated and not readable by older versions of your application
   which are compiled against safecopy 0.8 / cereal 0.4.

 - additional fixes for TH and kinded type variables
   [https://github.com/acid-state/acid-state/pull/56](https://github.com/acid-state/acid-state/pull/56)
