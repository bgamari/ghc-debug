# Revision history for ghc-debug-stub

## 0.7.0.0 -- 2025-05-20

* Compatability with 9.10 and 9.12

## 0.6.0.0 -- 2024-04-10

* Add requests for decoding cost centres and other profiling info
* Update support for GHC as of 9.11.20240410

## 0.5.0.0 -- 2023-06-06

* Add support for debugging over a TCP socket (`withGhcDebugTCP`)

## 0.4.0.0 -- 2022-12-14

* Fix compatability with HEAD/9.4/9.2
* Support for SRT requests

## 0.3.0.0 -- 2022-10-06

* Allow clients to reconnect multiple times to debuggee (Finley McIlwaine)
* Fix initialisation bugs due to uninitialised len (Teo Camarasu)

## 0.2.1.0 -- 2022-05-06

* Fix bug with write_block on BF_COMPACT closures

## 0.2.0.0 -- 2021-12-06

* Second version

## 0.1.0.0 -- 2021-06-14

* First version
