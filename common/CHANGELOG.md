# Revision history for ghc-debug-common

## 0.7.0.0 -- 2025-05-20

* Fix decoding of BCO bitmap field (fixes a runtime crash in projects which use Template Haskell)
* Fix build with 9.10 and 9.12

## 0.6.0.0 -- 2024-04-10

* Support for decoding profiled RTS
* Remove dependency on ghc-heap, all closures are decoded natively without
  any dependency on the compiler used to build ghc-debug.

## 0.5.0.0 -- 2023-06-06

* Bump to keep in sync with other libraries

## 0.4.0.0 -- 2022-12-14

* Add RequestSRT to allow support for tracing SRTs

## 0.3.0.0 -- 2022-10-06

* Support changes to TSO decoding on GHC HEAD
* Improvements to ARR_WORDS decoding
* Improvements to WeakPtr decoding.

## 0.2.1.0 -- 2022-05-06

* Internal refactoring to give better error message on decoding failure

## 0.2.0.0 -- 2021-12-06

* Second version

## 0.1.0.0 -- 2021-06-14

* First version.
