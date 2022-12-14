# Revision history for ghc-debug-client

## 0.4 -- 2022-12-14

* Add support for tracing SRTs. This is quite an invasive change which adds a new
  pointer type to the DebugClosure type. This change is reflected in the API for
  parTrace and traceFrom.

* The `Quadtraverse` abstraction is generalised to `Quintraverse` to account for
  this new type parameter.

## 0.3 -- 2022-10-06

* Abstract away tracing functions to allow configuration of progress reporting.
* Add stringAnalysis and arrWordsAnalysis in GHC.Debug.Strings
* Make block decoding more robust if the cache lookup fails for some reason.
* Fix bug in snapshots where we weren't storing stack frame source locations or
  version.

## 0.2.1.0 -- 2022-05-06

* Fix findRetainersOfConstructorExact

## 0.2.0.0 -- 2021-12-06

* Second version.

## 0.1.0.0 -- 2021-06-14

* First version.
