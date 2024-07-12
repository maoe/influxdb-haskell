# Revision history for influxdb

## v1.9.3.2 - 2024-07-12

* Allow newer dependencies ([101](https://github.com/maoe/influxdb-haskell/pull/101))

## v1.9.3.1 - 2024-03-15

* Support GHC 9.8 ([#98](https://github.com/maoe/influxdb-haskell/pull/98))
* Allow latest time package ([#99](https://github.com/maoe/influxdb-haskell/pull/99))

## v1.9.3 - 2023-06-29

* Mitigate InfluxDB compatibility problems ([#94](https://github.com/maoe/influxdb-haskell/pull/94))
* Support GHC 9.4 and 9.6 ([#95](https://github.com/maoe/influxdb-haskell/pull/95) and [#97](https://github.com/maoe/influxdb-haskell/pull/97))
* Revitalize CI ([#96](https://github.com/maoe/influxdb-haskell/pull/96))

## v1.9.2.2 - 2021-11-21

* Update dependencies

## v1.9.2.1 - 2021-10-20

* Add support for aeson 2.0 ([#89](https://github.com/maoe/influxdb-haskell/pull/89))

## v1.9.2 - 2021-09-08

* Derive Show for Line ([#87](https://github.com/maoe/influxdb-haskell/pull/87))
* Relax upper version bound for time ([#88](https://github.com/maoe/influxdb-haskell/pull/88))

## v1.9.1.2 - 2021-03-25

* Relax upper version bound for attoparsec

## v1.9.1.1 - 2021-03-12

* Support GHC 9.0.1 ([#85](https://github.com/maoe/influxdb-haskell/pull/85))

## v1.9.1 - 2021-02-21

* Show error on the "impossible" path in writeByteString ([#82](https://github.com/maoe/influxdb-haskell/pull/82))
* Relax upper version bounds for lens, time, doctest, and bytestring
* Switch from Travis CI to GitHub Actions ([#84](https://github.com/maoe/influxdb-haskell/pull/84))

## v1.9.0 - 2020-07-18

* Fix `Ignore` and `Empty` to replace the `QueryResults` instance for `Void`. The instance has been deprecated.
* Remove the deprecated `parseResults` method in `QueryResults`.
* Add the `coerceDecoder` method in `QueryResults`.
* Drop support for GHC 8.2 and older because of the use of `EmptyDataDeriving`.
* Update doctest comments with `TypeApplications`.

## v1.8.0 - 2020-06-19

This release reworked the `QueryResuls` type class. There are some breaking changes:

* `parseResults` has been deprecated. `QueryResults` has now `parseMeasurement` method.
* `Decoder` has been monomorphized so that it can be used with lens. The original `Decoder` type has been renamed to `SomeDecoder`.
* `QueryParams` has now `decoder` field.
* `parseResults` and `parseResultsWith` had been using `lenientDecoder` and it caused some unintuitive behavior ([#64](https://github.com/maoe/influxdb-haskell/issues/64), [#66](https://github.com/maoe/influxdb-haskell/issues/66)). Now they use `strictDecoder` instead.
* `parseErrorObject` now doesn't fail. It returns the error message of a response.
* `parseQueryField` which has been deprecated is now deleted.
* `QueryResults` instance for `ShowSeries` was broken. This is fixed.
* The constructor of `Decoder`, `parseResultsWith`, and `parseResultsWithDecoder` have been hidden from the top-level module. They're still available from `Database.InfluxDB.JSON`.

See [#68](https://github.com/maoe/influxdb-haskell/pull/68/files) for how to migrate your code from v1.7.x to v1.8.x.

## v1.7.1.6 - 2020-06-03

* Relax upper version bound for doctest

## v1.7.1.5 - 2020-05-27

* Relax upper version bound for http-client

## v1.7.1.4 - 2020-05-27

* Relax upper version bound for aeson
* Fix a GHC warning

## v1.7.1.3 - 2020-04-03

* Relax upper version bound for base to support GHC 8.10.1

## v1.7.1.2 - 2020-02-08

* Relax upper version bound for lens
* Fix documentation bugs
* Extend doctests
* Test with GHC 8.8.2

## v1.7.1.1 - 2019-09-09

* Relax upper version bound for lens

## v1.7.1 - 2019-07-19

* Escape backslashes when encoding `Line`s (#75)

## v1.7.0 - 2019-05-03

* Support GHC 8.8.1-alpha1
  * The types of `getField` and `getTag` have changed
* Relax upper version bounds for clock and network

## v1.6.1.3 - 2019-03-26

* Drop unused dependency on QuickCheck

## v1.6.1.2 - 2019-01-21

* Relax upper version bound for network

## v1.6.1.1 - 2019-01-10

* Relax upper version bound for http-client

## v1.6.1 - 2018-11-20

* Add secureServer smart constructor for Server
* Test with InfluxDB 1.7.1 and newer GHCs including 8.6.2
* Enhace Haddock comments

## v1.6.0.9 - 2018-09-11

* Relax upper version bound for network

## v1.6.0.8 - 2018-09-04

* Relax upper version bound for QuickCheck

## v1.6.0.7 - 2018-07-23

* Relax upper version bound for base to support GHC 8.6 (#69)

## v1.6.0.6 - 2018-07-07

* Relax upper version bound for lens

## v1.6.0.5 - 2018-06-25

* Relax upper version bound for doctest

## v1.6.0.4 - 2018-06-18

* Relax upper version bound for containers

## v1.6.0.3 - 2018-06-11

* Relax upper version bound for aeson

## v1.6.0.2 - 2018-04-29

* Relax upper version bound for network

## v1.6.0.1 - 2018-04-20

* Relax upper version bound for foldl

## v1.6.0 - 2018-04-14

This release includes a few significant breaking changes.

* Deprecate the confusing parseQueryField and re-export parseJSON instead
* Rewrite the QueryResults instances for tuples
* Add Timestamp instance for TimeSpec (#59)
* Extend haddock comments

## v1.5.2 - 2018-04-11

* Export parseResultsWithDecoder, Decoder, lenientDecoder and strictDecoder from Database.InfluxDB
* Extend haddock comments

## v1.5.1 - 2018-03-29

* Add basic auth support for query (#58)

## v1.5.0 - 2018-03-15

* Change UnexpectedResponse constructor to include the request and throw it in place of UserError in query/write/manage
* Relax upper version bound for doctest
* Extend Haddock comments in Database.InfluxDB.Line

The first item is a breaking change.

## v1.4.0 - 2018-03-13

* Implement proper escaping/quoting for queries ([#54](https://github.com/maoe/influxdb-haskell/pull/54))
* Relax upper version bound for aeson
* Test against InfluxDB 1.5

## v1.3.0.1 - 2018-03-06

* Relax upper version bounds for doctest and QuickCheck

## v1.3.0 - 2018-03-05

* Relax upper version bound for base ([#51](https://github.com/maoe/influxdb-haskell/pull/51))
* Implement proper escaping and quoting for special characters ([#51](https://github.com/maoe/influxdb-haskell/pull/51), [#52](https://github.com/maoe/influxdb-haskell/pull/52))
  * Introduce the Measurement type and accompanying functions
* Fix a bug in the HTTP writer where the precision parameter is ignored when constructing requests
* Some minor doctest fixes

## v1.2.2.3 - 2018-01-30

* Relax upper version bounds for http-types, lens and time

## v1.2.2.2 - 2017-11-30

* Relax upper version bounds for http-types and tasty-hunit

## v1.2.2.1 - 2017-11-30

* Relax upper version bound for http-types

## v1.2.2 - 2017-06-26

* A couple of documentation fixes
* Add `Ord` instance for `Server`

## v1.2.1 - 2017-06-19

* Export `formatDatabase` and `formatKey` from `Database.InfluxDB` for convenience

## v1.2.0 - 2017-06-19

There are a lot of breaking changes in this release. The API has been cleaned up
and a lot of Haddock comments are added extensively.

* The `FieldVal` has been renamed to `Field` which takes `Nullability` as a type parameter.
* `localServer` has been renamed to `defaultServer`
* Some constructors in `InfluxException` have been renamed
  * `BadRequest` to `ClientError`
  * `IllformedJSON` to `UnexpectedResponse`
* Added a smart constructor `credentials` for `Credentials`
* Dropped `parseTimestamp` and added `parseUTCTime`
* `ping` handles timeout proerply and throws `InfluxException` on failure
* `PingResult` has been renamed to `Pong` and is now an abstract data type.
* `PingParams` has been turned into an abstract data type.
* `waitForLeader` has been renamed to `timeout`.
* `parsekey` has been removed. `getField` and `parseQueryField` can be used instead.
* Drop support for `http-client < 0.5`

## v1.1.2.2 - 2017-05-31

* Relax upper version bound for foldl

## v1.1.2.1 - 2017-05-02

* Relax version bounds for base and aeson

## v1.1.2 - 2017-04-10

* Tighten lower version bound for base [#43](https://github.com/maoe/influxdb-haskell/issues/43)
* Add `Database.InfluxDB.Format.{string,byteString8}`

## v1.1.1 - 2017-03-29

* Relax unnecessary Traversable constraints to Foldable

## v1.1.0 - 2017-03-23

* Handle empty "values" in parseSeriesBody

## v1.0.0 - 2017-03-03

The library was completely rewritten and support for older InfluxDB has been dropped.

* Support for InfluxDB 1.2

## v0.10.0 - 2016-05-17

* Fix a typo in a Haddock comment (#28)
* Drop support for retry < 0.7
* Add stack.yml
* Add support for GHC 8.0.1 (#29)

## v0.9.1.3 - 2015-06-02

* Relax upper bound for aeson

## v0.9.1.2 - 2015-05-15

* Relax upper bound for attoparsec

## v0.9.1.1 - 2015-03-07

* Allow retry >= 0.6 && < 0.7

## v0.9.1 - 2015-03-07

* Add `writeSeriesData`
* Relax upper version bound for exceptions
* Drop support for old retry package
  * retry < 0.6 had an unexpected behavior wrt exception masking state (https://github.com/Soostone/retry/pull/12)

## v0.9.0.1 - 2015-01-06

* Support for GHC 7.10.1

## v0.9.0 - 2014-11-27

* The `Value` parsers (accidentally) could throw exceptions. It's fixed now.
* Add `fromSeriesData_` which discards parsing errors and returns only successful data
* Remove `listInterfaces`

## v0.8.0 - 2014-11-07

* Retry on connection failure and response timeout in addition to IOException
  * Note that this may break existing code silently

## v0.7.1.1 - 2014-09-19

* Relax upper bound for http-client
* Set upper bounds for some packages

## v0.7.1 - 2014-09-16

* Add more lenses

## v0.7.0 - 2014-09-12

* Support for influxdb v0.8 (#15)
  * Add shard spaces API
  * Add `configureDatabase`
* Add Typeable and Generic instances where missing
* Remove unused `ScheduledDelete` type

## v0.6.0 - 2014-08-19

* Support for retry-0.5 (#16)
  * `newServerPoolWithRetrySettings` has been renamed to `newServerPoolWithRetryPolicy`
    * `serverRetrySettings` field in `ServerPool` has been renamed to `serverRetryPolicy`
* Support for network-uri (#17)

## v0.5.1 - 2014-07-18

* Export `InfluxException` from `Database.InfluxDB`

## v0.5.0 - 2014-07-18

* Add `InfluxException` type and use it when decoding JSON or SeriesData (#12)
* New API
  * `ping`
  * `listInterfaces`
  * `isInSync`
* BUGFIX: Fix `when expecting a Float, encountered Int instead` error (#14)

## v0.4.2 - 2014-06-06

* Export `newServerPoolWithRetrySettings` from `Database.InfluxDB`

## v0.4.1 - 2014-06-05

* Make retry settings configurable (#5)

## v0.4.0 - 2014-06-05

* Remove `databaseReplicationFactor` field from `Database` type

## v0.3.0.1 - 2014-06-04

* Allow exceptions-0.6 (@JohnLato)

## v0.3.0 - 2014-06-03

* Support for InfluxDB v0.7
  * Renamed `username` field for `/cluster_admins` to `user`
  * No support for the old field name

## v0.2.2 - 2014-05-08

* Support for retry-0.4
* Add deleteSeries
* Add authenticateClusterAdmin and authenticateDatabaseUser

## v0.2.1.1 - 2014-04-22

* Bug fix: Treat as integer if base10Exponent is positive

## v0.2.1 - 2014-04-22

* Add `stripPrefixSnake`

## v0.2.0.1 - 2014-04-17

* Drop unnecessary dependency on `scientific` when using old `aeson`.

## v0.2.0 - 2014-04-16

* Add more `FromValue` instances
* Add `(.:?)` and `(.!=)`
* Add `deriveSeriesData` and some variants
* Add left folds for `Stream` type

## v0.1.0.1 - 2014-04-07

* Support for older aeson
* Textual paramters in some functions for convenience
* A lot of bug fixes

## v0.0.0 - 2014-04-03

* Initial release
