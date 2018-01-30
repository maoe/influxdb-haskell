## v1.2.2.3 - 2018-01-30

* Relax upper version bounds for http-types and tasty-hunit

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
