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
