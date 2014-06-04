## v0.3.0.1

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
