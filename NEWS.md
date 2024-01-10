# teal.code 0.5.0

### Breaking Change

* `qenv` objects should now be created with `qenv()` rather than `new_qenv()`. The new constructor always creates an empty object. `new_qenv` is now deprecated.

### Miscellaneous

* Exported the `qenv` class from the package.
* The `@code` field in the `qenv` class now holds `character`, not `expression`.
* The `get_code` method  returns a single concatenated string of the code.
* Added `within` support for `qenv.error` class.
* Added `get_env` method that allows to extract environment stored in `qenv@env` slot.

# teal.code 0.4.1

### Miscellaneous
* Fix NEWS
* Updated usage and installation instructions in `README`.
* Updated phrasing of the `qenv` vignette.
* Specified minimal version of package dependencies.
* Added `within` method for `qenv` for convenient passing of inline code to `eval_code`.

# teal.code 0.4.0

### Breaking Change
* `chunks` have been removed. The new `qenv` object should be used instead. See the new `qenv` vignette in the package for further details.

### Miscellaneous
* `dev_suppress` has been added to suppress rendering of plots on IDE.

# teal.code 0.3.0

### Major breaking change
* `chunks` have now been deprecated and will be removed from the package in a future release. The new `qenv` object should be used instead. See the new `qenv` vignette in the package for further details.

### New features
* Added `concat` method to the `qenv` to offer the concatenate functionality.

# teal.code 0.2.0

### Miscellaneous
* Removed the `%<chunk%` operator. Please use `chunks_push` instead.

# teal.code 0.1.1

### Enhancements
* New wrapper function `chunks_deep_clone` to make a deep (i.e. completely independent) copy of a `chunks` objects.
* Added a new wrapper `chunks_new` for `chunks` initialization.

### Miscellaneous
* Added a template to the `pkgdown` site.
* Added a vignette to explain the advanced features of the `chunks` object.

# teal.code 0.1.0

* Initial release of `teal.code`, a package for code storage and execution class for teal applications.
