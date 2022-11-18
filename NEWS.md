# teal.code 0.2.0.9004

### Major breaking change
* `chunks` have now been deprecated and will be removed from the package in a future release. The new `qenv` object should be used instead. See the new `qenv` vignette in the package for further details.

### Enhancements
* Improved output format of warnings in `get_warnings`.

### Miscellaneous
* Silenced deprecation warnings in unit tests.

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
