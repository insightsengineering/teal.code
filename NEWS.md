# teal.code 0.4.1.9000

# New features

* `@code` slot in `qenv` object is now a `character` (previously it was `expression`)
* `get_code()` is extended by `names` parameter that allows to extract the code just for a
specific object
* you can now specify `# @effect object_name` comment tag at the end of the line for the `character` code input in
`new_qenv()` and `eval_code()` to specify lines having side-effects on objects, so that they are also returned in 
`get_code()` (when `names` is used)

# teal.code 0.4.1

### Miscellaneous
* Fix NEWS
* Updated usage and installation instructions in `README`.
* Updated phrasing of the `qenv` vignette.
* Specified minimal version of package dependencies.

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
