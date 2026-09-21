# Resubmission qol 1.3.6
Last CRAN release was on 20.09.2026.

### New functionality

* `macro()`: If a non character variable is passed it is now converted to character instead of aborting and throwing an error.
* `transpose_plus()`: In a wide to long transposition the `values` parameter can now take in a named list which carries custom variable expressions for the generated id variable.
* `multi_join()`: When joining multiple data frames on different variable names it is now possible to pass in a list of vectors for the first `on` list entry to enable joining each data frame on the first one on different variable names.

### Fixed

* `compute.()`: Some custom functions didn't work consistently in different situations. This is fixed now.


## R CMD check results

0 errors | 0 warnings | 0 note
