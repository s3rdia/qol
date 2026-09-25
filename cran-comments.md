# Resubmission qol 1.3.6
Last CRAN release was on 20.09.2026.

### New functionality

* `macro()`: If a non character variable is passed it is now converted to character instead of aborting and throwing an error.
* `transpose_plus()`: In a wide to long transposition the `values` parameter can now take in a named list which carries custom variable expressions for the generated id variable.
* `multi_join()`: When joining multiple data frames on different variable names it is now possible to pass in a list of vectors for the first `on` list entry to enable joining each data frame on the first one on different variable names.
* `drop_type_vars()`: When passing in a list of data frames, the function now removes the variables `TYPE`, `TYPE_NR` and `DEPTH` from all data frames within the list. 
* `retain_stat()`: Can now generate cumulative values.
* `multi_join()`: Single variables passed to the `on` parameter can now be written without quotation marks.
* `any_table()`, `excel_output_style()`: The new `block_borders` style parameter enables to draw borders between statistic blocks within the table area.

### Fixed

* `compute.()`: Some custom functions didn't work consistently in different situations. This is fixed now.
* Most of the functions couldn't resolve custom parameters passed on from a wrapper function by themselves. This is hopefully fixed.
* `compute.()`: Doesn't error anymore when calculations are written on multiple lines.
* `any_table()`: Variable labels weren't set when the variable name contained a `statistics` keyword. This is fixed now.
* `any_table()`: When working with pre summarised data then now the value variables appear in user provided order instead of order of appearance within the given data frame.
* `running_number()`: Now considers all variables when passing a vector of variable names into `by`.
* `if.()`, `else_if.()`, `ifelse_multi()`, `where.()`: Don't error anymore on parsing variable names ending in a number.
* `summarise_plus()`: In case the function was used inside a custom function, percentiles weren't detected as `statistics`. This is fixed now.


## R CMD check results

0 errors | 0 warnings | 0 note
