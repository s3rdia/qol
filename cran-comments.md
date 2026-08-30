# Resubmission qol 1.3.5
Last CRAN release was on 20.08.2026.

### New functionality

* `dummy_data()`: Can now generate a dummy data frame in wide format using the new `wide` parameter.
* `transpose_plus()`: Added `statistics` parameter which enables the function to not only transpose sums but any available `statistic`. Can also take in a named list to output specific stats per variable like in `any_table()` and `summarise_plus()`.

### Changed functionality

* `summarise_plus()`: When only passing `class` variables without `values` and using the default `statistics` then only frequencies will be calculated instead of sums and frequencies, which would be identical.
* `transpose_plus()`: Instead of aborting when no values are passed, the function now generates a variable to output unweighted results.
* `transpose_plus()`: Received a new parameter `summarise` which summarises the data before transposing. This is the default behaviour when using formats, but was not without formats. `summarise` is TRUE by default.

### Fixed

* `combine_into_workbook()`: Fixed table of contents example not working as intended.
* `crosstabs()`: If a vector of variables is provided for `columns` then now the first variable will be picked instead of the second one.
* `else_if()`, `else.()`: When assigning to multiple variables in a do-over-loop situation the functions would only consider the first assignment. This is fixed now.
* `transpose_plus()`: The function had a serious flaw. When transposing multiple variables at once the results were always picked from the all nested results even though they have to be picked from their respective combination. This is fixed now.
* `transpose_plus()`: If an invalid format is passed, this now throws a warning instead of removing all formats silently.
* `remove_stat_extension()`: When a vector was passed only the last element was considered. Now all passed stat extensions are removed.

### Optimization

* `apply_formats()`: Removed unnecessary calculation. Additionally optimzed checking for NA values for discrete formats.
* `summarise_plus()`: With `nesting = "all" or "single` a list of logical vectors containing non NA observations is now computed once before generating all combinations, which allows to remove the individual data frame scanning per combination.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Moved column width and row height adjustments before the background coloring to make it run only over the table span instead of the whole sheet.


## R CMD check results

0 errors | 0 warnings | 0 note
