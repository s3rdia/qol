# Resubmission qol 1.2.2
Last CRAN release was on 11.02.2026.

### New functions

* `round_values()`: Rounds values according to the round half up rule.

### New functionality

* `any_table()`: Added `row_pct` and `col_pct` keywords to `pct_group` parameter. With this the function can calculate total percentages for rows and columns regardless of the respective other dimension.
* `rename_multi()`: Variable names can now be passed without quotation marks.
* `any_table()`: Percentages based on single formatted variable expressions can now be computed with the `pct_value` parameter.
* `any_table()`: If the `pct_value or `pct_group` parameters are used but are not part of the statistics parameter, they will be automatically added to statistics.

### Changed functionality

* `frequencies()`, `crosstabs()`, `any_table()`, `content_report()`: Now uses round half up rule for rounding values.

### Fixed

* `any_table()`: When there was an NA value in the first table column while the row label column was deleted, the NA symbol wasn't set. This is fixed now.
* `any_table()`: If the custom NA symbol is set to a number, Excels number stored as text error is now ignored.
* `transpose_plus()`: Took out a debug print I forgot in the function.
* `summarise_plus()`: Fixed formats not matching data when computing percentages, if numeric values are stored as character.
* `if.()`, `else_if.()`, `else.()`: When passing a vector to a new variable, the functions don't error any more if there are NA values.

### New Error Checks

* `any_table()`: Added an error check in case all caLculations are done and no value variable was computed.

### Additionally

* `frequencies()`: Fixed two of the examples where `list` was missing in formats parameter.


## R CMD check results

0 errors | 0 warnings | 0 note
