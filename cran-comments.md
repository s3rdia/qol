# Resubmission qol 1.3.6
Last CRAN release was on 25.10.2026.

### Changed functionality

* `excel_output_style()`: Reduced all font sizes which were 10 to 9 to.

### Fixed

* `any_table()`: The default `pct_group` now ignores the internal pseudo column variable. With an empty `columns` parameter the percentages are computed within the last real variable instead of every row adding up to 100 %.
* `summarise_plus()`, `any_table()`: The note about missing values being dropped for percentile calculations is now only printed once per value variable instead of once per computed statistic and variable combination.
* `retain_stat()`: Should now work within `compute.()` when using multiple `by` variables.
* `any_table()`: Doesn't error anymore when using the keywords `row_pct` and `col_pct` on their own in the `pct_group` parameter.
* `set_labels()`: When entering NULL the function put out an error instead of resetting the labels. This is fixed now.
* `combine_into_workbook()`: When the `print_miss` parameter is used in one of the tables the respective first sheet of this table series is now used to clone the style from instead of the first sheet of the whole workbook.
* `combine_into_workbook()`: Individual table styles are now fully applied to the respective sheets.

### Additionally

* `any_table()`: Added the `block` keywords within `var_labels` to the first example.


## R CMD check results

0 errors | 0 warnings | 0 note
