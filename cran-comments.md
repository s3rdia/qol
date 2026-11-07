# Resubmission qol 1.3.6
Last CRAN release was on 25.10.2026.

### New functionality

* `combine_into_workbook()`, `create_table_of_contents()`: Are now able to set different colors for the subheader fonts.

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
* `any_table()`: Formatted variables in the column header now have their expressions displayed in order of the format even if an expression is missing at the time of the table pivoting.
* `export_multi()`: When passing a plain list without storing it into an object first, then now a generic filename is set instead of the function erroring.
* `create_table_of_contents()`: Empty titles caused the table of contents page to have a small column width for the title column. This is fixed now.

### Additionally

* `any_table()`: Added the `block` keywords within `var_labels` to the first example.
* `create_table_of_contents()`: Now sets the table of contents sheet to active, so the intended view is also achieved with openxlsx2 as of v1.31.


## R CMD check results

0 errors | 0 warnings | 0 note
