# Resubmission qol 1.3.3
Last CRAN release was on 16.06.2026.

### New functions

* `ifelse_multi()`: A variation of the `ifelse()` function which can handle multiple conditions in one go. The function takes in the conditions as characters. The conditions are parsed before evaluation to enable SAS like writing styles.

### Renamed functions

* `stack_data()`: `set` was renamed to prevent masking errors in combination with `data.table`.

### New functionality

* `if.()`, `else_if.()`: Are now also able to use the new writing style with conditions as characters introduced by `ifelse_multi()`.
* `any_table()`: With the new `compute` parameter all possibilities of the `compute.()` function can be used within `any_table()`.
* `any_table()`: `order_by` parameter now allows to input a vector of variable names.
* `any_table()`, `summarise_plus()`: Variable combinations in rows, columns and types can now also take in variable combinations inside the brackets like "state + (age, sex + education, first_person)".
* `All functions with filepaths`: Wherever a file path or name can be passed, macro variables can be used inside the path or name.

### Changed functionality

* `any_table()`, `summarise_plus()`: Variable combinations in rows, columns and types like "state + (age, sex + education, first_person)", now have to use commas as separators in the brackets.
* `import_data()`: Now uses Latin-1 encoding.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Now the whole background is filled white as default. The new default is set in `excel_output_style()`.

### Fixed

* `multi_join()`: Before joining it is now checked whether there will be duplicate variable names after the join. If so, these variables will be dropped before joining. Otherwise it was possible that a variable could show up in the final data frame with the exact same name multiple times.
* `any_table()`: The statistic `sum_wgt` couldn't be renamed correctly. It can now be renamed by just doing `stat_labels = list("sum" = "Some label")`.
* `stack_data()`: When a data frame and a list was input, the function tried to hack the list into one of the data frame columns. Now all lists are flattened and data frames extracted to prevent this behaviour.
* `print_start_message()`: Now doesn't reset start timer anymore when message is suppressed.
* `print_start_message()`: Doesn't reset message stack  anymore when called in a deeply nested situation, which was caused by the optimization introduced in the last update.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Footnote heights are applied again.
* `export_with_style()`: Excel number stored as text error is now ignored for the whole table.

### Optimization

* `summarise_plus()`: Brought percentiles up to more speed.
* I underestimated how big in size the call stack can get depending on what functions are called. Therefore the message stack now doesn't receive the full call stack per message anymore but a condensed version and just as a list of character strings.

### Additionally

* `multi_join()`: Now displays the actual data frame names to be joined instead of just an iterative number in the console.
* `any_table()`: Received some additional messages for computing stats.
* `summarise_plus()`: Added example for type combinations.
* Minimum `openxlsx2` version is now 1.28, which allows a faster background fill.
* `load_file_multi()`: Added a message when files are stacked.


## R CMD check results

0 errors | 0 warnings | 0 note
