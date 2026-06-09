# Resubmission qol 1.3.2
Last CRAN release was on 16.05.2026.

### New functions

* `code_statistics()`: Reads in a folder or entire folder structure, grabs all R script files, scans the contents for different patterns and outputs a small report. 

### New functionality

* `running_number()`: With the new `sort` parameter a vektor of variable names can be passed as `by`, which then is automatically sorted before generating the running number. There is also a new `group_nr` parameter which creates a running number for each group as a whole instead of a running number within the group.
* `concat()`: Can now concatenate variables with a separator.
* `recode_multi()`: New `convert` parameter converts recoded variables to numeric or character depending on the input format instead of leaving them as factors.
* `any_table()`, `excel_output_style()`: Can now apply a `background_color` to all cells which aren't covered by any other background color parameter.
* `frequencies()`: The `number format` option `sum_decimals` within the `excel_output_style()` function can now be used to set the decimal places in text outputs as well. 
* `libname()`: With the new `recursive` parameter files can now also be retreived from subfolders.

### Fixed

* `if.()`, `else_if.()`, `else.()`: Do over loop couldn't handle logical vectors. Additionally character expresions where always converted to symbols, which was fine if variable names were meant, but crashed if actual character values were passed. This is fixed now.
* `print_step()`: Doesn't print empty lines anymore, when surrounding the function with `suppressMessages()`.
* `sort_plus()`: Doesn't crash anymore when trying to preserve factor variables.
* `export_data()`: When exporting a csv file characters like ä, ö, ü are displayed correctly, when opening the file in Excel.
* `libname()`: When using the `extensions` parameter then now only files with these extensions are printed out to the console. Otherwise a `NOTE` is displayed stating that no file has been found.
* `if.()`, `else_if.()`: If a colon ":" was written somewhere in a character expression inside a condition the function crashed because it was looking for the return value of the special ":" symbol. This is fixed now.

### Additionally

* `if.()`, `export_with_style()`: Updated examples.
* `summarise_plus()`, `drop_type_vars()`: Updated documentation with a detailed description of the automatically generated variables TYPE, TYPE_NR and DEPTH when using the `nesting = "all"` or `nesting = "single"` option.
* `expand_formats()`: Added a `NOTE`message if the cartesian product produces over 1 million rows.
* `any_table()`: Added two new `NOTE` messages in case `pct_value` or `pct_block` is specified in statistics put corresponding parameter isn't set. In this case the statistic will be removed.


## R CMD check results

0 errors | 0 warnings | 0 note
