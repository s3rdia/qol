# Resubmission qol 1.3.2
Last CRAN release was on 16.05.2026.

### New functions

* `code_statistics()`: Reads in a folder or entire folder structure, grabs all R script files, scans the contents for different patterns and outputs a small report.

### Renamed functions

* `retain_stat()`: `retain_sum` got a new name because it can now generate any statistic not only sums with the new `statistics` parameter.
* `set_labels()`, `get_labels()`: The global setters and getters for variable and statistic labels `set_variable_labels()`, `get_variable_labels()` and `set_stat_labels()`, `get_stat_labels()` have been put together in one function.

### New functionality

* `running_number()`: With the new `sort` parameter a vektor of variable names can be passed as `by`, which then is automatically sorted before generating the running number. There is also a new `group_nr` parameter which creates a running number for each group as a whole instead of a running number within the group.
* `concat()`: Can now concatenate variables with a separator.
* `recode_multi()`: New `convert` parameter converts recoded variables to numeric or character depending on the input format instead of leaving them as factors.
* `any_table()`, `excel_output_style()`: Can now apply a `background_color` to all cells which aren't covered by any other background color parameter.
* `frequencies()`: The `number format` option `sum_decimals` within the `excel_output_style()` function can now be used to set the decimal places in text outputs as well. 
* `libname()`: With the new `recursive` parameter files can now also be retreived from subfolders.
* `export_with_style()`: Can now apply a `background_color` to all cells which aren't covered by any other background color parameter.
* `export_with_style()`: Added a new `column_align` parameter with which every column alignment can be controlled individually.

### Changed functionality

* `compute.()`: Now handles calls sequential, so that a new variable generated within `compute.()` can directly be accesses and used within the same `compute.()` call.
* `compute.()`: In a do over loop, if different values are assigned to the same variable, then now the last value is assigned instead of the first.
* `summarise_plus()`: Percentiles are now calculated the SAS way, which is Type 2 described in `quantile()`.

### Fixed

* `if.()`, `else_if.()`, `else.()`: Do over loop couldn't handle logical vectors. Additionally character expresions where always converted to symbols, which was fine if variable names were meant, but crashed if actual character values were passed. This is fixed now.
* `print_step()`: Doesn't print empty lines anymore, when surrounding the function with `suppressMessages()`.
* `sort_plus()`: Doesn't crash anymore when trying to preserve factor variables.
* `export_data()`: When exporting a csv file characters like ä, ö, ü are displayed correctly, when opening the file in Excel.
* `libname()`: When using the `extensions` parameter then now only files with these extensions are printed out to the console. Otherwise a `NOTE` is displayed stating that no file has been found.
* `if.()`, `else_if.()`: If a colon ":" was written somewhere in a character expression inside a condition the function crashed because it was looking for the return value of the special ":" symbol. This is fixed now.
* `export_with_style()`: Function doesn't crash anymore, if a variable label is provided, but the corresponding variable doesn't exist in the data frame.
* `dummy_data()`: Variable `number_of_persons` now has the right values. Before it was generated too early.
* `compute.()`: If do-over-loop behaviour is combined with normal variable generation, then the loop now doesn't occur for the normal variable generation anymore.
* `any_table()`: The statistic `freq_g0` couldn't be renamed correctly. It can now be renamed by just doing `stat_labels = list("freq" = "Some label")`.
* `if.()`, `else_if.()`, `else.()`: If the variable to which a value should be assigned already existed in the data frame, then the existing values were overwritten with NA, if there were NA values in a variable that was used in the condition. Values are now preserved.

### Optimization

* `if.()`: Got rid of an unnecessary loop.
* `print_step()`, `print_start_message()`, `print_headline()`, `print_closing()`: Execution token is now only retrieved, if the print function is called from a low depth. Meaning deeply nested print statements wont retrieve the token because this becomes very resource heavy.
* `summarise_plus()`: Tackled a performance nightmare in the core summarisation function. Basically every statistic operation is now up to 10+ times faster than before and uses less than half the memory. This also has an impact on all the tabulation functions.
* `any_table()`: The cell merging for the column and row headers was handled range by range, meaning a merge per range. Now the ranges to be merged are collected first and then merged in one go. This gives a massive performance gain for larger tables.

### Additionally

* `if.()`, `export_with_style()`: Updated examples.
* `summarise_plus()`, `drop_type_vars()`: Updated documentation with a detailed description of the automatically generated variables TYPE, TYPE_NR and DEPTH when using the `nesting = "all"` or `nesting = "single"` option.
* `expand_formats()`: Added a `NOTE`message if the cartesian product produces over 1 million rows.
* `any_table()`: Added two new `NOTE` messages in case `pct_value` or `pct_block` is specified in statistics put corresponding parameter isn't set. In this case the statistic will be removed.
* `export_with_style()`: Added some more messages informing the user about the formatting timings.
* Global options documentation received some more comments. Additionally the functions themselves received some messages stating whether options have been set. Also the getters which retrieve a list have a nicer output.


## R CMD check results

0 errors | 0 warnings | 0 note
