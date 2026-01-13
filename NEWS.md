# qol 1.2.0

### New functions

* `vars_between()`: Get variable names between two variables in a data frame. (21.12.2025)
* `convert_factor()`: Converts all given variables to factor. (23.12.2025)
* `transpose_plus()`: Format driven transposition, which is able to do more than just transpose values. (27.12.2025)
* `sort_plus()`: Sort data frame observations with some additions. (27.12.2025)
* `split_by()`: Since `split_by_var()` didn't have any benefit, `split_by_var()` and `split_by_condition()` are now fused into one function. Additionally to give the new function more power, formats can be used to not only split up a data frame by the actual values in the data frame, but also into the desired ones passed with a format. Using multilabels it is also possible to generate completely new values and therefore data frames on the fly. (30.12.2025)
* `set()`: Stack data frames by column names with optional character compression. (30.12.2025)
* Error handling functions:
	* `resolve_intersection()`: Compares if two vectors have intersecting values. (02.01.2026)
	* `part_of_df()`: Check if variable names are part of a data frame. (02.01.2026)
	* `remove_doubled_values()`: Remove values from a vector that appear more than once. (02.01.2026)
	* `check_weight()`: Check whether a suitable weight variable was provided. (02.01.2026)
* `rename_multi()`: Rename one or more variables. (02.01.2026)
* `retain_variables()`: Order variables to the front or back of a data frame. Can also add empty variables. (04.01.2026)
* `add_variable_range()`: Add empty variables to a data frame in the provided range. (04.01.2026)
* Global style options:
	* `set_style_options()`: Sets the options from `excel_output_style()` and `number_format_style()` to a global environment so that every function which is capable of outputting styled Excel workbooks can use them without passing the `style` parameter every time individually. (06.01.2026)
	* `get_style_options()`: Print the currently set global options. (06.01.2026)
	* `reset_style_options()`: Set global style options to default values of `excel_output_style()` and `number_format_style()`. (06.01.2026)
	* `close_file()`: A simple, more readable wrapper for setting file parameter to NULL. (07.01.2026)
	* `set_variable_labels()`, `get_variable_labels()`, `set_statistic_labels()`, `get_statistic_labels()`, `set_print()`, `get_print()`, `set_monitor()`, `get_monitor()`, `set_na.rm()`, `get_na.rm()`: Additional global setters and getters. (07.01.2026)
	* `set_output()`, `get_output()`: Additional global setters and getters. (11.01.2026)
	* `set_titles()`, `get_titles()`, `set_footnotes()`, `get_footnotes()`: Additional global setters and getters. (11.01.2026)
* `content_report()`: Collects and prints global and per variable information about the provided data frame. (08.01.2026)
* `import_data()`, `export_data()`: Lightweight import and export for csv and xlsx files. (10.01.2026)
* `first_row_as_names()`: Sets the values of the first data frame row as variable names and deletes first row. (10.01.2026)
* `qol_news()`: Opens changelog GitHub Page. (11.01.2026)

### New functionality

* `inverse()`: Now supports variable names written without quotation marks. (21.12.2025)
* `keep()`/`dropp()`: Now support variable ranges, like "state:income". (21.12.2025)
* `any_table()`: Removed `pre_summed` parameter. Instead the function now checks on it's own, whether the provided data frame is pre summarised or not. (23.12.2025)
* `dots_to_char()`: Renamed from `args_to_char()` and is now able to get the original argument from up the environment tree and return it as character vector. (02.01.2026)
* `args_to_char()`: Now converts the contents of a given argument to a character vector. (02.01.2026)
* `excel_output_style()`: Parameter `file` is now split up into `save_path` and `file` (meaning just the file name + extension). (06.01.2026)
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: The `style` parameter is now set to the new global styling options as default instead of `excel_output_style()`. (06.01.2026)
* `any_table()`: The `var_labels` and `stat_labels` parameter is now set to the new global options as default. (07.01.2026)
* In general: New global options for `print`, `monitor` and `na.rm` have been implemented into the functions capable of using them. (07.01.2026)
* `if.()`: If only a single variable name is provided, it is now evaluated as !is.na(var_name). (10.01.2026)
* `retain_value()`: Reworked iternally, can now also handle character variables. (10.01.2026)
* `excel_output_style()`: New parameters `as_heatmap`, `heatmap_low_color`, `heatmap_middle_color` and `heatmap_high_color`. These can also be set as global options. If `as_heatmap` is TRUE the tables from `any_table()`, `frequencies()`, `crosstabs()` and `export_with_style()` receive a conditional formatting. (11.01.2026)
* `crosstabs()`: Now has a new parameter `show_total` to control output of row and column totals. (11.01.2026)
* `keep()`/`dropp()`: The ":" can now be used as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:"). (12.01.2026)
* `any_table()`: Is now able to generate 'pct_value' statistic without outputting additional 'sum' values. (12.01.2026)
* `build_master()`: Now generates a code block which can rebuilt the master file. Additionally the folder structure is now generated in code blocks which can open the folders and files. Overall gave the master file a bit more visual structure. (12.01.2026)

### Removed

* `split_by_var()` and `split_by_condition()`: See comment under 'New functions'. (30.12.2025)
* `is_numeric()`: There was simply no benefit. (02.01.2026)

### Fixed

* `keep()`: Variables where always output in provided order. order_vars = FALSE (default) will now output variables in order of appearance. (21.12.2025)
* `any_table()`: Now checks if a column combination is also part of the row combinations. (22.12.2025)
* `any_table()`: Fixed row header variables where sorted alphabetically instead of in provided order. Bug was introduced in version 1.1.1. (23.12.2025)
* `any_table()`: Fixed by variables could be sorted in the wrong order. (30.12.2025)
* `any_table()`: Added missing format for variable 'state' in examples. (02.01.2026)
* `frequencies()`: 'formats' parameter was missing a '=' in examples. (02.01.2026)
* `summarise_plus()`: Group percentages with nesting = "all" or "single" and na.rm = TRUE are now computed as intended. (02.01.2026)
* `handle_cell_styles()`: Set apply_font and font_id in a save way to prevent warnings. (04.01.2026, thanks to @JanMarvin)
* `summarise_plus()`: Removed conversion to numeric values before applying formats, which could lead to not matching formats, if a numeric value was intentionally stored as character value. (07.01.2026)
* `any_table()`: When na.rm was TRUE and many table cells where generated while only having few observations, it could happen that some combinations weren't generated and a result mismatch happend. Results are now joined instead of cbind together to be safe. (11.01.2026)

### Optimization

* `summarise_plus()`: Now uses faster `collapse::na_omit` for NA removal. (27.12.2025)
* `dummy_data()`: Optimized and now just takes half the time to generate data. (27.12.2025)
* `summarise_plus()`: Swapped in more `collapse` functions. Turned off sorting of `collapse::GRP` when using the shortcut route. (28.12.2025)
* `apply_formats()`: NA value subsetting is now only done once and not twice with interval formats. `data.table::setkey` is now only called on the format data frame. (28.12.2025)
* `summarise_plus()`: When na.rm option is TRUE and only statistics based on sums are selected, `summarise_plus()` is now able to take the shortcut route. This also gives `any_table()` a huge performance boost when na.rm option is TRUE. (29.12.2025)
* In general: Swapped in more `collapse` functions. (29.12.2025)
* `any_table()`: If more than two group percentages have to be computed, any additional one gets computed faster, because they are computed on a smaller data frame. (29.12.2025)
* In general: Error handling has been generalized in many places to make it more robust and get a better overview. (02.01.2026)
* `apply_format()`: Removed redundant data.table transformations. (11.01.2026)

### New Error Checks

* `any_table()`: Added an error check in case a variable combination was provided, which is not part of a pre summarised data frame. (23.12.2025)
* `summarise_plus()` and `any_table()`: Added an error check in case an invalid statistic is provided. (29.12.2025)
* `summarise_plus()` and `any_table()`: Added an error check in case an invalid type is provided. (02.01.2026)
* `recode_multi()`: Added an error check in case an unknown object is provided. (02.01.2026)
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Added check if specified save path exists. (08.01.2026)
* `export_with_style()`: Added check for output. (11.01.2026)

### Additionally

* Restructured some "Small Helpers" into "Renaming" and "Variable Selection". (21.12.2025)
* Adjusted some warning messages. (22.12.2025)
* Added some comments to the heavier functions, to enhance visual code structure. (22.12.2025)
* Added `transpose_plus()` and `sort_plus()` examples to the README. (27.12.2025)
* `discrete_format()` and `interval_format()`: Labels will now be converted to numeric if they are all numeric. (02.01.2026)
* In general: Added more messages to display what functions do. (03.01.2026)
* `retain_value()`, `retain_sum()`: `value` parameter is now called `values`. (10.01.2026)
* `mark_cases()`, `retain_value()`, `retain_sum()`: Adjusted unit tests to something that actually makes sense. (10.01.2026)
* In general: Added some unit tests on file saving and retrieving. (10.01.2026)
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Updated examples to show how a file is saved via the style element. (13.01.2026)

### GitHub related

* Updated article on further comparison with SAS on import csv and xlsx and `content_report()` as well as the new `if.()` selection with only providing a variable name. Also added `transpose_plus()` example and selecting ranges with `keep()`/`dropp()`. (12.01.2026)


# qol 1.1.1

CRAN release on 13.12.2025

### New functions

* `multi_join()`: Join two or more data frames together in one operation. (30.11.2025)
* `libname()`: Check if path exists and retrieve files. (04.12.2025)

### Fixed

* `any_table()`: Fixed multi layered column header labels where not applied correct. (28.11.2025)
* `any_table()`: Fixed incorrect column order when using order_by "values" while variable names have underscores. (04.12.2025)
* `any_table()`: Using pre summed data now also works, if variable names carry underscores. (04.12.2025)

### GitHub related

* Added article comparing this package with SAS even further. (02.12.2025)


# qol 1.1.0

CRAN release on 20.11.2025

### New functions

* `build_master()`: Reads a given folder structure, which contains scripts, and builds a master script as a markdown file. (18.10.2025)
* `build_rstheme()`: Build a complete theme file, which can be used to change the visual appearance of RStudio. (23.10.2025)
* `combine_into_workbook()`: Combines any number of tables created with `any_table()` into one workbook and styles them according to their meta information. (26.10.2025)
* `replace_except()`: Replaces a provided pattern with another, while protecting exceptions. (29.10.2025)
* `mark_case()`: Marks first or last cases within a provided group. (31.10.2025)
* `retain_value()`: Retains the first value for all cases of the same group. (31.10.2025)
* `retain_sum()`: Retains the summarised values for all cases of the same group. (31.10.2025)

### New functionality

* `interval_format()`: Implemented keywords "low "and "high" with which one can define pseudo low or high values, if one doesn't know, what the minimum or maximum value of a variable is. (20.10.2025)
* `discrete_format()`: Implemented keyword "other" with which one can catch any other value not covered by the explicitly specified values. (27.10.2025)
* `any_table()`: Now returns styling meta information as a third list element. This meta information can be used by `combine_into_workbook()`. (26.10.2025)
* `any_table()`: Now supports underscores in variable names. (29.10.2025)
* `any_table()`: Added new column ordering possibilities by "columns" or "values_stats". (03.11.2025)
* `any_table()`: Can now output tables even though no column variables are specified. (05.11.2025)

### Fixed

* In `any_table()` the header and table row heights as well as the column widths set by the style option where 1 row/column to short. (14.10.2025)
* In `any_table()` the row heights didn't catch the whole table. (14.10.2025)
* `any_table()` ran into an error, if a variable was provided as pct_group, which was not part of the row and column variables. (14.10.2025)
* Fixed typos in frequencies examples, where it said "frequency" instead of "frequencies". (18.10.2025)
* In Excel outputs the number stored as text error is now ignored. (19.10.2025, thanks to @JanMarvin)
* Fixed table length captured with too many rows. (19.10.2025)
* `any_table()`: Fixed order_by stats not working as intended in some cases. (03.11.2025)
* `any_table()`: Fixed table rows not ordered correctly in some cases. (03.11.2025)
* `any_table()`: Depending on variable constellation and ordering of the column header it could happen, that the header wasn't merged correct in the Excel workbook. This was fixed. (04.11.2025)
* `any_table()`: Doesn't run into an error, if the table only consists of one value column. (05.11.2025)

### Changed functionality

* In `excel_output_style()` the options `column_widths` and `row_heights` now start at the first column/row instead of the beginning of the table. (19.10.2025)
* In `summarise_plus()`, if summarised values should be merged back, the variables TYPE, TYPE_NR and DEPTHS are now not merged back anymore. (31.10.2025)

### Additionally

* Added missing functions `export_with_style()` and `get_excel_range()` to the ?qol overview page. (14.10.2025)
* Added information to the startup message to use ?qol to get an overview. (14.10.2025)
* Now using openxlsx2 helper to convert row and column numbers to Excel ranges. (19.10.2025, thanks to @JanMarvin)
* All Excel tables now have named ranges for the table and the values. (19.10.2025, thanks to @JanMarvin)
* Added an example to the README showing how to save an Excel workbook to the filesystem. (19.10.2025)
* Used lintr package for some code cleanup. (20.10.2025, thanks to @JanMarvin for the advise)
* Added custom theme section to the README. (23.10.2025)
* Added message, if a format is applied to a factor variable. (27.10.2025)
* Excel workbooks will now only be opened in interactive sessions. (29.10.2025, thanks to @JanMarvin)
* Corrected typos in snippets. (03.11.2025)

### GitHub related

* Added article comparing this package with SAS. (14.10.2025)
* Changed GitHub Page style. (14.10.2025)
* Added example themes with corresponding code. (23.10.2025)


# qol 1.0.2

CRAN release on 14.10.2025

### DESCRIPTION file

* Fixed brackets in DESCRIPTION so that auto linking works.
* Added another URL.

### Fixed

* There could be an error in `summarise_plus()`, `any_table()` and `args_to_char()`, if there where to many variables provided.
* `summarise_plus()` ran into an error, if a value label from a format contained a ".".
* `any_table()` could lead to wrong results if the pre_summed option was used and a variable was part of multiple combinations in the summarised data.

### Changed functionality

* In `summarise_plus()`, when types are defined, the total row is now removed if not explicitly defined as type.
* In `summarise_plus()`, when the nesting option "deepest" is used, the variables TYPE, TYPE_NR and DEPTHS are now also generated.

### Unit tests

* Adjusted tests according to changed functionalities

### Additionally

* Added openxlsx2 as import in the qol main help file
* Corrected a typo in a warning message in `any_table()` concerning variable order.


# qol 1.0.1 

* Added references to specific SAS functions in the description field of the Description file where they are mentioned.
* Removed specific seed in R/dummy_data.R
* Fixed a bug in dummy_data where it could happen that not enough observations where generated.


# qol 1.0.0 

* Initial CRAN submission.
