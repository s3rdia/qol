# qol 1.2.1

### New functions

* `expand_formats()`: Generates a data frame which contains all nested combinations of the provided format labels. (15.01.2026)
* `set_print_miss()`, `get_print_miss()`: Additional global setters and getters. (15.01.2026)
* `concat()`: Concatenate multiple variables inside a data frame into a new variable with automatic or individual padding. (21.01.2026)
* `sub_string()`: Can extract text from left, right or middle and is able to look up characters as start or end position. (22.01.2026)
* `remove_blanks()`: Removes blanks in the expressions of a character variable. (24.01.2026)
* `macro()`, `apply_macro()`: Resolve objects starting with "&" within a text. Functions which can print titles, footnotes, variable or statistic labels make use of these functions, so that e.g. a title can be passed as "The current year is &year" and will be resolved to "The current year is 2026". (24.01.2026)
* `where.()`: A quick way to filter a data frame and get a direct view of the result. (27.01.2026)
* `free_memory()`: Provides more flexible ways to remove objects from memory. (28.01.2026)
* `import_multi()`: Import multiple csv or xlsx files. The function is also capable of importing all sheets from multiple xlsx file. (10.02.2026)
* `export_multi()`: Export multiple csv or xlsx files based on a list of data frames. The function is also capable of exporting all data frames to multiple sheets in one xlsx file. (10.02.2026)
* `get_duplicate_var_names()`: Checks for duplicate variable names in a data frame, e.g. AGE, age and Age. (11.02.2026)
* `get_duplicate_var_count()`: Counts the number of duplicated variables in a data frame. If a variable appears three times, e.g. AGE, age and Age, the variable count will be one. (11.02.2026)

### New functionality

* `summarise_plus()`, `any_table()`, `frequencies()`, `crosstabs()`: The new parameter `print_miss` outputs all possible categories of the grouping variables based on the provided formats, even if there are no observations for a combination. (15.01.2026)
* `retain_variables()`: The ":" can now be used as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:"). (15.01.2026)
* `excel_output_style()`: New `subheader` parameters which come into play when setting the `by_as_subheaders` to TRUE when using `any_table()`. The parameters can also be set as global option. (18.01.2026)
* `any_table()`: When using by variables with the new styling option `by_as_subheaders` the tables aren't split among multiple sheets, instead the by variable expressions are used as subheaders in one big table. (18.01.2026)
* `summarise_plus`, `any_table()`, `crosstabs()`: `statistics` parameter can now be passed without quotation marks. (11.02.2026)
* `any_table()`: `pct_group` parameter can now be passed without quotation marks. (11.02.2026)

### Changed functionality

* `frequencies()`: Until now the function always printed a means and a frequencies table as default. Now it only prints a frequencies table as default to get the main results as fast as possible on screen. The means table can be activated again with the new `means` parameter. (17.01.2026)
* `frequencies()`: When using multiple by variables with excel `output`, worksheets are now ordered in provided variable order instead of alternating. (17.01.2026)
* `mark_case()`: Now outputs a variable with 1/0 instead of TRUE/FALSE. (18.01.2026)
* `recode()`: Removed `new_var` parameter. Recode now doesn't return the whole data frame anymore, but only the recoded variable as a vector. So instead of writing `my_data <- my_data |> recode("age_group", age = age.)`, the syntax is now more natural like this `my_data[["age_group"]] <- my_data |> recode(age = age.)`. (21.01.2026)
* `running_number()`, `mark_case()`, `retain_value()`, `retain_sum()`: Removed `var_name` parameter. Functions now don't return the whole data frame anymore, but only the retained variable as a vector or list of vectors. So instead of writing e.g. `my_data <- my_data |> running_number("running")`, the syntax is now more natural like this `my_data[["running"]] <- my_data |> running_number()`. (22.01.2026)
* `any_table()`: When using pre summarised data, the error handling is now less restrict. Instead of aborting if the TYPE variable is missing, it is now auto generated. Additionally if statistic extensions are missing to the value variable names, the function now doesn't abort, instead extensions are now added according to the provided statistics and a warning is thrown. (23.01.2026)
* `summarise_plus()`: Numeric values stored as characters are now returned as character, while originally numeric variables stay numeric. (24.01.2026)
* `any_table()`: Instead of aborting when no values are passed, the function now generates a variable to output unweighted results. (02.02.2026)

### Fixed

* `frequencies()`: Mean tables are now printed when by variables are provided. (17.01.2026)
* `frequencies()`: No empty "total" table is printed when by variables are provided. (17.01.2026)
* `frequencies()`: "total" row is not printed anymore, when variables use multilabels and are computed with a by group. (17.01.2026)
* Unit test: Fixed global footnote option unit test. `set_titles()` was called instead of `set_footnotes()`. (18.01.2026)
* `any_table()`: When variable names started with the same base name and variable labels should be assigned, it could happen that the label of the shortest variable was applied to all variables. Now variable labels are always assigned from longest to shortest variable name to prevent this. (23.01.2026)
* `any_table()`: If a variable name was part of another variable label, it could happen that the already set label was altered with the label of the variable appearing in the label. This can't happen anymore. (24.01.2026)
* `summarise_plus()`: When a factor variable is used as class variable and a factor level has a "." in it, it is now output as provided instead of the "." being converted to "!!!". (24.01.2026)
* `summarise_plus()`: When a character variable is used as class variable and there is a "." in an expression, the function doesn't error anymore. (24.01.2026)
* `any_table()`: When removing the value variable label with "" from the column header, so that the header line is removed, the alternation of the header lines won't mess up anymore. (28.01.2026)
* `any_table()`: The function ran into an error when statistic labels where set in the variable labels parameter. This is no more possible. (28.01.2026)
* `build_master()`: Added `library(qol)` to the top of the file. Loading the package is neccessary so that the `libname()` functions are able to run. (29.01.2026)
* `any_table()`: Percentages could have the wrong number of decimal places, if the group percentage variable name ended in a specific way and a number. (05.02.2026)

### Optimization

* `summarise_plus()`: When only statistics based on sums are selected, the function already pre summarises the data frame, to apply the formats on a much smaller data frame. When using nesting = "all"/"single" the data frame is now pre summarised a second time before applying formats, this time only using the grouping variables of the combination beeing processed. This drastically cuts down memory allocation - especially for larger data frames - and speeds up every iteration significantly. In addition to this function `any_table()` benefits greatly from this optimization. (16.01.2026)
* `any_table()`, `frequencies()`, `crosstabs()`: When using by variables in excel `output`, the new `print_miss` option enables a shortcut in formatting the sheets after the first one. Since the option guarantees that all follow up sheets are printed with the exact same table width and height, because all categories are printed, only the first sheet must be formatted. All other sheets can clone the entire style from the first sheet. (17.01.2026)
* `replace_except()`: Got rid of the nested for loop. This saves time in `any_table()` on larger data frames. (17.01.2026)
* `reorder_combination()`: Reordering is now only done on unique vector values instead of a whole larger vector. This saves time in `any_table()` on larger data frames. (17.01.2026)

### New Error Checks

* `combine_into_workbook()`: Added an error check in case a provided object is no `any_table()` result list. (17.01.2026)
* `discrete_format()`, `interval_format()`: Added an error check in case something other than list elements are provided. (29.01.2026)

### Additionally

* `any_table()`: Removed c() in examples where not necessary. (24.01.2026)
* `if.()`: Now outputs a message on how many observations have been removed and how many are left. (27.01.2026)
* `any_table()`, `combine_into_worbook()`: Adjusted examples. (02.02.2026)
* `if.()`, `else_if.()`, `else.()`: If used inside a function, these functions should now be able to catch the original variable name passed into the parent function. (10.02.2026)
* `content_report`: Added duplicate variable count to global information. (11.02.2026)


# qol 1.2.0

CRAN release on 13.01.2026

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
* `mark_case()`, `retain_value()`, `retain_sum()`: Adjusted unit tests to something that actually makes sense. (10.01.2026)
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

CRAN release on 10.10.2025

* Added references to specific SAS functions in the description field of the Description file where they are mentioned.
* Removed specific seed in R/dummy_data.R
* Fixed a bug in dummy_data where it could happen that not enough observations where generated.


# qol 1.0.0 

* Initial CRAN submission.
