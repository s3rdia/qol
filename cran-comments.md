# Resubmission qol 1.2.3
Last CRAN release was on 11.03.2026. The package has a new dependency "fst", which is used in the new `save_file()` and `load_file()` functions. 

### New functions

* `row_calculation()`: Perform row wise calculations on numeric variables.
* `do_if()`, `else_do()`, `end_do()`, `end_all_do()`: These functions create overarching filter variables. When used with functions capable of handling these filter variables, it is possible to create more readable if-blocks without writing the same condition over and over again.
* `compute()`: Compute new variables without having to write the name of the data frame multiple times.
* `save_file()`, `save_file_multi()`: Saves fst and rds files. Offers variable selection and observation subsetting. By default the function has a write protection, which has to be explicitly turned off to be able to overwrite files.
* `load_file()`, `load_file_multi()`: Loads fst and rds files. Provided variables to keep are read in case insensitive and are returned in provided order. Additionally a subset can be defined directly. Loaded files in the multi version can be stacked or output as a list.
* `set_threads()`, `get_threads()`: Globally sets/gets the number of used threads for the save and load file functions.
* Base R message system:
	* `print_message()`, `print_headline()`, `print_start_message()`, `print_closing()`, `print_step()`: Print out different messages with custom styling to the console.
	* `get_message_stack()`: Get the global message stack for inspection.
	* `set_no_print()`: Prevents messages being printed to the console globally. Can e.g. be used in unit test situations.
	* `print_stack_as_messages()`: Print the global message stack as message, warning or error to be able to receive their signals.
	* `convert_square_brackets()`: Transforms the format symbols (like {b}{/b}) into the actual console readable formattings.
	* `set_up_custom_message()`: Sets up the basic items for a custom message. These custom types can be input into `print_message()` and `print_step()`.
* `hex_to_256()`, `hex_to_ansi()`: Generate a 256-color 6x6x6 color cube and apply hex color and font weight to a text as ansi codes.
* `round_multi()`: Rounds multiple variables at once inside a data frame.
* `report_test_results()`, `test_package()`, `test_single_file()`: Print out tinytest results with custom reporting style.

### New functionality

* `if.()`, `else_if.()`: The ":" can now be used in conditions as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:").
* `if.()`, `else_if.()`, `else.()`: All functions are now able to handle vectors in a do over loop. Meaning, that if using vectors in conditions or assignments, the functions will iterate over each vector simultaneously.
* `if.()`, `else_if.()`, `else.()`: Can be used within the new `do_if()` context.
* `if.()`, `else_if.()`, `else.()`: Since the functions now make use of the new `compute()`, they are able to do more than just simple variable assignments and can also perform calculations.
* `row_calculation()`: Now has an optional rounding parameter.
* `dummy_data()`: Added `weight_per_year` variable.
* `build_master()`: Added `with_monitor` parameter, which allows to visualize the time consumption of the individual scripts.
* `split_by()`: Now also outputs NA values.
* `import_data()`, `import_multi()`, `export_data()`, `export_multi()`: Now can import and export csv files with file extension txt.
* `round_values()`: Now is also able to round in multiples of a provided value.
* `combine_into_worbook()`: Now can handle `export_with_style()` results.
* `excel_output_style()`: Now accepts multiple title and footnote colors, sizes and boldings.
* `excel_output_style()`: New option `header_stat_merging` determines how the statistic symbols or labels are merged in the column header. As a new standard the labels are now not fully merged but in blocks.
* `if.()`: Observation selection now also works as do over loop.
* `any_table()`: New parameter `pct_block` enables percentage calculation inside the respective second to last grouping of the row or column variables.
* `any_table()`: If the result expression of a format starts with an "!", this expression will be used in calculations but will be dropped in the result table. This can be used to calculate block percentages based on a total expression without showing the total in the result table.
* `any_table()`: Variable combinations in rows and columns can now also be passed like "state + (age sex education)", which reslts in "state + age", "state + sex", "state + education", with the addition that the results with the same root grouping will be sorted together concerning the row header variables. For the column sorting there is a new `order_by` option with "blocks" for that purpose.
* `any_table()`: There are new special `var_labels`, which add additional top column headers. When assigning labels to variables that start with "block", e.g. "block1" = "Column Percentages", "block2" = "Row Percentages", these labels will appear as the top line of the column header.
* `summarise_plus()`: Variable combinations in types can now also be passed like "state + (age sex education)", which reslts in "state + age", "state + sex", "state + education".
* `frequencies()`, `crosstabs()`, `any_table()`, `export_with_style()`: Titles and footnotes can now also link to cells "cell:" and to files "file:".

### Changed functionality

* `summarise_plus()`: Instead of aborting when no values are passed, the function now generates a variable to output unweighted results.
* `Unit tests`: Moved unit tests completely from `testthat` to `tinytest`. `testthat` repeatedly got in the way with its own environment, `tinytest` offers more natural conditions and works faster.
* `transpose_plus()`: Removed macro variable usage.
* `export_with_style()`: Now returns a list with table, workbook and meta information, which can be used with `combine_into_worbook`.

### Fixed

* `any_table()`: Some examples had a too long running time. Additionally shortened the running time of unit tests by ignoring styling options in the larger tables.
* `any_table()`: Fixed percentages based on single formatted variable expressions couldn't be computed with by variables.
* `any_table()`: The column header wasn't put together correctly, if only multiple group percentages were computed.
* `any_table()`: Excel number stored as text error is now ignored in general even if no styling is used. This was especially noticeable when generating tables with by variables and the `print_miss` option.
* `any_table()`: Even if no styling is used the tables now all receive the named regions.
* `any_table()`: Columns are now sorted correctly, if only row or column percentages are computed.
* `dummy_data()`: Now cuts down observations by random sample instead of just the head, so that the cut expressions aren't lost completely.
* `dummy_data()`: `balance` variable wasn't calculated correctly, which is now fixed.
* `any_table()`, `combine_into_workbook()`: The `output` parameter wasn't stored in the meta information list which lead to `combine_into_workbook()` not able to access it and always output formatted tables. Now the `output` parameter is stored in meta information.
* `import_data()`: In the examples the file names to read in weren't correct. This is fixed now.
* `split_by()`: Should now work inside custom functions.
* `retain_value()`, `retain_sum()`: On error now return NA instead of the whole data frame.
* `transpose_plus()`: Wide to long transposition now working again as intended.
* `any_table()`: Without column variables each variable in the column header received a "_ " at the end, which is not the case anymore.
* `sort_plus()`: When formats where used for sorting, the temporary sorting variables where always put up front instead of inserted in sequential order. This is fixed now.
* `keep()`, `dropp()`: If a character pattern is used to keep or drop variables and no variable matching the pattern is found, the functions now abort with a warning.
* `where.()`: Now aborts with a warning if no observations or variables are left in the data frame.
* `export_with_style()`: Can now rename variables that have blanks in their name.
* `summarise_plus()`, `frequencies()`, `crosstabs()`, `any_table()`: If an invalid format is passed, this now throws a warning instead of removing all formats silently. 

### Optimization

* `if.()`, `else_if.()`, `else.()`: With the new `compute()` they now perform variable assignments and calculations in a single pass instead of per iteration.
* `running_number()`: Now uses faster `data.table` function.
* `round_values()`: Now uses vectorized math to reduce memory allocation.
* `dummy_data()`: Reduced memory allocation.
* `summarise_plus()`: When using factor or character variables in `class` parameter, the check to preserve "." is now executed on the unique values first and only mapped back to the data frame, if there are any replacements needed, instead of running a `gsub` every time over the whole vector.
* `import_data()`, `import_multi()`: `import_data()` now also accepts a `openxlsx2` workbook as infile and not only a path to a workbook. This enables a massive performance boost for `import_multi()`, which now passes a workbook to `import_data()` instead of the file path.

### Additionally

* `any_table()`: Fixed two of the examples where `list` was missing in formats parameter.
* Swapped out a variable in abort `split_by()` because of duplicates test, because the test didn't throw the expected error. This was a very rare error because of bad luck with the dummy_data sample. The function works as intended.
* `any_table()`: Upped the dummy_data observations in tests to prevent rare errors.
* `any_table()`: Omitted table formatting in some tests to make them run quicker.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Added missing global options function in the `see also` section.
* `import_multi()`, `export_multi()`: Added some messages to show progress.
* `any_table()`, `export_with_style()`: Added examples for `combine_into_worbook()`.
* `any_table()`: When row header labels are suppressed, the excess slashes are now removed.
* `compute()`: Now throws a warning, if duplicate variable names are used, but goes on computing the valid variables.


## R CMD check results

0 errors | 0 warnings | 0 note
