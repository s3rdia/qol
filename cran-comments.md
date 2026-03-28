# Resubmission qol 1.2.3
Last CRAN release was on 11.03.2026. The package has a new dependency "fst", which is used in the new `save_file()` and `load_file()` functions. 

### New functions

* `row_calculation()`: Perform row wise calculations on numeric variables.
* `do_if()`, `else_do()`, `end_do()`, `end_all_do()`: These functions create overarching filter variables. When used with functions capable of handling these filter variables, it is possible to create more readable if-blocks without writing the same condition over and over again.
* `compute()`: Compute new variables without having to write the name of the data frame multiple times.
* `save_file()`, `save_file_multi()`: Saves fst and rds files. Offers variable selection and observation subsetting. By default the function has a write protection, which has to be explicitly turned off to be able to overwrite files.
* `load_file()`, `load_file_multi()`: Loads fst and rds files. Provided variables to keep are read in case insensitive and are returned in provided order. Additionally a subset can be defined directly. Loaded files in the multi version can be stacked or output as a list.
* `set_threads()`, `get_threads()`: Globally sets/gets the number of used threads for the save and load file functions.

### New functionality

* `if.()`, `else_if.()`: The ":" can now be used in conditions as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:").
* `if.()`, `else_if.()`, `else.()`: All functions are now able to handle vectors in a do over loop. Meaning, that if using vectors in conditions or assignments, the functions will iterate over each vector simultaneously.
* `if.()`, `else_if.()`, `else.()`: Can be used within the new `do_if()` context.
* `if.()`, `else_if.()`, `else.()`: Since the functions now make use of the new `compute()`, they are able to do more than just simple variable assignments and can also perform calculations.
* `row_calculation()`: Now has an optional rounding parameter.
* `dummy_data()`: Added `weight_per_year` variable.
* `build_master()`: Added `with_monitor` parameter, which allows to visualize the time consumption of the individual scripts.

### Changed functionality

* `summarise_plus()`: Instead of aborting when no values are passed, the function now generates a variable to output unweighted results.

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

### Optimization

* `if.()`, `else_if.()`, `else.()`: With the new `compute()` they now perform variable assignments and calculations in a single pass instead of per iteration.
* `running_number()`: Now uses faster `data.table` function.
* `round_values()`: Now uses vectorized math to reduce memory allocation.
* `dummy_data()`: Reduced memory allocation.

### Additionally

* `any_table()`: Fixed two of the examples where `list` was missing in formats parameter.
* Swapped out a variable in abort `split_by()` because of duplicates test, because the test didn't throw the expected error. This was a very rare error because of bad luck with the dummy_data sample. The function works as intended.
* `any_table()`: Upped the dummy_data observations in tests to prevent rare errors.
* `any_table()`: Omitted table formatting in some tests to make them run quicker.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Added missing global options function in the `see also` section.


## R CMD check results

0 errors | 0 warnings | 0 note
