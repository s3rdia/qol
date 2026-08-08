# Resubmission qol 1.3.4
Last CRAN release was on 16.07.2026.

### New functions

* `create_table_of_contents()`: Creates a table of contents sheet with custom style based on the provided 'Excel' workbook.
* `check_required_package()`: Check whether a certain package is required and ask the user whether it should be installed or not.

### New functionality

* `load_file()`, `load_file_multi()`: When passing a named vector or list into the `keep` parameter, the original variables will directly be renamed.
* `any_table()`: Can now handle duplicate column names by making them unique (except NA columns).
* `frequencies()`, `crosstabs()`: Now also receive a colored background.
* `import_multi()`: Received a new paramter `stack_data` which can stack read in files and return them as a single data frame.
* `import_multi()`: Can now handle a vector of sheet names and import only the specified ones.
* `multi_join()`: Variable names in the `on` parameter can now be passed without quotation marks.
* `remove_stat_extension()`: Statistic extensions can now be passed without quotation marks.
* `set_style_options()`: Parameters, which are originally NULL, can now be reset by assigning a value of NULL.
* `combine_into_workbook()`: Can now create a custom styleable table of contents.
* `excel_output_style()`: Added parameters for the custom styleable table of contents.
* `any_table()`: When using `by` variables the special keyword `[by_var]` is now replaced with the actual `by` value in the titles and footnotes.
* `frequencies()`, `crosstabs()`: Now also export the Excel workbook.
* `frequencies()`, `crosstabs()`: When using `by` variables the special keyword `[by_var]` is now replaced with the actual `by` value in the titles and footnotes.
* `where.()`: Added a html rendered view of the data frame as fallback, if the new window through `utils::View()` can't be opened.
* `where.()`: Is now also able to use the new writing style with conditions as characters introduced by `ifelse_multi()`.
* `any_table()`: Can now render the tables as html file and show it in a browser window. This can be controlled via the new `output` options `html` and `excel_html`. The function now also returns an additional html element.

### Changed functionality

* `combine_into_workbook()`: With the function making use of the new `style` parameter, the `file` parameter has been removed. Saving files no works with the `style` parameter like in the other tabulation functions.
* `retain_value()`: Without by variable the function now carries forward values through upcoming NA values instead of just writing the first value into all other cells. 

### Removed

* `get_integer_length()`: There is no real benefit in this function.

### Fixed

* `any_table()`: Variables generated with the `compute` parameter now receive a default `statistics` extension, if they are missing one, so that the column header is generated correctly and a number format is applied.
* `mark_case()`: When marking `last` cases and the last expression within the `by` variable was NA, then the last case in the second to last expression became NA instead of TRUE. This is fixed now.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: When different title or footnote styles where used, they were only applied, if multiple colors where used. Otherwise the formatting broke. This is fixed now. Additionally the code is now simplified and streamlined.
* `any_table()`, `frequencies()`, `crosstabs()`: The additional `by` title is now also taken into account with individual styling, when multiple titles are present.
* `import_data()`: Fixed a Problem with encoding.
* `close_file()`: Now sets the `file` entry in the global list of style elements actually to NULL instead of removing it.
* `any_table()`, `summarise_plus()`: Now keep on running and throw a warning, if a character variable is passed as `values`, instead of throwing a cryptic error.
* `import_multi()`: Ran into an error when trying to import a not existing Excel workbook. Now the file path is checked before.
* `convert_factor()`: Now uses `unique()` instead of `collapse:unique()` to identify german umlauts as they are seen on screen and not as they are stored in memory, which caused errors.
* `frequencies()`: When using interval formats without weight, the sum columns where displayed even though they were equal to the freq columns. This is fixed now. 
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Removed the extra empty row over the tables when no titles where set. Additionally there are no more empty named regions for titles and footnotes, if they are not present.
* `combine_into_workbook()`: Now uses the global options for `print` and `monitor` parameter.
* `any_table()`: `pct_block` values are now rounded.
* `where.()`: Now shows the full data frame when condition is NULL instead of throwing an error.

### Additionally

* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Extended `titles` and `footnotes` description according to the new functionality.
* `frequencies()`, `crosstabs()`: Added some new messages on the styling progress.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Added named regions for titles, main title, footnotes, table header and row headers.
* Cleanup of a partial matches.
* `stack_data()`: Swapped out a copypasta error message with the right one.
* `combine_into_workbook()`: Added example on how to save a file.
* `dummy_data()`: `NUTS2` variable is now stored as an integer.
* `load_file()`: Added example which shows how to directly rename variables.
* `dummy_data()`: `NUTS3` variable is now stored as an integer.
* New suggested package `yyjsonr` added, which is required, for the new html view of `where.()`.
* `rename_multi()`: Clarified error message on old variable names not found in data frame.


## R CMD check results

0 errors | 0 warnings | 0 note
