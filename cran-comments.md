# Resubmission qol 1.3.4
Last CRAN release was on 16.07.2026.

### New functionality

* `load_file()`, `load_file_multi()`: When passing a named vector or list into the `keep` parameter, the original variables will directly be renamed.
* `any_table()`: Can now handle duplicate column names by making them unique (except NA columns).
* `frequencies()`, `crosstabs()`: Now also receive a colored background.
* `import_multi()`: Received a new paramter `stack_data` which can stack read in files and return them as a single data frame.
* `import_multi()`: Can now handle a vector of sheet names and import only the specified ones.

### Removed

* `get_integer_length()`: There is no real benefit in this function.

### Fixed

* `any_table()`: Variables generated with the `compute` parameter now receive a default `statistics` extension, if they are missing one, so that the column header is generated correctly and a number format is applied.
* `mark_case()`: When marking `last` cases and the last expression within the `by` variable was NA, then the last case in the second to last expression became NA instead of TRUE. This is fixed now.
* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: When different title or footnote styles where used, they were only applied, if multiple colors where used. Otherwise the formatting broke. This is fixed now. Additionally the code is now simplified and streamlined.
* `any_table()`, `frequencies()`, `crosstabs()`: The additional `by` title is now also taken into account with individual styling, when multiple titles are present.
* `import_data()`: Fixed a Problem with encoding.

### Additionally

* `any_table()`, `frequencies()`, `crosstabs()`, `export_with_style()`: Extended `titles` and `footnotes` description according to the new functionality.
* `frequencies()`, `crosstabs()`: Added some new messages on the styling progress.
* Cleanup of a partial matches.
* `stack_data()`: Swapped out a copypasta error message with the right one.


## R CMD check results

0 errors | 0 warnings | 0 note
