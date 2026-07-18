# Resubmission qol 1.3.4
Last CRAN release was on 16.07.2026.

### New functionality

* `load_file()`, `load_file_multi()`: When passing a named vector or list into the `keep` parameter, the original variables will directly be renamed.

### Removed

* `get_integer_length()`: There is no real benefit in this function.

### Fixed

* `any_table()`: Variables generated with the `compute` parameter now receive a default `statistics` extension, if they are missing one, so that the column header is generated correctly and a number format is applied.
* `mark_case()`: When marking `last` cases and the last expression within the `by` variable was NA, then the last case in the second to last expression became NA instead of TRUE. This is fixed now.


## R CMD check results

0 errors | 0 warnings | 0 note
