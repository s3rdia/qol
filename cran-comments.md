# Resubmission qol 1.3.1
Last CRAN release was on 18.04.2026.

### Renamed functions

* `compute.()`, `recode.()`: Both functions have been renamed and now have a "." at the end to prevent masking errors in combination with `dplyr`.

### Fixed

* `combine_into_workbook()`: Titles and footnotes are now styled again.
* `compute.()`: If a variable was all NA, a type miss match happend, leading to the function beeing aborted. This is fixed now.

### Additionally

* Message time stamps are now drawn in a lighter grey to enhance visibility on darker themes.
* `else_do()`, `end_do()`, `end_all_do()`: Functions now throw errors instead of warnings, if `do_if()` is missing.


## R CMD check results

0 errors | 0 warnings | 0 note
