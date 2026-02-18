# Resubmission qol 1.2.2
Last CRAN release was on 11.02.2026.

### Fixed

* `any_table()`: When there was an NA value in the first table column while the row label column was deleted, the NA symbol wasn't set. This is fixed now.
* `any_table()`: If the custom NA symbol is set to a number, Excels number stored as text error is now ignored.
* `transpose_plus()`: Took out a debug print I forgot in the function.
* `summarise_plus()`: Fixed formats not matching data when computing percentages, if numeric values are stored as character.

### Additionally

* `frequencies()`: Fixed two of the examples where `list` was missing in formats parameter. (14.02.2026)


## R CMD check results

0 errors | 0 warnings | 0 note
