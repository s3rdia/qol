# Resubmission qol 1.3.5
Last CRAN release was on 20.08.2026.

### Changed functionality

* `summarise_plus()`: When only passing `class` variables without `values` and using the default `statistics` then only frequencies will be calculated instead of sums and frequencies, which would be identical.

### Fixed

* `combine_into_workbook()`: Fixed table of contents example not working as intended.
* `crosstabs()`: If a vector of variables is provided for `columns` then now the first variable will be picked instead of the second one.
* `else_if()`, `else.()`: When assigning to multiple variables in a do-over-loop situation the functions would only consider the first assignment. This is fixed now. 


## R CMD check results

0 errors | 0 warnings | 0 note
