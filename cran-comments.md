# Resubmission qol 1.3.3
Last CRAN release was on 16.06.2026.

### New functions

* `ifelse_multi()`: A variation of the `ifelse()` function which can handle multiple conditions in one go. The function takes in the conditions as characters. The conditions are parsed before evaluation to enable SAS like writing styles.

### New functionality

* `if.()`, `else_if.()`: Are now also able to use the new writing style with conditions as characters introduced by `ifelse_multi()`.
* `any_table()`: With the new `compute` parameter all possibilities of the `compute.()` function can be used within `any_table()`.
* `any_table()`: `order_by` parameter now allows to input a vector of variable names.

### Fixed

* `multi_join()`: Before joining it is now checked whether there will be duplicate variable names after the join. If so, these variables will be dropped before joining. Otherwise it was possible that a variable could show up in the final data frame with the exact same name multiple times.
* `any_table()`: The statistic `sum_wgt` couldn't be renamed correctly. It can now be renamed by just doing `stat_labels = list("sum" = "Some label")`.

### Optimization

* `summarise_plus()`: Brought percentiles up to more speed.

### Additionally

* `multi_join()`: Now displays the actual data frame names to be joined instead of just an iterative number in the console.


## R CMD check results

0 errors | 0 warnings | 0 note
