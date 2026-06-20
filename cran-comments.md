# Resubmission qol 1.3.3
Last CRAN release was on 16.06.2026.

### New functions

* `ifelse_multi()`: A variation of the `ifelse()` function which can handle multiple conditions in one go. The function takes in the conditions as characters. The conditions are parsed before evaluation to enable SAS like writing styles.

### New functionality

* `if.()`, `else_if.()`: Are now also able to use the new writing style with conditions as characters introduced by `ifelse_multi()`.

### Optimization

* `summarise_plus()`: Brought percentiles up to more speed.


## R CMD check results

0 errors | 0 warnings | 0 note
