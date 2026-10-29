# Resubmission qol 1.3.6
Last CRAN release was on 25.10.2026.

### Fixed

* `any_table()`: The default `pct_group` now ignores the internal pseudo column variable. With an empty `columns` parameter the percentages are computed within the last real variable instead of every row adding up to 100 %.
* `summarise_plus()`, `any_table()`: The note about missing values being dropped for percentile calculations is now only printed once per value variable instead of once per computed statistic and variable combination.


## R CMD check results

0 errors | 0 warnings | 0 note
