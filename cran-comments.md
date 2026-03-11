# Resubmission qol 1.2.3
Last CRAN release was on 11.03.2026.

### Fixed

* `any_table()`: Some examples had a too long running time. Additionally shortened the running time of unit tests by ignoring styling options in the larger tables.
* `any_table()`: Fixed percentages based on single formatted variable expressions couldn't be computed with by variables.
* `any_table()`: The column header wasn't put together correctly, if only multiple group percentages were computed.
* `any_table()`: Excel number stored as text error is now ignored in general even if no styling is used. This was especially noticeable when generating tables with by variables and the `print_miss` option.
* `any_table()`: Even if no styling is used the tables now all receive the named regions.
* `any_table()`: Columns are now sorted correctly, if only row or column percentages are computed.

### Additionally

* `any_table()`: Fixed two of the examples where `list` was missing in formats parameter.


## R CMD check results

0 errors | 0 warnings | 0 note
