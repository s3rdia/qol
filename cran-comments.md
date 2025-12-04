# Resubmission qol 1.1.1
Last CRAN release was on 20.11.2025.

New functions
* multi_join(): Join two or more data frames together in one operation.
* libname(): Check if path exists and retrieve files.

Fixed
* any_table(): Fixed multi layerd column header labels where not applied correct.
* any_table(): Fixed incorrect column order when using order_by "values" while variable names have underscores.
* any_table(): Using pre summed data now also works, if variable names carry underscores.

## R CMD check results

0 errors | 0 warnings | 0 note
