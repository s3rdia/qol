# Resubmission qol 1.1.2
Last CRAN release was on 13.12.2025.

New functions
* vars_between(): Get variable names between two variables in a data frame.
* keep()/dropp(): Now support variable ranges, like "state:income".

New functionality
* inverse(): Now supports variable names written without quotation marks.

Fixed
* keep(): Variables where always output in provided order. order_vars = FALSE (default) will now output variables in order of appearance.

## R CMD check results

0 errors | 0 warnings | 0 note
