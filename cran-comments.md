# Resubmission qol 1.2.0
Last CRAN release was on 13.12.2025.

New functions
* vars_between(): Get variable names between two variables in a data frame.
* convert_factor(): Converts all given variables to factor.
* transpose_plus(): Format driven transposition, which is able to do more than just transpose values.
* sort_plus(): Sort data frame observations with some additions.

New functionality
* inverse(): Now supports variable names written without quotation marks.
* keep()/dropp(): Now support variable ranges, like "state:income".
* any_table(): Removed pre_summed parameter. Instead the function now checks on it's own, whether the provided data frame is pre summarised or not.

Fixed
* keep(): Variables where always output in provided order. order_vars = FALSE (default) will now output variables in order of appearance.
* any_table(): Now checks if a column combination is also part of the row combinations.
* any_table(): Fixed row header variables where sorted alphabetically instead of in provided order. Bug was introduced in version 1.1.1.

Additionally
* any_table(): Added an error check in case a variable combination was provided, which is not part of a pre summarised data frame.
* summarise_plus(): Now uses faster collapse::na_omit for NA removal.
* dummy_data(): Optimized and now just takes half the time to generate data.

## R CMD check results

0 errors | 0 warnings | 0 note
