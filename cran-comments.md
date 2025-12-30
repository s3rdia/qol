# Resubmission qol 1.2.0
Last CRAN release was on 13.12.2025.

New functions
* vars_between(): Get variable names between two variables in a data frame.
* convert_factor(): Converts all given variables to factor.
* transpose_plus(): Format driven transposition, which is able to do more than just transpose values.
* sort_plus(): Sort data frame observations with some additions.
* split_by(): Since split_by_var() didn't have any benefit, split_by_var() and split_by_condition() are now fused into one function. Additionally to give the new function more power, formats can be used to not only split up a data frame by the actual values in the data frame, but also into the desired ones passed with a format. Using multilabels it is also possible to generate completely new values and therefore data frames on the fly.
* set(): Stack data frames by column names with optional character compression.

New functionality
* inverse(): Now supports variable names written without quotation marks.
* keep()/dropp(): Now support variable ranges, like "state:income".
* any_table(): Removed pre_summed parameter. Instead the function now checks on it's own, whether the provided data frame is pre summarised or not.

Removed
* split_by_var() and split_by_condition(): See comment under 'New functions'.

Fixed
* keep(): Variables where always output in provided order. order_vars = FALSE (default) will now output variables in order of appearance.
* any_table(): Now checks if a column combination is also part of the row combinations.
* any_table(): Fixed row header variables where sorted alphabetically instead of in provided order. Bug was introduced in version 1.1.1.
* any_table(): Fixed by variables could be sorted in the wrong order.

Optimization
* summarise_plus(): Now uses faster `collapse::na_omit` for NA removal.
* dummy_data(): Optimized and now just takes half the time to generate data.
* summarise_plus(): Swapped in more collapse functions. Turned off sorting of "collapse::GRP" when using the shortcut route.
* apply_formats(): NA value subsetting is now only done once and not twice with interval formats. "data.table::setkey" is now only called on the format data frame.
* summarise_plus(): When na.rm option is TRUE and only statistics based on sums are selected, summarise_plus() is now able to take the shortcut route. This also gives any_table() a huge performance boost when na.rm option is TRUE.
* In general: Swapped in more collapse functions.
* any_table(): If more than two group percentages have to be computed, any additional one gets computed faster, because they are computed on a smaller data frame.

Additionally
* any_table(): Added an error check in case a variable combination was provided, which is not part of a pre summarised data frame.
* summarise_plus() and any_table(): Added an error check in case an invalid statistic is provided.

## R CMD check results

0 errors | 0 warnings | 0 note
