# Resubmission qol 1.2.0
Last CRAN release was on 13.12.2025.

New functions
* vars_between(): Get variable names between two variables in a data frame.
* convert_factor(): Converts all given variables to factor.
* transpose_plus(): Format driven transposition, which is able to do more than just transpose values.
* sort_plus(): Sort data frame observations with some additions.
* split_by(): Since split_by_var() didn't have any benefit, split_by_var() and split_by_condition() are now fused into one function. Additionally to give the new function more power, formats can be used to not only split up a data frame by the actual values in the data frame, but also into the desired ones passed with a format. Using multilabels it is also possible to generate completely new values and therefore data frames on the fly.
* set(): Stack data frames by column names with optional character compression.
* Error handling functions:
	* resolve_intersection(): Compares if two vectors have intersecting values.
	* part_of_df(): Check if variable names are part of a data frame.
	* remove_doubled_values(): Remove values from a vector that appear more than once.
	* check_weight(): Check whether a suitable weight variable was provided.
* rename_multi(): Rename one or more variables.

New functionality
* inverse(): Now supports variable names written without quotation marks.
* keep()/dropp(): Now support variable ranges, like "state:income".
* any_table(): Removed pre_summed parameter. Instead the function now checks on it's own, whether the provided data frame is pre summarised or not.
* dots_to_char(): Renamed from args_to_char() and is now able to get the original argument from up the environment tree and return it as character vector.
* args_to_char(): Now converts the contents of a given argument to a character vector.

Removed
* split_by_var() and split_by_condition(): See comment under 'New functions'.
* is_numeric(): There was simply no benefit.

Fixed
* keep(): Variables where always output in provided order. order_vars = FALSE (default) will now output variables in order of appearance.
* any_table(): Now checks if a column combination is also part of the row combinations.
* any_table(): Fixed row header variables where sorted alphabetically instead of in provided order. Bug was introduced in version 1.1.1.
* any_table(): Fixed by variables could be sorted in the wrong order.
* summarise_plus(): Group percentages with nesting = "all" or "single" and na.rm = TRUE are now computed as intended. 

Optimization
* summarise_plus(): Now uses faster `collapse::na_omit` for NA removal.
* dummy_data(): Optimized and now just takes half the time to generate data.
* summarise_plus(): Swapped in more collapse functions. Turned off sorting of "collapse::GRP" when using the shortcut route.
* apply_formats(): NA value subsetting is now only done once and not twice with interval formats. "data.table::setkey" is now only called on the format data frame.
* summarise_plus(): When na.rm option is TRUE and only statistics based on sums are selected, summarise_plus() is now able to take the shortcut route. This also gives any_table() a huge performance boost when na.rm option is TRUE.
* In general: Swapped in more collapse functions.
* any_table(): If more than two group percentages have to be computed, any additional one gets computed faster, because they are computed on a smaller data frame.
* In general: Error handling has been generalized in many places.

New Error Checks
* summarise_plus() and any_table(): Added an error check in case an invalid statistic is provided.
* summarise_plus() and any_table(): Added an error check in case an invalid statistic is provided.
* summarise_plus() and any_table(): Added an error check in case an invalid type is provided.
* recode_multi(): Added an error check in case an unknown object is provided.

Additionally
* any_table(): Added an error check in case a variable combination was provided, which is not part of a pre summarised data frame.
* any_table(): Added missing format for variable 'state' in examples.
* Added transpose_plus() and sort_plus() examples to the README.
* discrete_format() and interval_format(): Labels will now be converted to numeric if they are all numeric.
* In general: Added more messages to display what functions do.

## R CMD check results

0 errors | 0 warnings | 0 note
