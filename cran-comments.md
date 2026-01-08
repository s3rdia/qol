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
* retain_variables(): Order variables to the front or back of a data frame. Can also add empty variables.
* add_variable_range(): Add empty variables to a data frame in the provided range.
* Global style options:
	* set_style_options(): Sets the options from excel_output_style() and number_format_style() to a global environment so that every function which is capable of outputting styled Excel workbooks can use them without passing the style parameter every time individually.
	* get_style_options(): Print the currently set global options.
	* reset_style_options(): Set global style options to default values of excel_output_style() and number_format_style().
	* close_file(): A simple, more readable wrapper for setting file parameter to NULL.
	* set_variable_labels(), get_variable_labels(), set_statistic_labels(), get_statistic_labels(), set_print(), get_print(), set_monitor(), get_monitor(), set_na.rm(), get_na.rm(): Additional global setters and getters.
* content_report(): Collects and prints global and per variable information about the provided data frame.

New functionality
* inverse(): Now supports variable names written without quotation marks.
* keep()/dropp(): Now support variable ranges, like "state:income".
* any_table(): Removed pre_summed parameter. Instead the function now checks on it's own, whether the provided data frame is pre summarised or not.
* dots_to_char(): Renamed from args_to_char() and is now able to get the original argument from up the environment tree and return it as character vector.
* args_to_char(): Now converts the contents of a given argument to a character vector.
* excel_output_style(): Parameter file is now split up into save_path and file (meaning just the file name + extension).
* any_table(), frequencies(), crosstabs(), export_with_style(): The style parameter is now set to the new global styling options as default instead of excel_output_style().
* any_table(), frequencies(), crosstabs(): The var_labels and stat_labels parameter is now set to the new global options as default.
* In general: New global options for print, monitor and na.rm have been implemented into the functions capable of using them.

Removed
* split_by_var() and split_by_condition(): See comment under 'New functions'.
* is_numeric(): There was simply no benefit.

Fixed
* keep(): Variables where always output in provided order. order_vars = FALSE (default) will now output variables in order of appearance.
* any_table(): Now checks if a column combination is also part of the row combinations.
* any_table(): Fixed row header variables where sorted alphabetically instead of in provided order. Bug was introduced in version 1.1.1.
* any_table(): Fixed by variables could be sorted in the wrong order.
* summarise_plus(): Group percentages with nesting = "all" or "single" and na.rm = TRUE are now computed as intended.
* handle_cell_styles(): Set apply_font and font_id in a save way to prevent warnings.
* summarise_plus(): Removed conversion to numeric values before applying formats, which could lead to not matching formats, if a numeric value was intentionally stored as character value.

Optimization
* summarise_plus(): Now uses faster collapse::na_omit for NA removal.
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
* any_table(), frequencies(), crosstabs(), export_with_style(): Added check if specified save path exists.

Additionally
* any_table(): Added an error check in case a variable combination was provided, which is not part of a pre summarised data frame.
* any_table(): Added missing format for variable 'state' in examples.
* Added transpose_plus() and sort_plus() examples to the README.
* discrete_format() and interval_format(): Labels will now be converted to numeric if they are all numeric.
* In general: Added more messages to display what functions do.

## R CMD check results

0 errors | 0 warnings | 0 note
