# Resubmission qol 1.3.6
Last CRAN release was on 25.10.2026.

### New functionality

* `combine_into_workbook()`, `create_table_of_contents()`: Are now able to set different colors for the subheader fonts.
* `sub_string()`: New parameters `include_from` and `include_to` control whether the text to find entered in `from` and `to` is included in the substring or not.
* `sub_string()`: New parameter `search_from_end` looks up the text to find entered in `from` and `to` from the end instead of the front to find last occurences.
* `sub_string()`: `from` and `to` now accept numeric vectors to enable individual positions.
* `if.()`, `else_if.()`, `ifelse_multi()`, `where.()`, `do_if()`, `split_by()`: In the SAS like writing style it is now possible to negate conditions with the keyword `not` like `not (age < 18)`.
* `any_table()`, `export_with_style()`: `var_labels` parameter now converts macro variables passed as character for the list names. In `any_table()` additionally for the `stat_labels`.
* `summarise_plus()`, `any_table()`, `transpose_plus()`: Macro variables can now be used as variable names, if specific statistics per variable were passed into the `statistics` parameter.
* `rename_multi()`: Macro variables can now be used in both the old and the new variable names.
* `compute.()`: Macro variables can now be used in the names of newly created variables.
* `multi_join()`: Unquoted variables in the `on` parameter can now refer to parameters of an enclosing custom function.

### Changed functionality

* `excel_output_style()`: Reduced all font sizes which were 10 to 9 to.

### Fixed

* `any_table()`: The default `pct_group` now ignores the internal pseudo column variable. With an empty `columns` parameter the percentages are computed within the last real variable instead of every row adding up to 100 %.
* `summarise_plus()`, `any_table()`: The note about missing values being dropped for percentile calculations is now only printed once per value variable instead of once per computed statistic and variable combination.
* `retain_stat()`: Should now work within `compute.()` when using multiple `by` variables.
* `any_table()`: Doesn't error anymore when using the keywords `row_pct` and `col_pct` on their own in the `pct_group` parameter.
* `set_labels()`: When entering NULL the function put out an error instead of resetting the labels. This is fixed now.
* `combine_into_workbook()`: When the `print_miss` parameter is used in one of the tables the respective first sheet of this table series is now used to clone the style from instead of the first sheet of the whole workbook.
* `combine_into_workbook()`: Individual table styles are now fully applied to the respective sheets.
* `any_table()`: Formatted variables in the column header now have their expressions displayed in order of the format even if an expression is missing at the time of the table pivoting.
* `export_multi()`: When passing a plain list without storing it into an object first, then now a generic filename is set instead of the function erroring.
* `create_table_of_contents()`: Empty titles caused the table of contents page to have a small column width for the title column. This is fixed now.
* `summarise_plus()`: When a list with specific statistics per variable were passed into the `statistics` parameter, the variable names were always converted into lower case names. This could lead to variable names not found in the data frame, if they are not written in all lower case. Now they are kept as they are passed.
* `discrete_format()`: When the `other` keyword was used, numbers stored as texts were always converted into numeric values. Now they are kept as character, if `as_character` is TRUE.
* `summarise_plus()`, `retain_value()`: There seems to be a rare edge case bug happening when an unknown encoding of a character variable is passed to collapse grouping. Problem is, the bug isn't reproducable reliable. So now there is an utf8 conversion happeinging before grouping to guarantee that there is always a known encodig.
* `any_table()`: In case value variables with a statistics extension are passed then they now don't receive a second extension anymore, which broke the column header.
* `if.()`, `else_if.()`: In a do-over-loop situation the conditions printed in the console displayed the vector names instead of the vector elements. This is fixed now.
* `any_table()`: If variables in not pre summed datasets already had statistic extensions in their variable names, the column order was corrupted. This is fixed now.
* Argument conversion: Unevaluated function parameters are now inspected without forcing promises, preventing promise evaluation warnings.

### Optimization

* `remove_blanks()`: Blank removal is now only performed on the unique values which are matched back afterwards.
* `concat()`: Now using a lookup table to determine the paddings instead of building them per observation.

### Additionally

* `any_table()`: Added the `block` keywords within `var_labels` to the first example.
* `create_table_of_contents()`: Now sets the table of contents sheet to active, so the intended view is also achieved with openxlsx2 as of v1.31.


## R CMD check results

0 errors | 0 warnings | 0 note
