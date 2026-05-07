# Resubmission qol 1.3.1
Last CRAN release was on 18.04.2026.

### Renamed functions

* `compute.()`, `recode.()`: Both functions have been renamed and now have a "." at the end to prevent masking errors in combination with `dplyr`.

### New functionality

* `set_no_color()`: Suppresses the color codes so that messages can be printed clean. The option is auto controlled on load via the system variable `NO_COLOR` but can also be set individually by this function. Console output in e.g. RStudio vs. output to a logging system should be handled automatically rightnow.
* `set_up_custom_message()`: Waiting symbols as well as the color of the time stamps can now be customized.
* `print_step()`: Now has a new `in_place` parameter, which prints the message on the same line as before, instead of in the next line. This can e.g. be used inside loops as follows.
* `any_table()`, `export_with_style()`: If the whole result list from these functions is passed for the `workbook` parameter, the functions now are able to extract the actual workbook from the list and run without error. Additionally if a list is passed, which is not a result list containing the workbook, the functions error and abort execution.

### Changed functionality

* `set_up_custom_message()`: They way custom messages are set up has slightly changed and will break existing code. Custom message types are now stored globally and are called within quotation marks like all the other built-in message types. See updated README for the new handling.
* `interval_format()`: The function had a flaw in that it always included the lower and upper bound values. The "workaround" in the examples to pass bounds like e.g. 0:499, 500:999, would miss values between 499 and 500 or 999 and 1000. The function now got two new parameters `inlcude_lower` (TRUE) and `include_upper` (FALSE), which by default read as "from ... to under ...". Which means bounds can be passed like 0:500, 500:1000 without overlapping.

### Fixed

* `combine_into_workbook()`: Titles and footnotes are now styled again.
* `compute.()`: If a variable was all NA, a type miss match happend, leading to the function beeing aborted. This is fixed now.
* `print_step()`: Waiting character "?" is now drawn as intended in non-utf8 mode.
* `compute.()`, `if.()`, `else_if.()`, `else.()`: Now detect vectors for do-over-loops in every place. Previously do over loop was only detected, if the variable for assignment was a vector.
* `compute.()`: Doesn't crash anymore, if multiple values are assigned to the same variable in a do over loop.
* `dummy_data()`: Variable `income_class` was miss matched in some places due to it being generated to early. This is fixed now.
* `any_table()`: If the `block` keyword was used in `var_labels` parameter and all `block` keywords weren't written in lower case, the column header generation crashed. This is fixed now.

### Additionally

* Message time stamps are now drawn in a lighter grey to enhance visibility on darker themes.
* `else_do()`, `end_do()`, `end_all_do()`: Functions now throw errors instead of warnings, if `do_if()` is missing.


## R CMD check results

0 errors | 0 warnings | 0 note
