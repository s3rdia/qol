# Resubmission qol 1.2.3
Last CRAN release was on 11.03.2026.

### New functions

* `row_calculation()`: Perform row wise calculations on numeric variables.
* `do_if()`, `else_do()`, `end_do()`, `end_all_do()`: These functions create overarching filter variables. When used with functions capable of handling these filter variables, it is possible to create more readable if-blocks without writing the same condition over and over again.

### New functionality

* `if.()`, `else_if.()`: The ":" can now be used in conditions as a placeholder for "starts with" ("text:"), "ends with" (":text") and "contains" (":text:").
* `if.()`, `else_if.()`, `else.()`: All functions are now able to handle vectors in a do over loop. Meaning, that if using vectors in conditions or assignments, the functions will iterate over each vector simultaneously.
* `if.()`, `else_if.()`, `else.()`: Can be used within the new `do_if()` context.

### Fixed

* `any_table()`: Some examples had a too long running time. Additionally shortened the running time of unit tests by ignoring styling options in the larger tables.
* `any_table()`: Fixed percentages based on single formatted variable expressions couldn't be computed with by variables.
* `any_table()`: The column header wasn't put together correctly, if only multiple group percentages were computed.
* `any_table()`: Excel number stored as text error is now ignored in general even if no styling is used. This was especially noticeable when generating tables with by variables and the `print_miss` option.
* `any_table()`: Even if no styling is used the tables now all receive the named regions.
* `any_table()`: Columns are now sorted correctly, if only row or column percentages are computed.
* `dummy_data()`: Now cuts down observations by random sample instead of just the head, so that the cut expressions aren't lost completely.
* `dummy_data()`: `balance` variable wasn't calculated correctly, which is now fixed.

### Additionally

* `any_table()`: Fixed two of the examples where `list` was missing in formats parameter.
* Swapped out a variable in `split_by()` test, because the test didn't trow the expected error. To me this seems like a once in a lifetime error, because I can't reproduce this. 


## R CMD check results

0 errors | 0 warnings | 0 note
