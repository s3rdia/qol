# Resubmission qol 1.3.6
Last CRAN release was on 20.09.2026.

### New functionality

* `macro()`: If a non character variable is passed it is now converted to character instead of aborting and throwing an error.
* `transpose_plus()`: In a wide to long transposition the `values` parameter can now take in a named list which carries custom variable expressions for the generated id variable.
* `multi_join()`: When joining multiple data frames on different variable names it is now possible to pass in a list of vectors for the first `on` list entry to enable joining each data frame on the first one on different variable names.
* `drop_type_vars()`: When passing in a list of data frames, the function now removes the variables `TYPE`, `TYPE_NR` and `DEPTH` from all data frames within the list. 
* `retain_stat()`: Can now generate cumulative values.
* `multi_join()`: Single variables passed to the `on` parameter can now be written without quotation marks.
* `any_table()`, `excel_output_style()`: The new `block_borders` style parameter enables to draw borders between statistic blocks within the table area.
* `transpose_plus()`: If specific `statistics` are chosen per variable then now the `values` parameter can be omitted.
* `keep()`, `dropp()`, `retain_variables()`, `add_variable_range`: Are now able to select variable ranges by pattern. When writing e.g. age1-age10 (the hyphen triggers this behaviour) then alle "age" variables from 1 to 10 are selected regardless of where they are located in the data set.
* `do_if()`: Is now also able to use the new writing style with conditions as characters introduced by `ifelse_multi()`.
* `rename_multi()`: Single variables and vectors of variable names can now be renamed in the same call.
* `keep()`, `dropp()`: Now also accept vectors.
* `discrete_format()`, `interval_format()`: Added `as_character` paramter which ensures that the value labels are kept as character. Meaning numbers like "00110" keep their leading zeros instead of beeing converted into numeric value 110.

### Fixed

* `compute.()`: Some custom functions didn't work consistently in different situations. This is fixed now.
* Most of the functions couldn't resolve custom parameters passed on from a wrapper function by themselves. This is hopefully fixed.
* `compute.()`: Doesn't error anymore when calculations are written on multiple lines.
* `any_table()`: Variable labels weren't set when the variable name contained a `statistics` keyword. This is fixed now.
* `any_table()`: When working with pre summarised data then now the value variables appear in user provided order instead of order of appearance within the given data frame.
* `running_number()`: Now considers all variables when passing a vector of variable names into `by`.
* `if.()`, `else_if.()`, `ifelse_multi()`, `where.()`: Don't error anymore on parsing variable names ending in a number.
* `summarise_plus()`: In case the function was used inside a custom function, percentiles weren't detected as `statistics`. This is fixed now.
* `transpose_plus()`: If specific `statistics` are chosen per variable then now variable vectors without quotation marks can be passed.
* `dropp()`: Now ignores ranges which consist of two invalid variables instead of returning an empty data frame.
* `if.()`, `else_if.()`, `ifelse_multi()`, `where.()`: Now allow multi line character conditions instead of aborting with an error.
* `if.()`, `else_if.()`, `ifelse_multi()`, `where.()`, `do_if()`: In SAS like patterns like "15 <= age < 65" now formulas are accepted, e.g. "median * 60 / 100 <= income < median * 120 / 100".
* `keep()`, `dropp()`: Don't error anymore when duplicate variable names are passed or duplicates are generated in combination with variable ranges.
* `if.()`, `else_if.()`, `ifelse_multi()`, `where.()`, `do_if()`: German umlauts and ß should now work within character conditions.

### Optimization

* `multi_join()`: Now sorts all variables on the `on` variables before joining, which has a big impact when joining larger data frames on multiple key variables. Smaller joins might suffer from a small penalty because of that, but this should be neglectable.

### Additionally

* `if.()`, `else_if.()`, `else.()`: Now the messages also display the condition and not only target variable and value.


## R CMD check results

0 errors | 0 warnings | 0 note
