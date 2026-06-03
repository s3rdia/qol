# Resubmission qol 1.3.2
Last CRAN release was on 16.05.2026.

### New functionality

* `running_number()`: With the new `sort` parameter a vektor of variable names can be passed as `by`, which then is automatically sorted before generating the running number. There is also a new `group_nr` parameter which creates a running number for each group as a whole instead of a running number within the group.
* `concat()`: Can now concatenate variables with a separator.
* `recode_multi()`: New `convert` parameter converts recoded variables to numeric or character depending on the input format instead of leaving them as factors.
* `any_table()`, `excel_output_style()`: Can now apply a `background_color` to all cells which aren't covered by any other background color parameter.

### Fixed

* `if.()`, `else_if.()`, `else.()`: Do over loop couldn't handle logical vectors. Additionally character expresions where always converted to symbols, which was fine if variable names were meant, but crashed if actual character values were passed. This is fixed now.
* `print_step()`: Doesn't print empty lines anymore, when surrounding the function with `suppressMessages()`.
* `sort_plus()`: Doesn't crash anymore when trying to preserve factor variables.
* `export_data()`: When exporting a csv file characters like ä, ö, ü are displayed correctly, when opening the file in Excel.

### Additionally

* `if.()`, `export_with_style()`: Updated examples.
* `summarise_plus()`, `drop_type_vars()`: Updated documentation with a detailed description of the automatically generated variables TYPE, TYPE_NR and DEPTH when using the `nesting = "all"` or `nesting = "single"` option.
* `expand_formats()`: Added a `NOTE`message if the cartesian product produces over 1 million rows.


## R CMD check results

0 errors | 0 warnings | 0 note
