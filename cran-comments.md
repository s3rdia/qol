# Resubmission qol 1.3.2
Last CRAN release was on 16.05.2026.

### New functionality

* `running_number()`: With the new `sort` parameter a vektor of variable names can be passed as `by`, which then is automatically sorted before generating the running number. There is also a new `group_nr` parameter which creates a running number for each group as a whole instead of a running number within the group.

### Additionally

* `if.()`, `export_with_style()`: Updated examples.
* `summarise_plus()`, `drop_type_vars()`: Updated documentation with a detailed description of the automatically generated variables TYPE, TYPE_NR and DEPTH when using the `nesting = "all"` or `nesting = "single"` option.


## R CMD check results

0 errors | 0 warnings | 0 note
