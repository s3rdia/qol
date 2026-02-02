# Set Global Styling Options For Excel Workbooks

Modify Styling options for Excel workbooks. Available parameters can be
seen in
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md)
or
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md).

`set_style_options()` sets the styling options for Excel workbooks
globally.These options are used by all tabulation and output functions,
which are capable of exporting styled outputs.

`reset_style_options()` resets global style options to the default
parameters.

`get_style_options()` prints out the currently set global styling
options.

`close_file()` is a simple, more readable wrapper for setting file
parameter to NULL.

`set_variable_labels()`: Can set variable labels globally so that they
don't have to be provided in every output function separately.

`get_variable_labels()`: Get the globally stored variable labels.

`set_stat_labels()`: Can set statistic labels globally so that they
don't have to be provided in every output function separately.

`get_stat_labels()`: Get the globally stored statistic labels.

`reset_qol_options()` resets global options to the default parameters.

## Usage

``` r
set_style_options(...)

reset_style_options()

get_style_options()

close_file()

set_variable_labels(...)

get_variable_labels()

set_stat_labels(...)

get_stat_labels()

reset_qol_options()
```

## Arguments

- ...:

  `set_stat_labels()`: Put in the statistics and their respective
  labels.

## Value

`set_style_options()`: Returns modified global styling options.

`reset_style_options()`: Returns default global styling options.

`get_style_options()`: List of global styling options.

`close_file()`: List of global styling options with file = NULL.

`set_variable_labels()`: List of variable labels.

`get_variable_labels()`: List of variable labels.

`set_stat_labels()`: List of statistic labels.

`get_stat_labels()`: List of statistic labels.

`reset_qol_options()`: Returns default global options.

## See also

Functions that use global styling options:
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md).

Functions that also use global variable labels:
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md).

Functions that use global variable and statistic labels:
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md).

Functions that also use global variable labels:
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md).

## Examples

``` r
set_style_options(save_path    = "C:/My Projects/",
                  sum_decimals = 8)

reset_style_options()

get_style_options()

close_file()

set_variable_labels(age_gr = "Group of ages",
                    status = "Current status")

get_variable_labels()

set_stat_labels(pct  = "%",
                freq = "Count")

get_stat_labels()

reset_qol_options()
```
