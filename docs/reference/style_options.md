# Set Global Styling Options For Excel Workbooks

Modify Styling options for Excel workbooks. Available parameters can be
seen in
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md)
or
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md).

`set_style_options()` sets the styling options for Excel workbooks
globally.These options are used by all tabulation and output functions,
which are capable of exporting styled outputs.

`get_style_options()` Prints out the currently set global styling
options.

`reset_style_options()` Resets global style options to the default
parameters. This includes all options set with `set_style_options()`,
`set_labels()`,
[`set_titles()`](https://s3rdia.github.io/qol/reference/qol_options.md)
and
[`set_footnotes()`](https://s3rdia.github.io/qol/reference/qol_options.md).

`close_file()` is a simple, more readable wrapper for setting file
parameter to NULL.

`set_labels()`: Can set variable and statistic labels globally so that
they don't have to be provided in every output function separately.

`get_labels()`: Get the globally stored variable and statistic labels as
a list and print the contents to the console.

## Usage

``` r
set_style_options(..., save_file = NULL)

get_style_options(from_file = NULL)

reset_style_options()

close_file()

set_labels(...)

get_labels()
```

## Arguments

- ...:

  `set_labels()`: Put in the variable names and their respective labels.

- save_file:

  A full file path to an RDS file in which global style options should
  be stored.

- from_file:

  A full file path to an RDS file in which global style options are
  stored.

## Value

`set_style_options()`: Returns modified global styling options.

`get_style_options()`: List of global styling options.

`reset_style_options()`: Returns default global styling options.

`close_file()`: List of global styling options with file = NULL.

`set_labels()`: List of variable and statistic labels.

`get_labels()`: List of variable and statistic labels.

## See also

Functions that use global styling options:
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md).

Functions that use global variable and statistic labels:
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md).

Functions that also use global variable labels:
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md).

## Examples

``` r
# This function can process any parameter from excel_output_style() or
# number_format_style() and sets the option globally.
set_style_options(save_path    = "C:/My Projects/",
                  sum_decimals = 8)

get_style_options()

# Reset all the style options including variable and statistic labels as well
# as titles and footnotes.
reset_style_options()

# Reset the file parameter
close_file()

# Set variable and statistic labels globally, retrieve and reset them
set_labels(age_gr = "Group of ages",
           status = "Current status",
           pct    = "%")

# To reset labels do this
set_labels(NULL)

get_labels()
```
