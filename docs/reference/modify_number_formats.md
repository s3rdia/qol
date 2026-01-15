# Modify Number Formats Used by [`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md)

Modify previously created number formats with
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md).

## Usage

``` r
modify_number_formats(formats_to_modify, ...)
```

## Arguments

- formats_to_modify:

  Pre created number formats where only certain elements should be
  modified while the rest is kept as is.

- ...:

  Pass in names and corresponding new values for existing number
  formats.

## Value

Returns a modified list of number format options.

## Details

`modify_number_formats()` is based on 'SAS' number formats and the
Output Delivery System (ODS), which provides efficient and readable ways
to set up different table styles.

With the number format style you have full control over formatting
numbers according to the different statistics. There is no need to think
about calculating the right place to input the number formats and how to
do this in a loop for multiple cells. Just input the different number
formats and decimals for the different statistics and everything else is
handled by the functions capable of using number styles.

The concept basically is: design over complex calculations.

## See also

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md).

Global style options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_variable_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_stat_labels()`](https://s3rdia.github.io/qol/reference/style_options.md).

Functions that can handle styles:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md).

## Examples

``` r
# For default values
format_list <- number_format_style(pct_excel    = "0.00000000",
                                   pct_decimals = 8)

# Set specific options, the rest will be kept as is
format_list <- format_list |> modify_number_formats(sum_excel = "#,###,##0.000")

# IMPORTANT: Don't forget to add individual formats to an excel style, otherwise
# they won't come into affect.
excel_style <- excel_output_style(number_formats = format_list)
```
