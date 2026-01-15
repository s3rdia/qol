# Number Formats Used by [`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md)

Set individual number formats for the different statistics in tables
produced with
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md).

## Usage

``` r
number_format_style(
  pct_excel = "0.0",
  freq_excel = "#,###,##0",
  freq.g0_excel = "#,###,##0",
  sum_excel = "#,###,##0",
  sum.wgt_excel = "#,###,##0",
  mean_excel = "#,###,##0",
  median_excel = "#,###,##0",
  mode_excel = "#,###,##0",
  min_excel = "#,###,##0",
  max_excel = "#,###,##0",
  sd_excel = "#,###,##0.000",
  variance_excel = "#,###,##0.000",
  first_excel = "#,###,##0",
  last_excel = "#,###,##0",
  p_excel = "#,###,##0",
  missing_excel = "#,###,##0",
  pct_decimals = 1,
  freq_decimals = 0,
  freq.g0_decimals = 0,
  sum_decimals = 3,
  sum.wgt_decimals = 3,
  mean_decimals = 2,
  median_decimals = 2,
  mode_decimals = 2,
  min_decimals = 2,
  max_decimals = 2,
  sd_decimals = 3,
  variance_decimals = 3,
  first_decimals = 0,
  last_decimals = 0,
  p_decimals = 2,
  missing_decimals = 0
)
```

## Arguments

- pct_excel:

  Number format for percentage applied in Excel workbook.

- freq_excel:

  Number format for frequency applied in Excel workbook.

- freq.g0_excel:

  Number format for frequency greater zero applied in Excel workbook.

- sum_excel:

  Number format for sum applied in Excel workbook.

- sum.wgt_excel:

  Number format for sum of weights applied in Excel workbook.

- mean_excel:

  Number format for mean applied in Excel workbook.

- median_excel:

  Number format for median applied in Excel workbook.

- mode_excel:

  Number format for mode applied in Excel workbook.

- min_excel:

  Number format for min applied in Excel workbook.

- max_excel:

  Number format for max applied in Excel workbook.

- sd_excel:

  Number format for sd applied in Excel workbook.

- variance_excel:

  Number format for variance applied in Excel workbook.

- first_excel:

  Number format for first applied in Excel workbook.

- last_excel:

  Number format for last applied in Excel workbook.

- p_excel:

  Number format for percentile applied in Excel workbook.

- missing_excel:

  Number format for missing applied in Excel workbook.

- pct_decimals:

  Number of decimals for percentage.

- freq_decimals:

  Number of decimals for frequency.

- freq.g0_decimals:

  Number of decimals for frequency greater zero.

- sum_decimals:

  Number of decimals for sum.

- sum.wgt_decimals:

  Number of decimals for sum of weights.

- mean_decimals:

  Number of decimals for mean.

- median_decimals:

  Number of decimals for median.

- mode_decimals:

  Number of decimals for mode.

- min_decimals:

  Number of decimals for min.

- max_decimals:

  Number of decimals for max.

- sd_decimals:

  Number of decimals for sd.

- variance_decimals:

  Number of decimals for variance.

- first_decimals:

  Number of decimals for first.

- last_decimals:

  Number of decimals for last.

- p_decimals:

  Number of decimals for percentile.

- missing_decimals:

  Number of decimals for missing.

## Value

Returns a list of named number format options.

## Details

`number_format_style()` is based on 'SAS' number formats and the Output
Delivery System (ODS), which provides efficient and readable ways to set
up different table styles.

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
[`modify_number_formats()`](https://s3rdia.github.io/qol/reference/modify_number_formats.md).

Global style options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_variable_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_stat_labels()`](https://s3rdia.github.io/qol/reference/style_options.md).

Functions that can handle styles:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md)

## Examples

``` r
# For default values
format_list <- number_format_style()

# Set specific options, the rest will be set to default values
format_list <- number_format_style(pct_excel    = "0.00000000",
                                   pct_decimals = 8)

# IMPORTANT: Don't forget to add individual formats to an excel style, otherwise
# they won't come into affect.
excel_style <- excel_output_style(number_formats = format_list)
```
