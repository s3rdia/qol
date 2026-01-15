# Modify Style for 'Excel' Table Outputs

Modify a previously created style with
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md).

## Usage

``` r
modify_output_style(style_to_modify, ...)
```

## Arguments

- style_to_modify:

  A pre created style where only certain elements should be modified
  while the rest is kept as is.

- ...:

  Pass in names and corresponding new values for existing style
  elements.

## Value

Returns a modified list of named style options.

## Details

`modify_output_style()` is based on the Output Delivery System (ODS) in
'SAS', which provides efficient and readable ways to set up different
table styles.

With the output style you have full control over the table design. There
is no need to think about calculating the right place to input a
background color or a border of a certain type and how to do this in a
loop for multiple cells. Just input colors, borders, font styles, etc.
for the different table parts and everything else is handled by the
functions capable of using styles.

The concept basically is: design over complex calculations.

## See also

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md),
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
excel_style <- excel_output_style()

# Set specific options, the rest will be kept as is
excel_style <- excel_style |> modify_output_style(sheet_name      = "Sheet",
                                                  title_font_bold = FALSE)

# For cells with no background color pass an empty string
excel_style <- excel_style |> modify_output_style(table_back_color = "")
```
