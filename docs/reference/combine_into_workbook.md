# Combine Multiple Tables Into One Workbook

Combines any number of tables created with
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md)
into one workbook and styles them according to their meta information.

## Usage

``` r
combine_into_workbook(
  ...,
  output = "excel",
  style = .qol_options[["excel_style"]],
  table_of_contents = FALSE,
  toc_header = "Table of Contents",
  toc_sheet_name = "Contents",
  subheaders = list(),
  subheader_colors = c(),
  subheader_underline = FALSE,
  colored_tabs = FALSE,
  print = .qol_options[["print"]],
  monitor = .qol_options[["monitor"]]
)
```

## Arguments

- ...:

  Provide any number of result lists output by
  [`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md).

- output:

  The following output formats are available: excel and excel_nostyle.

- style:

  A list of options can be passed to control the appearance of the table
  of contents. Styles can be created with
  [`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md).

- table_of_contents:

  Whether to create a table of contents.

- toc_header:

  The main header.

- toc_sheet_name:

  The table of contents sheet name.

- subheaders:

  A list of custom subheaders. The entry names are the actual subheaders
  to be displayed, while the values are the sheet names at which the
  subheaders start.

- subheader_colors:

  Subheader background colors. These colors will also be used to color
  the tabs, if the option is activated.

- subheader_underline:

  FALSE by default. If TRUE underlines the subheaders.

- colored_tabs:

  FALSE by default. If TRUE colors the tabs according to the subheader
  colors.

- print:

  TRUE by default. If TRUE prints the output, if FALSE doesn't print
  anything. Can be used if one only wants to catch the combined
  workbook.

- monitor:

  FALSE by default. If TRUE outputs two charts to visualize the
  functions time consumption.

## Value

A fully styled workbook containing the provided tables.

## See also

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md),
[`modify_number_formats()`](https://s3rdia.github.io/qol/reference/modify_number_formats.md).

Global style options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_labels()`](https://s3rdia.github.io/qol/reference/style_options.md).

Other global options:
[`set_titles()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_footnotes()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_monitor()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_na.rm()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print_miss()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_output()`](https://s3rdia.github.io/qol/reference/qol_options.md).

Standalone table of contents:
[`create_table_of_contents()`](https://s3rdia.github.io/qol/reference/create_table_of_contents.md).

Creating formats:
[`discrete_format()`](https://s3rdia.github.io/qol/reference/formats.md)
and
[`interval_format()`](https://s3rdia.github.io/qol/reference/formats.md).

Functions that can handle formats and styles:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md).

Additional functions that can handle styles:
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md)

Additional functions that can handle formats:
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md),
[`recode.()`](https://s3rdia.github.io/qol/reference/recode.md),
[`recode_multi()`](https://s3rdia.github.io/qol/reference/recode.md),
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md),
[`sort_plus()`](https://s3rdia.github.io/qol/reference/sort_plus.md)

## Examples
