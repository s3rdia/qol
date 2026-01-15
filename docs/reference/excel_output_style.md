# Style for 'Excel' Table Outputs

Set different options which define the visual output of 'Excel' tables
produced by
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md) and
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md).

## Usage

``` r
excel_output_style(
  save_path = NULL,
  file = NULL,
  sheet_name = "Table",
  font = "Arial",
  column_widths = "auto",
  row_heights = "auto",
  title_heights = NULL,
  header_heights = NULL,
  table_heights = NULL,
  footnote_heights = NULL,
  start_row = 2,
  start_column = 2,
  freeze_col_header = FALSE,
  freeze_row_header = FALSE,
  filters = TRUE,
  grid_lines = TRUE,
  header_back_color = "FFFFFF",
  header_font_color = "000000",
  header_font_size = 10,
  header_font_bold = TRUE,
  header_alignment = "center",
  header_wrap = "1",
  header_indent = 0,
  header_borders = TRUE,
  header_border_color = "000000",
  cat_col_back_color = "FFFFFF",
  cat_col_font_color = "000000",
  cat_col_font_size = 10,
  cat_col_font_bold = FALSE,
  cat_col_alignment = "left",
  cat_col_wrap = "1",
  cat_col_indent = 1,
  cat_col_borders = TRUE,
  cat_col_border_color = "000000",
  table_back_color = "FFFFFF",
  table_font_color = "000000",
  table_font_size = 10,
  table_font_bold = FALSE,
  table_alignment = "right",
  table_indent = 1,
  table_borders = FALSE,
  table_border_color = "000000",
  as_heatmap = FALSE,
  heatmap_low_color = "F8696B",
  heatmap_middle_color = "FFFFFF",
  heatmap_high_color = "63BE7B",
  box_back_color = "FFFFFF",
  box_font_color = "000000",
  box_font_size = 10,
  box_font_bold = TRUE,
  box_alignment = "center",
  box_wrap = "1",
  box_indent = 0,
  box_borders = TRUE,
  box_border_color = "000000",
  number_formats = number_format_style(),
  title_font_color = "000000",
  title_font_size = 10,
  title_font_bold = TRUE,
  title_alignment = "left",
  footnote_font_color = "000000",
  footnote_font_size = 8,
  footnote_font_bold = FALSE,
  footnote_alignment = "left",
  na_symbol = "."
)
```

## Arguments

- save_path:

  If NULL, opens the output as temporary file. Otherwise specify an
  output path.

- file:

  If NULL, opens the output as temporary file. Otherwise specify a
  filename with extension.

- sheet_name:

  Name of the sheet inside the workbook to which the output shall be
  written. If multiple outputs are produced in one go, the sheet name
  additionally receives a running number.

- font:

  Set the font to be used for the entire output.

- column_widths:

  Specify whether column widths should be set automatically and
  individually or if a numeric vector is passed each column width can be
  specified manually. If a table has more columns than column widths are
  provided, the last given column width will be repeated until the end
  of the table.

- row_heights:

  Specify whether row heights should be set automatically and
  individually or if a numeric vector is passed each row height can be
  specified manually. If a table has more rows than row heights are
  provided, the last given row height will be repeated until the end of
  the table.

- title_heights:

  Set individual row heights for the titles only.

- header_heights:

  Set individual row heights for the table header only.

- table_heights:

  Set individual row heights for the table body only.

- footnote_heights:

  Set individual row heights for the footnotes only.

- start_row:

  The row in which the table starts.

- start_column:

  The column in which the table starts.

- freeze_col_header:

  Whether to freeze the column header so that it is always visible while
  scrolling down the document.

- freeze_row_header:

  Whether to freeze the row header so that it is always visible while
  scrolling sideways in the document.

- filters:

  Whether to set filters in the column header, when exporting a data
  frame.

- grid_lines:

  Whether to show grid lines or not.

- header_back_color:

  Background cell color of the table header.

- header_font_color:

  Font color of the table header.

- header_font_size:

  Font size of the table header.

- header_font_bold:

  Whether to print the table header in bold letters.

- header_alignment:

  Set the text alignment of the table header.

- header_wrap:

  Whether to wrap the texts in the table header.

- header_indent:

  Indentation level of the table header.

- header_borders:

  Whether to draw borders around the table header cells.

- header_border_color:

  Borders colors of the table header cells.

- cat_col_back_color:

  Background cell color of the category columns inside the table.

- cat_col_font_color:

  Font color of the category columns inside the table.

- cat_col_font_size:

  Font size of the category columns inside the table.

- cat_col_font_bold:

  Whether to print the category columns inside the table in bold
  letters.

- cat_col_alignment:

  Set the text alignment of the category columns inside the table.

- cat_col_wrap:

  Whether to wrap the texts in the category columns inside the table.

- cat_col_indent:

  Indentation level of the category columns inside the table.

- cat_col_borders:

  Whether to draw borders around the category columns inside the table.

- cat_col_border_color:

  Borders colors of the category columns inside the table.

- table_back_color:

  Background color of the inner table cells.

- table_font_color:

  Font color of the inner table cells.

- table_font_size:

  Font size of the inner table cells.

- table_font_bold:

  Whether to print the inner table cells in bold numbers

- table_alignment:

  Set the text alignment of the inner table cells.

- table_indent:

  Indentation level of the inner table cells.

- table_borders:

  Whether to draw borders around the inner table cells.

- table_border_color:

  Borders colors of the inner table cells.

- as_heatmap:

  Whether to lay a conditional formatting over the values.

- heatmap_low_color:

  The color for lower values in the conditional formatting.

- heatmap_middle_color:

  The color for middle values in the conditional formatting.

- heatmap_high_color:

  The color for high values in the conditional formatting.

- box_back_color:

  Background color of the left box in table header.

- box_font_color:

  Font color of the left box in table header.

- box_font_size:

  Font size of the left box in table header.

- box_font_bold:

  Whether to print the left box in table header in bold letters.

- box_alignment:

  Set the text alignment of the left box in table header.

- box_wrap:

  Whether to wrap the texts in the left box in table header.

- box_indent:

  Indentation level of the left box in table header.

- box_borders:

  Whether to draw borders around the left box in table header.

- box_border_color:

  Borders colors of the left box in table header.

- number_formats:

  Put in a list of number formats which should be assigned to the
  different stats. Number formats can be created with
  [`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md).

- title_font_color:

  Font color of the titles.

- title_font_size:

  Font size of the tables titles.

- title_font_bold:

  Whether to print the tables titles in bold letters.

- title_alignment:

  Set the text alignment of the titles.

- footnote_font_color:

  Font color of the footnotes

- footnote_font_size:

  Font size of the tables footnotes

- footnote_font_bold:

  Whether to print the tables footnotes in bold letters.

- footnote_alignment:

  Set the text alignment of the footnotes.

- na_symbol:

  Define the symbol that should be used for NA values.

## Value

Returns a list of named style options.

## Details

`excel_output_style()` is based on the Output Delivery System (ODS) in
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
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
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

# Set specific options, the rest will be set to default values
excel_style <- excel_output_style(font       = "Calibri",
                                  sheet_name = "My_Output")

# For cells with no background color pass an empty string
excel_style <- excel_output_style(table_back_color = "")
```
