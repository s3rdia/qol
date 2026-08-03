# Create Table Of Contents For 'Excel' Workbooks

Creates a table of contents sheet with custom style based on the
provided 'Excel' workbook.

## Usage

``` r
create_table_of_contents(
  wb,
  style = .qol_options[["excel_style"]],
  toc_header = "Table of Contents",
  toc_sheet_name = "Contents",
  titles = c(),
  subheaders = list(),
  subheader_colors = c(),
  subheader_underline = FALSE,
  colored_tabs = FALSE,
  print = .qol_options[["print"]]
)
```

## Arguments

- wb:

  Workbook which receives the table of contents sheet.

- style:

  A list of options can be passed to control the appearance of the table
  of contents. Styles can be created with
  [`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md).

- toc_header:

  The main header.

- toc_sheet_name:

  The table of contents sheet name.

- titles:

  Input a vector of custom titles displayed along the sheet names.

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

## Value

Returns a modified 'Excel' workbook with table of contents sheet.

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

Combine Excel workbooks:
[`combine_into_workbook()`](https://s3rdia.github.io/qol/reference/combine_into_workbook.md).

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

``` r
# Example data frame
my_data <- dummy_data(10)

# First we create a workbook to play around with
# print = FALSE and output = "excel_nostyle".
# This skips the styling and output part, so that the function runs faster.
set_print(FALSE)
set_output("excel_nostyle")

set_style_options(sheet_name = "sheet1")
tab1 <- my_data |> export_with_style()

set_style_options(sheet_name = "sheet2")
tab2 <- my_data |> export_with_style()

set_style_options(sheet_name = "sheet3")
tab3 <- my_data |> export_with_style()

set_style_options(sheet_name = "sheet4")
tab4 <- my_data |> export_with_style()

set_style_options(sheet_name = "sheet5")
tab5 <- my_data |> export_with_style()

set_style_options(sheet_name = "sheet6")
tab6 <- my_data |> export_with_style()

wb <- combine_into_workbook(tab1, tab2, tab3, tab4, tab5, tab6)

# Now add a custom styled table of contents sheet
create_table_of_contents(wb,
                         toc_header     = "My Custom Header",
                         toc_sheet_name = "TOC",
                         titles         = c("A title for sheet number 1",
                                            "A second title, this one goes to number two",
                                            "And a third one",
                                            "By the way: If you don't input titles here",
                                            "The title column will be empty. See below."),
                         subheaders      = list("This is a subheadline" = "sheet1",
                                                "Another one"           = "sheet3",
                                                "And another one"       = "sheet5"),
                         subheader_underline = TRUE,
                         colored_tabs        = TRUE,
                         style               = excel_output_style(toc_header_font_size = 20,
                                                                  toc_other_font_size = 14))

# To save a table as xlsx file you have to set the path and filename in the
# style element
# Example files paths
workbook_file <- tempfile(fileext = ".xlsx")

set_style_options(save_path = dirname(workbook_file),
                  file      = basename(workbook_file))

create_table_of_contents(wb)

# Manual cleanup for example
unlink(workbook_file)

# Reset the global options afterwards
set_print(TRUE)
set_output("excel")
```
