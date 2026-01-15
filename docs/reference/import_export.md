# High Level Import From And Export To CSV And XLSX

`import_data()`: A wrapper for
[`data.table::fread()`](https://rdatatable.gitlab.io/data.table/reference/fread.html)
and
[`openxlsx2::wb_to_df()`](https://janmarvin.github.io/openxlsx2/reference/wb_to_df.html),
providing basic import functionality with minimal code.

`export_data()`: A wrapper for
[`data.table::fwrite()`](https://rdatatable.gitlab.io/data.table/reference/fwrite.html)
and
[`openxlsx2::wb_save()`](https://janmarvin.github.io/openxlsx2/reference/wb_save.html),
providing basic export functionality with minimal code.

## Usage

``` r
import_data(
  infile,
  sheet = 1,
  region = NULL,
  separator = "auto",
  decimal = "auto",
  var_names = TRUE
)

export_data(
  data_frame,
  outfile,
  separator = ";",
  decimal = ",",
  var_names = TRUE
)
```

## Arguments

- infile:

  Full file path with extension to a csv or xlsx file to be imported.

- sheet:

  Only used in xlsx import. Which sheet of the workbook to import.

- region:

  Only used in xlsx import. Can either be an 'Excel' range like
  'A1:BY27' or the name of a named region.

- separator:

  Only used in CSV-export. Defines the single character value separator.

- decimal:

  Only used in CSV-export. Defines the single character decimal
  character.

- var_names:

  TRUE by default. Whether to export variable names or not.

- data_frame:

  A data frame to export.

- outfile:

  Full file path with extension. Allowed extensions are ".csv" and
  ".xlsx".

## Value

Returns a data frame.

## Details

`import_data()` and `export_data()` are based on the 'SAS' procedures
Proc Import and Proc Export, which provide a very straight forward
syntax. While 'SAS' can import many different formats with these
procedures, these 'R' versions concentrate on importing CSV and XLSX
files.

The main goal here is to just provide as few as possible parameters to
tackle most of the imports and exports. These error handling also tries
to let an import and export happen, even though a parameter wasn't
provided in the correct way.

## See also

Functions that can export with style:
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md).

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md),
[`modify_number_formats()`](https://s3rdia.github.io/qol/reference/modify_number_formats.md).

Global style options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_variable_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_stat_labels()`](https://s3rdia.github.io/qol/reference/style_options.md).

## Examples

``` r
# Example files
csv_file  <- system.file("extdata", "qol_example_data.csv",  package = "qol")
xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

# Import: Provide full file path
my_csv  <- import_data(csv_file)
my_xlsx <- import_data(xlsx_file)

# Import specific regions
range_import <- import_data(xlsx_file, region = "B4:H32")
name_import  <- import_data(xlsx_file, region = "test_region")

# Import from another sheet
sheet_import <- import_data(xlsx_file, sheet = "Sheet 2")

# Example data frame
my_data <- dummy_data(100)

# Example export file paths
export_csv  <- tempfile(fileext = ".csv")
export_xlsx <- tempfile(fileext = ".xlsx")

# Export: Provide full file path
my_data |> export_data(export_csv)
my_data |> export_data(export_xlsx)

# Manual cleanup for example
unlink(c(export_csv, export_xlsx))
```
