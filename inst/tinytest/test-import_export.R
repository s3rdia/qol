set_no_print(TRUE)

# Example csv, txt and xlsx files load
csv_file  <- system.file("extdata", "qol_example_data_csv.csv",   package = "qol")
txt_file  <- system.file("extdata", "qol_example_data_txt.txt",   package = "qol")
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

expect_true(file.exists(csv_file), info = "Example csv, txt and xlsx files load")
expect_true(file.exists(txt_file), info = "Example csv, txt and xlsx files load")
expect_true(file.exists(xlsx_file), info = "Example csv, txt and xlsx files load")

###############################################################################
# Export
###############################################################################

export_df <- dummy_data(10)
export_list     <- list(first  = export_df,
                        second = export_df)
export_list_txt <- list(first.txt  = export_df,
                        second.txt = export_df)


# CSV export works correctly
temp_file <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file)

expect_true(file.exists(temp_file), info = "CSV export works correctly")


# TXT export works correctly
temp_file <- tempfile(fileext = ".txt")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file)

expect_true(file.exists(temp_file), info = "TXT export works correctly")


# XLSX export works correctly
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file)

expect_true(file.exists(temp_file), info = "XLSX export works correctly")


# Abort export with invalid path
export_df |> export_data(1)

expect_error(print_stack_as_messages("ERROR"), "<Outfile> must be a single character. Export will be aborted.", info = "Abort export with invalid path")


# Abort export with non existing path
export_df |> export_data("Test")

expect_error(print_stack_as_messages("ERROR"), "Path does not exist:", info = "Abort export with non existing path")


# Warning in export on missing file extension
temp_file <- tempfile(fileext = "")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file)

expect_warning(print_stack_as_messages("WARNING"), "No file extension provided in <outfile>. 'csv' will be used.", info = "Warning in export on missing file extension")

expect_true(file.exists(paste0(temp_file, ".csv")), info = "Warning in export on missing file extension")


# Warning in export on invalid file extension
temp_file <- tempfile(fileext = ".test")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file)

expect_warning(print_stack_as_messages("WARNING"), "Only 'csv', 'txt' or 'xlsx' are allowed as file extensions in the <outfile>. 'csv' will be used.", info = "Warning in export on invalid file extension")

temp_file <- sub(".test", ".csv", temp_file, ignore.case = TRUE)
expect_true(file.exists(temp_file), info = "Warning in export on invalid file extension")


# Warning in export if separator is not provided as character
temp_file <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file, separator = 1)

expect_warning(print_stack_as_messages("WARNING"), "<Separator> must be provided as character. ';' will be used.", info = "Warning in export if separator is not provided as character")

expect_true(file.exists(temp_file), info = "Warning in export if separator is not provided as character")


# Warning in export if separator is longer than one character
temp_file <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file, separator = ";;")

expect_warning(print_stack_as_messages("WARNING"), "<Separator> may only be one character. ';' will be used.", info = "Warning in export if separator is longer than one character")

expect_true(file.exists(temp_file), info = "Warning in export if separator is longer than one character")


# Warning in export if decimal is not provided as character
temp_file <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file, decimal = 1)

expect_warning(print_stack_as_messages("WARNING"), "<Decimal> must be provided as character. ',' will be used.", info = "Warning in export if decimal is not provided as character")

expect_true(file.exists(temp_file), info = "Warning in export if decimal is not provided as character")


# Warning in export if decimal is longer than one character
temp_file <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file, decimal = ",,")

expect_warning(print_stack_as_messages("WARNING"), "<Decimal> may only be one character. ',' will be used.", info = "Warning in export if decimal is longer than one character")

expect_true(file.exists(temp_file), info = "Warning in export if decimal is longer than one character")


# Warning in export if separator and decimal are equal
temp_file <- tempfile(fileext = ".csv")
on.exit(unlink(temp_file), add = TRUE)

export_df |> export_data(temp_file, decimal = ";")

expect_warning(print_stack_as_messages("WARNING"), "<Decimal> may not be the same character as the <separator>. ',' will be used.", info = "Warning in export if separator and decimal are equal")

expect_true(file.exists(temp_file), info = "Warning in export if separator and decimal are equal")


# Multi CSV export works correctly
file1 <- file.path(tempdir(), "first.csv")
file2 <- file.path(tempdir(), "second.csv")
on.exit(unlink(c(file1, file2)), add = TRUE)

export_multi(export_list, tempdir(), separator = ";")

expect_true(file.exists(file1), info = "Multi CSV export works correctly")
expect_true(file.exists(file2), info = "Multi CSV export works correctly")


# Multi TXT export works correctly
file1 <- file.path(tempdir(), "first.txt")
file2 <- file.path(tempdir(), "second.txt")
on.exit(unlink(c(file1, file2)), add = TRUE)

export_multi(export_list_txt, tempdir(), separator = ";")

expect_true(file.exists(file1), info = "Multi TXT export works correctly")
expect_true(file.exists(file2), info = "Multi TXT export works correctly")


# Multi XLSX export works correctly
file1 <- file.path(tempdir(), "first.xlsx")
file2 <- file.path(tempdir(), "second.xlsx")
on.exit(unlink(c(file1, file2)), add = TRUE)

export_multi(export_list, tempdir(), into_sheets = FALSE)

expect_true(file.exists(file1), info = "Multi XLSX export works correctly")
expect_true(file.exists(file2), info = "Multi XLSX export works correctly")


# Multi sheet export into one XLSX file works correctly
file <- file.path(tempdir(), "export_list.xlsx")
on.exit(unlink(file), add = TRUE)

export_multi(export_list, tempdir())

expect_true(file.exists(file), info = "Multi sheet export into one XLSX file works correctly")

###############################################################################
# Import
###############################################################################


# CSV import works correctly
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file)

expect_equal(nrow(infile), 100, info = "CSV import works correctly")
expect_equal(ncol(infile), 11, info = "CSV import works correctly")


# TXT import works correctly
txt_file <- system.file("extdata", "qol_example_data_txt.txt", package = "qol")

infile <- import_data(txt_file)

expect_equal(nrow(infile), 100, info = "TXT import works correctly")
expect_equal(ncol(infile), 11, info = "TXT import works correctly")


# CSV import without first row as column names
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file, var_names = FALSE)

expect_equal(nrow(infile), 101, info = "CSV import without first row as column names")


# XLSX import works correctly
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file)

expect_equal(nrow(infile), 100, info = "XLSX import works correctly")
expect_equal(ncol(infile), 11, info = "XLSX import works correctly")


# XLSX import with specific region
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file, sheet = 2, region = "A1:C11")

expect_equal(nrow(infile), 10, info = "XLSX import with specific region")
expect_equal(ncol(infile), 3, info = "XLSX import with specific region")


# XLSX import with named region
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file, region = "test_region")

expect_equal(nrow(infile), 100, info = "XLSX import with named region")
expect_equal(ncol(infile), 11, info = "XLSX import with named region")


# XLSX import without first row as column names
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file, var_names = FALSE)

expect_equal(nrow(infile), 101, info = "XLSX import without first row as column names")


# Abort import with invalid path
import_data(1)

expect_error(print_stack_as_messages("ERROR"), "<Infile> must be a single character. Import will be aborted.", info = "Abort import with invalid path")


# Abort import with non existing path
import_data("Test")

expect_error(print_stack_as_messages("ERROR"), "Path does not exist:", info = "Abort import with non existing path")


# Abort import on missing file extension
csv_file <- gsub(".csv", "", system.file("extdata", "qol_example_data_csv.csv", package = "qol"))

infile <- import_data(csv_file)

expect_error(print_stack_as_messages("ERROR"), "No file extension provided in <infile>. 'csv' and 'xlsx' are allowed.", info = "Abort import on missing file extension")


# Abort import on invalid file extension
csv_file <- gsub(".csv", ".test", system.file("extdata", "qol_example_data_csv.csv", package = "qol"))

infile <- import_data(csv_file)

expect_error(print_stack_as_messages("ERROR"), "Only 'csv', 'txt' or 'xlsx' are allowed as file extensions in the <infile>.", info = "Abort import on invalid file extension")


# Warning in import if separator is not provided as character
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file, separator = 1)

expect_warning(print_stack_as_messages("WARNING"), "<Separator> must be provided as character. Automatic detection will be used.", info = "Warning in import if separator is not provided as character")

expect_equal(nrow(infile), 100, info = "Warning in import if separator is not provided as character")
expect_equal(ncol(infile), 11, info = "Warning in import if separator is not provided as character")


# Warning in import if separator is longer than one character
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file, separator = ";;")

expect_warning(print_stack_as_messages("WARNING"), "<Separator> may only be one character. Automatic detection will be used.", info = "Warning in import if separator is longer than one character")

expect_equal(nrow(infile), 100, info = "Warning in import if separator is longer than one character")
expect_equal(ncol(infile), 11, info = "Warning in import if separator is longer than one character")


# Warning in import if decimal is not provided as character
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file, decimal = 1)

expect_warning(print_stack_as_messages("WARNING"), "<Decimal> must be provided as character. Automatic detection will be used.", info = "Warning in import if decimal is not provided as character")

expect_equal(nrow(infile), 100, info = "Warning in import if decimal is not provided as character")
expect_equal(ncol(infile), 11, info = "Warning in import if decimal is not provided as character")


# Warning in import if decimal is longer than one character
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file, decimal = ",,")

expect_warning(print_stack_as_messages("WARNING"), "<Decimal> may only be one character. Automatic detection will be used.", info = "Warning in import if decimal is longer than one character")

expect_equal(nrow(infile), 100, info = "Warning in import if decimal is longer than one character")
expect_equal(ncol(infile), 11, info = "Warning in import if decimal is longer than one character")


# Warning in import if separator and decimal are equal
csv_file <- system.file("extdata", "qol_example_data_csv.csv", package = "qol")

infile <- import_data(csv_file, separator = ";", decimal = ";")

expect_warning(print_stack_as_messages("WARNING"), "<Decimal> may not be the same character as the <separator>. Automatic detection will be used.", info = "Warning in import if separator and decimal are equal")

expect_equal(nrow(infile), 100, info = "Warning in import if separator and decimal are equal")
expect_equal(ncol(infile), 11, info = "Warning in import if separator and decimal are equal")


# Warning in import if region is not provided as character
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file, region = 1)

expect_warning(print_stack_as_messages("WARNING"), "Region must be provided as character. Allowed are specific ranges like 'A1:BY27' or", info = "Warning in import if region is not provided as character")

expect_equal(nrow(infile), 100, info = "Warning in import if region is not provided as character")
expect_equal(ncol(infile), 11, info = "Warning in import if region is not provided as character")


# Warning in import if region is a vector
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file, region = c("A", "B"))

expect_warning(print_stack_as_messages("WARNING"), "Only one character element allowed for region. The whole file will be read.", info = "Warning in import if region is a vector")

expect_equal(nrow(infile), 100, info = "Warning in import if region is a vector")
expect_equal(ncol(infile), 11, info = "Warning in import if region is a vector")


# Warning in import if region not found
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_data(xlsx_file, region = "Test")

expect_warning(print_stack_as_messages("WARNING"), "Region 'Test' doesn't exist in sheet", info = "Warning in import if region not found")

expect_equal(nrow(infile), 100, info = "Warning in import if region not found")
expect_equal(ncol(infile), 11, info = "Warning in import if region not found")


# Multi file import works correctly (single sheet)
csv_file  <- system.file("extdata", "qol_example_data_csv.csv",   package = "qol")
txt_file  <- system.file("extdata", "qol_example_data_txt.txt",   package = "qol")
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")
files <- c(csv_file, txt_file, xlsx_file)

infile <- import_multi(files, 1)

expect_inherits(infile, "list", info = "Multi file import works correctly (single sheet)")
expect_equal(length(infile), 3, info = "Multi file import works correctly (single sheet)")


# Multi file import works correctly (all sheets)
xlsx_file <- system.file("extdata", "qol_example_data_xlsx.xlsx", package = "qol")

infile <- import_multi(xlsx_file)

expect_inherits(infile, "list", info = "Multi file import works correctly (all sheets)")
expect_equal(length(infile), 2, info = "Multi file import works correctly (all sheets)")


set_no_print()
