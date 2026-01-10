test_that("Example files load", {
    csv_file  <- system.file("extdata", "qol_example_data.csv",  package = "qol")
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    expect_true(file.exists(csv_file))
    expect_true(file.exists(xlsx_file))
})

###############################################################################
# Export
###############################################################################

export_df <- dummy_data(10)


test_that("CSV export works correctly", {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    export_df |> export_data(temp_file)

    expect_true(file.exists(temp_file))
})


test_that("XLSX export works correctly", {
    temp_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp_file), add = TRUE)

    export_df |> export_data(temp_file)

    expect_true(file.exists(temp_file))
})


test_that("Abort export with invalid path", {
    expect_message(export_df |> export_data(1),
                   " X ERROR: <Outfile> must be a single character. Export will be aborted.")
})


test_that("Abort export with non existing path", {
    expect_message(export_df |> export_data("Test"),
                   " X ERROR: Path does not exist:")
})


test_that("Warning in export on missing file extension", {
    temp_file <- tempfile(fileext = "")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file),
                   " ! WARNING: No file extension provided in <outfile>. 'csv' will be used.")

    expect_true(file.exists(paste0(temp_file, ".csv")))
})


test_that("Warning in export on invalid file extension", {
    temp_file <- tempfile(fileext = ".test")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file),
                   " ! WARNING: Only 'csv' or 'xlsx' are allowed as file extensions in the <outfile>. 'csv' will be used.")

    temp_file <- sub(".test", ".csv", temp_file, ignore.case = TRUE)
    expect_true(file.exists(temp_file))
})


test_that("Warning in export if separator is not provided as character", {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file, separator = 1),
                   " ! WARNING: <Separator> must be provided as character. ';' will be used.")

    expect_true(file.exists(temp_file))
})


test_that("Warning in export if separator is longer than one character", {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file, separator = ";;"),
                   " ! WARNING: <Separator> may only be one character. ';' will be used.")

    expect_true(file.exists(temp_file))
})


test_that("Warning in export if decimal is not provided as character", {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file, decimal = 1),
                   " ! WARNING: <Decimal> must be provided as character. ',' will be used.")

    expect_true(file.exists(temp_file))
})


test_that("Warning in export if decimal is longer than one character", {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file, decimal = ",,"),
                   " ! WARNING: <Decimal> may only be one character. ',' will be used.")

    expect_true(file.exists(temp_file))
})


test_that("Warning in export if separator and decimal are equal", {
    temp_file <- tempfile(fileext = ".csv")
    on.exit(unlink(temp_file), add = TRUE)

    expect_message(export_df |> export_data(temp_file, decimal = ";"),
                   " ! WARNING: <Decimal> may not be the same character as the <separator>. ',' will be used.")

    expect_true(file.exists(temp_file))
})

###############################################################################
# Import
###############################################################################


test_that("CSV import works correctly", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    infile <- import_data(csv_file)

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("CSV import without first row as column names", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    infile <- import_data(csv_file, var_names = FALSE)

    expect_equal(nrow(infile), 101)
})


test_that("XLSX import works correctly", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    infile <- import_data(xlsx_file)

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("XLSX import with specific region", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    infile <- import_data(xlsx_file, sheet = 2, region = "A1:C11")

    expect_equal(nrow(infile), 10)
    expect_equal(ncol(infile), 3)
})


test_that("XLSX import with named region", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    infile <- import_data(xlsx_file, region = "test_region")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("XLSX import without first row as column names", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    infile <- import_data(xlsx_file, var_names = FALSE)

    expect_equal(nrow(infile), 101)
})


test_that("Abort import with invalid path", {
    expect_message(import_data(1),
                   " X ERROR: <Infile> must be a single character. Import will be aborted.")
})


test_that("Abort import with non existing path", {
    expect_message(import_data("Test"),
                   " X ERROR: Path does not exist:")
})


test_that("Abort import on missing file extension", {
    csv_file <- sub(".csv", "", system.file("extdata", "qol_example_data.csv", package = "qol"))

    expect_message(infile <- import_data(csv_file),
                   " X ERROR: No file extension provided in <infile>. 'csv' and 'xlsx' are allowed.")
})


test_that("Abort import on invalid file extension", {
    csv_file <- sub(".csv", ".test", system.file("extdata", "qol_example_data.csv", package = "qol"))

    expect_message(infile <- import_data(csv_file),
                   " X ERROR: Only 'csv' or 'xlsx' are allowed as file extensions in the <infile>.")
})


test_that("Warning in import if separator is not provided as character", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    expect_message(infile <- import_data(csv_file, separator = 1),
                   " ! WARNING: <Separator> must be provided as character. Automatic detection will be used.")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if separator is longer than one character", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    expect_message(infile <- import_data(csv_file, separator = ";;"),
                   " ! WARNING: <Separator> may only be one character. Automatic detection will be used.")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if decimal is not provided as character", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    expect_message(infile <- import_data(csv_file, decimal = 1),
                   " ! WARNING: <Decimal> must be provided as character. Automatic detection will be used.")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if decimal is longer than one character", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    expect_message(infile <- import_data(csv_file, decimal = ",,"),
                   " ! WARNING: <Decimal> may only be one character. Automatic detection will be used.")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if separator and decimal are equal", {
    csv_file <- system.file("extdata", "qol_example_data.csv", package = "qol")

    expect_message(infile <- import_data(csv_file, separator = ";", decimal = ";"),
                   " ! WARNING: <Decimal> may not be the same character as the <separator>. Automatic detection will be used.")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if region is not provided as character", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    expect_message(infile <- import_data(xlsx_file, region = 1),
                             " ! WARNING: Region must be provided as character. Allowed are specific ranges like 'A1:BY27' or")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if region is a vector", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    expect_message(infile <- import_data(xlsx_file, region = c("A", "B")),
                             " ! WARNING: Only one character element allowed for region. The whole file will be read.")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})


test_that("Warning in import if region not found", {
    xlsx_file <- system.file("extdata", "qol_example_data.xlsx", package = "qol")

    expect_message(infile <- import_data(xlsx_file, region = "Test"),
                             " ! WARNING: Region 'Test' doesn't exist in sheet")

    expect_equal(nrow(infile), 100)
    expect_equal(ncol(infile), 11)
})
