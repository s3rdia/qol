###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

set_style_options(as_heatmap = TRUE)

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Different way of passing variables in", {
    result_df1 <- suppressMessages(dummy_df |>
         crosstabs(rows    = age,
                   columns = sex,
                   by      = education,
                   weight  = weight,
                   print   = FALSE))

    result_df2 <- suppressMessages(dummy_df |>
         crosstabs(rows    = "age",
                   columns = "sex",
                   by      = "education",
                   weight  = "weight",
                   print   = FALSE))

    expect_identical(result_df1, result_df2)
})


test_that("Simplest form of crosstabs", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = age,
                  columns = sex,
                  print   = FALSE))

    expect_s3_class(result_df, "data.table")

    values <- length(unique(dummy_df[["sex"]]))
    expect_equal(ncol(result_df), (values * 4) + 1)
})


test_that("crosstabs with titles and footnotes", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows      = age,
                  columns   = sex,
                  titles    = "Hello world",
                  footnotes = "This is a footnote",
                  print     = FALSE))

    expect_s3_class(result_df, "data.table")

    values <- length(unique(dummy_df[["sex"]]))
    expect_equal(ncol(result_df), (values * 4) + 1)
})


test_that("crosstabs not allowed with multiple row variables", {
    expect_message(result_df <- dummy_df |>
        crosstabs(rows    = c(age, state),
                  columns = sex,
                  print   = FALSE), " X ERROR: Only one variable for <rows> allowed. Crosstabs will be aborted.")
})


test_that("crosstabs not allowed with multiple column variables", {
    expect_message(result_df <- dummy_df |>
         crosstabs(rows    = sex,
                   columns = c(age, state),
                   print   = FALSE), " X ERROR: Only one variable for <columns> allowed. Crosstabs will be aborted.")
})


test_that("crosstabs with by variables", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = age,
                  columns = sex,
                  by      = education,
                  print   = FALSE))

    expect_true("BY" %in% names(result_df))
    expect_equal(length(unique(result_df[["BY"]])), 1)
})


test_that("crosstabs with multiple by variables", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = age,
                  columns = sex,
                  by      = c(education, year),
                  print   = FALSE))

    expect_true("BY" %in% names(result_df))
    expect_equal(length(unique(result_df[["BY"]])), 2)
})


test_that("crosstabs where by is also part of rows or columns is aborted", {
    expect_message(result_list <- dummy_df |>
           crosstabs(rows    = age,
                     columns = sex,
                     by      = c(sex, year),
                     print   = FALSE), " ! WARNING: The provided <by> variable '")
})


test_that("crosstabs with weighted results", {
    result_df1 <- suppressMessages(dummy_df |>
         crosstabs(rows    = age,
                   columns = sex,
                   weight  = weight,
                   print   = FALSE))

    result_df2 <- suppressMessages(dummy_df |>
         crosstabs(rows    = age,
                   columns = sex,
                   print   = FALSE))

    expect_false(identical(as.numeric(result_df1[["var_sum_1"]]),
                           as.numeric(result_df2[["var_sum_1"]])))
})


test_that("crosstabs with NAs removed", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = age,
                  columns = sex,
                  na.rm   = TRUE,
                  print   = FALSE))

    expect_true(sum(is.na(result_df[["fused_vars"]])) == 0)
})


test_that("crosstabs with multiple statistics", {
    result_df <- suppressMessages(dummy_df |>
          crosstabs(rows       = age,
                    columns    = sex,
                    statistics = c("sum", "freq", "pct_row", "pct_column", "pct_total"),
                    print      = FALSE))

    values <- length(unique(dummy_df[["sex"]]))
    expect_equal(ncol(result_df), (values * 4) + 1)
})


test_that("Apply single discrete labels", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = sex,
                  columns = age,
                  formats = list(sex = suppressMessages(discrete_format(
                      "Male"   = 1,
                      "Female" = 2))),
                  print   = FALSE))

    no_format_df <- suppressMessages(dummy_df |>
       crosstabs(rows    = sex,
                 columns = age,
                 print   = FALSE))

    expect_equal(collapse::fsum(format_df[["var_sum_1"]]),
                 collapse::fsum(no_format_df[["var_sum_1"]]))
    expect_true(all(c("Male", "Female")
                    %in% format_df[["sex"]]))
})


test_that("Apply discrete multilabel", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = sex,
                  columns = age,
                  formats = list(sex = suppressMessages(discrete_format(
                                      "Total"  = 1:2,
                                      "Male"   = 1,
                                      "Female" = 2)),
                                  age = suppressMessages(discrete_format(
                                      "Total"          = 0:100,
                                      "under 18"       = 0:17,
                                      "18 to under 25" = 18:24,
                                      "25 to under 55" = 25:54,
                                      "55 to under 65" = 55:64,
                                      "65 and older"   = 65:100))),
                  print   = FALSE))

    expect_true(all(c("Total", "Male", "Female")
                    %in% format_df[["sex"]]))
})


test_that("Apply interval multilabel", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = income,
                  columns = sex,
                  formats = list(income = suppressMessages(interval_format(
                        "Total"              = 0:99999,
                        "below 500"          = 0:499,
                        "500 to under 1000"  = 500:999,
                        "1000 to under 2000" = 1000:1999,
                        "2000 and more"      = 2000:99999))),
                  print   = FALSE))

    expect_true(all(c("Total", "below 500", "2000 and more")
                    %in% format_df[["income"]]))
})


test_that("crosstabs row multilabel leads to notification", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(format_df <- dummy_df |>
          crosstabs(rows    = sex,
                    columns = age,
                    formats = list(sex = suppressMessages(discrete_format(
                        "Total"  = 1:2,
                        "Male"   = 1,
                        "Female" = 2))),
                    statistics = c("pct_row"),
                    print   = FALSE), " ~ NOTE: The format for variable 'sex' is a multilabel")
})


test_that("crosstabs row multilabel leads to notification with by variables", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(format_df <- dummy_df |>
           crosstabs(rows    = sex,
                     columns = age,
                     by      = year,
                     formats = list(sex = suppressMessages(discrete_format(
                         "Total"  = 1:2,
                         "Male"   = 1,
                         "Female" = 2))),
                     statistics = c("pct_row"),
                     print   = FALSE), " ~ NOTE: The format for variable 'sex' is a multilabel")
})

###############################################################################
# The Excel tests are kept simpler because the whole stat evaluation is the
# same as before. It serves mainly to let the code run through all the formatting
# lines.
###############################################################################

test_that("crosstabs with excel output", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = age,
                  columns = sex,
                  output  = "excel",
                  print   = FALSE))

    expect_s3_class(result_df, "data.table")

    values <- length(unique(dummy_df[["sex"]]))
    expect_equal(ncol(result_df), (values * 4) + 1)
})


test_that("crosstabs with titles and footnotes and weight (excel)", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows      = age,
                  columns   = sex,
                  output    = "excel",
                  titles    = "Hello world",
                  footnotes = "This is a footnote",
                  weight    = weight,
                  print     = FALSE))

    expect_s3_class(result_df, "data.table")

    values <- length(unique(dummy_df[["sex"]]))
    expect_equal(ncol(result_df), (values * 4) + 1)
})


test_that("crosstabs with excel output and by variables (excel)", {
    result_df <- suppressMessages(dummy_df |>
        crosstabs(rows    = age,
                  columns = sex,
                  by      = education,
                  output  = "excel",
                  print   = FALSE))

    expect_true("BY" %in% names(result_df))
})


test_that("crosstabs with fast none styled excel output (excel)", {
    result_df <- suppressMessages(dummy_df |>
          crosstabs(rows    = age,
                    columns = sex,
                    output  = "excel_nostyle",
                    print   = FALSE))

    expect_s3_class(result_df, "data.table")

    values <- length(unique(dummy_df[["sex"]]))
    expect_equal(ncol(result_df), (values * 4) + 1)
})


test_that("crosstabs row multilabel leads to notification (excel)", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(format_df <- dummy_df |>
           crosstabs(rows    = sex,
                     columns = age,
                     formats = list(sex = suppressMessages(discrete_format(
                                         "Total"  = 1:2,
                                         "Male"   = 1,
                                         "Female" = 2)),
                                     age = suppressMessages(discrete_format(
                                         "Total"          = 0:100,
                                         "under 18"       = 0:17,
                                         "18 to under 25" = 18:24,
                                         "25 to under 55" = 25:54,
                                         "55 to under 65" = 55:64,
                                         "65 and older"   = 65:100))),
                     statistics = c("sum", "freq", "pct_row", "pct_column", "pct_total"),
                     output  = "excel",
                     print   = FALSE), " ~ NOTE: The format for variable 'sex' is a multilabel")
})


test_that("crosstabs row multilabel leads to notification (excel)", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(format_df <- dummy_df |>
           crosstabs(rows    = sex,
                     columns = age,
                     formats = list(sex = suppressMessages(discrete_format(
                         "Total"  = 1:2,
                         "Male"   = 1,
                         "Female" = 2))),
                     statistics = c("pct_row"),
                     output  = "excel",
                     print   = FALSE), " ~ NOTE: The format for variable 'sex' is a multilabel")
})


test_that("crosstabs row multilabel leads to notification with by variables (excel)", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(format_df <- dummy_df |>
           crosstabs(rows    = sex,
                     columns = age,
                     by      = year,
                     formats = list(sex = suppressMessages(discrete_format(
                         "Total"  = 1:2,
                         "Male"   = 1,
                         "Female" = 2))),
                     statistics = c("pct_row"),
                     output  = "excel",
                     print   = FALSE), " ~ NOTE: The format for variable 'sex' is a multilabel")
})


test_that("Invalid output format leads to console output", {
    expect_message(result_df <- dummy_df |>
           crosstabs(rows    = age,
                     columns = sex,
                     by      = education,
                     output  = "test",
                     print   = FALSE),
       " ! WARNING: <Output> format 'test' not available. Using 'console' instead.")
})


test_that("Save crosstabs as Excel file", {
    temp_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp_file), add = TRUE)

    suppressMessages(dummy_df |>
         crosstabs(rows    = age,
                   columns = sex,
                   output  = "excel",
                   style   = excel_output_style(save_path = dirname(temp_file),
                                                file      = basename(temp_file))))

    expect_true(file.exists(temp_file))
})


set_style_options(as_heatmap = FALSE)
