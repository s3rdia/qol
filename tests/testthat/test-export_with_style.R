###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

set_style_options(as_heatmap = TRUE)

dummy_df <- suppressMessages(dummy_data(100))
dummy_df[["weight_sum"]] <- dummy_df[["weight"]]


test_that("Simplest form of export_with_style", {
    result_df <- suppressMessages(dummy_df |>
            export_with_style(print = FALSE))

    expect_type(result_df, "environment")
})


test_that("export_with_style with full styling", {
    result_df <- suppressMessages(dummy_df |>
          export_with_style(titles     = "Hello world link: https://cran.r-project.org/",
                            footnotes  = "This is a footnote link: https://cran.r-project.org/",
                            var_labels = list(weight_sum = "Test label"),
                            style      = excel_output_style(freeze_col_header = TRUE,
                                                            freeze_row_header = TRUE),
                            print = FALSE))

    expect_type(result_df, "environment")
})



test_that("export_with_style with fixed row headers", {
    result_df <- suppressMessages(dummy_df |>
          export_with_style(style = excel_output_style(freeze_row_header = TRUE),
                            print = FALSE))

    expect_type(result_df, "environment")
})



test_that("export_with_style with fixed column headers", {
    result_df <- suppressMessages(dummy_df |>
          export_with_style(style = excel_output_style(freeze_col_header = TRUE),
                            print = FALSE))

    expect_type(result_df, "environment")
})


test_that("Save export_with_style as Excel file", {
    temp_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp_file), add = TRUE)

    suppressMessages(dummy_df |>
         export_with_style(style = excel_output_style(save_path = dirname(temp_file),
                                                      file      = basename(temp_file))))

    expect_true(file.exists(temp_file))
})


set_style_options(as_heatmap = FALSE)
