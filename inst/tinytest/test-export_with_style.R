set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

set_style_options(as_heatmap = TRUE)

dummy_df <- dummy_data(100)
dummy_df[["weight_sum"]] <- dummy_df[["weight"]]


# Simplest form of export_with_style
result_df <- dummy_df |>
        export_with_style(print = FALSE)

expect_inherits(result_df, c("wbWorkbook", "R6"), info = "Simplest form of export_with_style")


# export_with_style with full styling
result_df <- dummy_df |>
      export_with_style(titles     = "Hello world link: https://cran.r-project.org/",
                        footnotes  = "This is a footnote link: https://cran.r-project.org/",
                        var_labels = list(weight_sum = "Test label"),
                        style      = excel_output_style(freeze_col_header = TRUE,
                                                        freeze_row_header = TRUE),
                        print = FALSE)

expect_inherits(result_df, c("wbWorkbook", "R6"), info = "export_with_style with full styling")



# export_with_style with fixed row headers
result_df <- dummy_df |>
      export_with_style(style = excel_output_style(freeze_row_header = TRUE),
                        print = FALSE)

expect_inherits(result_df, c("wbWorkbook", "R6"), info = "export_with_style with fixed row headers")



# export_with_style with fixed column headers
result_df <- dummy_df |>
      export_with_style(style = excel_output_style(freeze_col_header = TRUE),
                        print = FALSE)

expect_inherits(result_df, c("wbWorkbook", "R6"), info = "export_with_style with fixed column headers")


# Save export_with_style as Excel file
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

dummy_df |>
     export_with_style(style = excel_output_style(save_path = dirname(temp_file),
                                                  file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "Save export_with_style as Excel file")


set_style_options(as_heatmap = FALSE)


set_no_print()
