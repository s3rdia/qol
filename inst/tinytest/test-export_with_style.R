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
result_df <- dummy_df |> export_with_style(print = FALSE)

expect_inherits(result_df, "list", info = "Simplest form of export_with_style")
expect_equal(length(result_df), 3, info = "Simplest form of export_with_style")


# export_with_style with full styling
result_df <- dummy_df |>
      export_with_style(titles     = "Hello world link: https://cran.r-project.org/",
                        footnotes  = "This is a footnote link: https://cran.r-project.org/",
                        var_labels = list(weight_sum = "Test label"),
                        style      = excel_output_style(freeze_col_header = TRUE,
                                                        freeze_row_header = TRUE,
                                                        background_color  = "FF00FF"),
                        print = FALSE)

expect_inherits(result_df, "list", info = "export_with_style with full styling")
expect_equal(length(result_df), 3, info = "Simplest form of export_with_style")


# export_with_style with individual column alignment
result_df <- dummy_df |> export_with_style(column_align = c("right", "left"), print = FALSE)

expect_inherits(result_df, "list", info = "export_with_style with individual column alignment")
expect_equal(length(result_df), 3, info = "export_with_style with individual column alignment")



# export_with_style with fixed row headers
result_df <- dummy_df |>
      export_with_style(style = excel_output_style(freeze_row_header = TRUE),
                        print = FALSE)

expect_inherits(result_df, "list", info = "export_with_style with fixed row headers")
expect_equal(length(result_df), 3, info = "Simplest form of export_with_style")



# export_with_style with fixed column headers
result_df <- dummy_df |>
      export_with_style(style = excel_output_style(freeze_col_header = TRUE),
                        print = FALSE)

expect_inherits(result_df, "list", info = "export_with_style with fixed column headers")
expect_equal(length(result_df), 3, info = "Simplest form of export_with_style")


# Save export_with_style as Excel file
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

dummy_df |>
     export_with_style(style = excel_output_style(save_path = dirname(temp_file),
                                                  file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "Save export_with_style as Excel file")


# export_with_style can pass on workbooks
my_style <- excel_output_style(sheet_name = "tab1")

df1 <- dummy_df |>
    export_with_style(style = my_style,
                      print = FALSE)

my_style <- my_style |> modify_output_style(sheet_name = "tab2")

result_list <- dummy_df |>
    export_with_style(style    = my_style,
                      workbook = df1,
                      print    = FALSE)

expect_inherits(result_list, "list", info = "export_with_style can pass on workbooks")
expect_equal(length(result_list), 3, info = "export_with_style can pass on workbooks")


# export_with_style aborts, if invalid workbook is passed
result_list <- dummy_df |>
    export_with_style(workbook = list("test" = "test"),
                      print    = FALSE)

expect_error(print_stack_as_messages("ERROR"), "Workbook object is invalid. You have to provide a workbook object",
             info = "export_with_style aborts, if invalid workbook is passed")


set_style_options(as_heatmap = FALSE)
set_no_print()
