set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

set_style_options(as_heatmap = TRUE)

dummy_df <- dummy_data(1000)


# Different way of passing variables in
result_list1 <- dummy_df |>
    frequencies(variables = age,
                by        = sex,
                weight    = weight,
                print     = FALSE)

result_list2 <- dummy_df |>
    frequencies(variables = "age",
                by        = "sex",
                weight    = "weight",
                print     = FALSE)

expect_identical(result_list1, result_list2, info = "Different way of passing variables in")


# Simplest form of frequencies
result_list <- dummy_df |>
      frequencies(variables = sex,
                  print     = FALSE)

expect_inherits(result_list, "list", info = "Simplest form of frequencies")
expect_equal(length(result_list), 2, info = "Simplest form of frequencies")

expect_true(is.null(result_list[["mean"]]), info = "Simplest form of frequencies")
expect_true(all(c("fused_vars", "TYPE", "TYPE_NR", "DEPTH",
                  "var_sum", "var_pct_group", "var_freq")
                %in% names(result_list[["freq"]])), info = "Simplest form of frequencies")


# Simplest form of frequencies with means
result_list <- dummy_df |>
        frequencies(variables = sex,
                    means     = TRUE,
                    print     = FALSE)

expect_true(all(c("variable", "mean", "sd", "min", "max", "freq", "miss")
                %in% names(result_list[["mean"]])), info = "Simplest form of frequencies with means")


# frequencies with titles and footnotes
result_list <- dummy_df |>
        frequencies(variables = sex,
                    titles    = "Hello world",
                    footnotes = "This is a footnote",
                    print     = FALSE)

expect_inherits(result_list, "list", info = "frequencies with titles and footnotes")
expect_equal(length(result_list), 2, info = "frequencies with titles and footnotes")


# frequencies with multiple variables
result_list <- dummy_df |>
        frequencies(variables = c(sex, age),
                    means     = TRUE,
                    print     = FALSE)

expect_true(all(c("sex", "age") %in% result_list[["freq"]][["TYPE"]]), info = "frequencies with multiple variables")


# frequencies with multiple variables and means
result_list <- dummy_df |>
        frequencies(variables = c(sex, age),
                    means     = TRUE,
                    print     = FALSE)

expect_equal(collapse::fnrow(result_list[["mean"]]), 2, info = "frequencies with multiple variables and means")
expect_true(all(c("sex", "age") %in% result_list[["mean"]][["variable"]]), info = "frequencies with multiple variables and means")


# Character variables won't be evaluated in mean tab
result_list <- dummy_df |>
        frequencies(variables = c(sex, education),
                    means     = TRUE,
                    print     = FALSE)

expect_equal(collapse::fnrow(result_list[["mean"]]), 1, info = "Character variables won't be evaluated in mean tab")
expect_true(!"education" %in% result_list[["mean"]][["variable"]], info = "Character variables won't be evaluated in mean tab")


# frequencies with by variables
result_list <- dummy_df |>
        frequencies(variables = age,
                    by        = sex,
                    print     = FALSE)

expect_true(all(c("by_vars", "BY") %in% names(result_list[["freq"]])), info = "frequencies with by variables")


# frequencies with multiple by variables
result_list <- dummy_df |>
        frequencies(variables = c(age, education),
                    by        = c(sex, year),
                    means     = TRUE,
                    print     = FALSE)

expect_true(all(c("sex", "year") %in% result_list[["freq"]][["BY"]]), info = "frequencies with multiple by variables")


# frequencies where by is also part of freq variables is aborted
result_list <- dummy_df |>
    frequencies(variables = c(age, sex),
                by        = c(sex, year),
                print     = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable '", info = "frequencies where by is also part of freq variables is aborted")


# frequencies throws a NOTE, if print_miss and means option are both TRUE
result_list <- dummy_df |>
        frequencies(variables  = age,
                    by         = sex,
                    print_miss = TRUE,
                    means      = TRUE,
                    print      = FALSE)

expect_message(print_stack_as_messages("NOTE"), "Wenn <print_miss> is TRUE, there will be no mean tables. <Means> is set to FALSE.",
               info = "frequencies throws a NOTE, if print_miss and means option are both TRUE")


# frequencies with weighted results
result_list1 <- dummy_df |>
        frequencies(variables = age,
                    weight    = weight,
                    print     = FALSE)

result_list2 <- dummy_df |>
         frequencies(variables = age,
                     print     = FALSE)

expect_false(identical(result_list1[["freq"]][["var_sum"]],
                       result_list2[["freq"]][["var_sum"]]), info = "frequencies with weighted results")


# frequencies with NAs removed
result_list <- dummy_df |>
        frequencies(variables = sex,
                    na.rm     = TRUE,
                    print     = FALSE)

expect_true(sum(is.na(result_list[["freq"]][["fused_vars"]])) <= 1, info = "frequencies with NAs removed")


# frequencies with character variable omits mean table
result_list <- dummy_df |>
        frequencies(variables = education,
                    means     = TRUE,
                    print     = FALSE)

expect_true(is.null(collapse::fnrow(result_list[["means"]])), info = "frequencies with character variable omits mean table")


# Apply single discrete labels
format_list <- dummy_df |>
         frequencies(variables = sex,
                     formats   = list(sex = discrete_format(
                        "Male"   = 1,
                        "Female" = 2)),
                     print     = FALSE)

no_format_list <- dummy_df |>
          frequencies(variables = age,
                      print     = FALSE)

expect_equal(collapse::fsum(format_list[["freq"]][["var_sum"]]),
             collapse::fsum(no_format_list[["freq"]][["var_sum"]]), info = "Apply single discrete labels")
expect_true(all(c("Male", "Female")
                %in% format_list[["freq"]][["fused_vars"]]), info = "Apply single discrete labels")


# Apply single interval labels
format_list <- dummy_df |>
        frequencies(variables = income,
                    formats   = list(income = discrete_format(
                        "below 500"          = 0:499,
                        "500 to under 1000"  = 500:999,
                        "1000 to under 2000" = 1000:1999,
                        "2000 and more"      = 2000:99999)),
                    print     = FALSE)

no_format_list <- dummy_df |>
           frequencies(variables = age,
                       print     = FALSE)

expect_equal(collapse::fsum(format_list[["freq"]][["var_sum"]]),
             collapse::fsum(no_format_list[["freq"]][["var_sum"]]), info = "Apply single interval labels")
expect_true(all(c("below 500", "2000 and more")
                %in% format_list[["freq"]][["fused_vars"]]), info = "Apply single interval labels")


# Apply discrete multilabel
format_list <- dummy_df |>
        frequencies(variables = sex,
                    formats   = list(sex = discrete_format(
                        "Total"  = 1:2,
                        "Male"   = 1,
                        "Female" = 2)),
                    print     = FALSE)

expect_true(all(c("Total", "Male", "Female")
                %in% format_list[["freq"]][["fused_vars"]]), info = "Apply discrete multilabel")


# Apply interval multilabel
format_list <- dummy_df |>
        frequencies(variables = income,
                    formats   = list(income = interval_format(
                        "Total"              = 0:99999,
                        "below 500"          = 0:499,
                        "500 to under 1000"  = 500:999,
                        "1000 to under 2000" = 1000:1999,
                        "2000 and more"      = 2000:99999)),
                    print     = FALSE)

no_format_list <- dummy_df |>
           frequencies(variables = income,
                       print     = FALSE)

expect_true(all(c("Total", "below 500", "2000 and more")
                %in% format_list[["freq"]][["fused_vars"]]), info = "Apply interval multilabel")

###############################################################################
# The Excel tests are kept simpler because the whole stat evaluation is the
# same as before. It serves mainly to let the code run through all the formatting
# lines.
###############################################################################

# frequencies with excel output
result_list <- dummy_df |>
    frequencies(variables = age,
                means     = TRUE,
                output    = "excel",
                print     = FALSE)

expect_inherits(result_list, "list", info = "frequencies with excel output")
expect_equal(length(result_list), 2, info = "frequencies with excel output")


# frequencies with titles and footnotes and weight
result_list <- dummy_df |>
    frequencies(variables = sex,
                output    = "excel_nostyle",
                titles    = "Hello world",
                footnotes = "This is a footnote",
                weight    = weight,
                print     = FALSE)

expect_inherits(result_list, "list", info = "frequencies with titles and footnotes and weight")
expect_equal(length(result_list), 2, info = "frequencies with titles and footnotes and weight")


# frequencies with excel output and by variables
result_list <- dummy_df |>
    frequencies(variables  = age,
                by         = sex,
                output     = "excel",
                print      = FALSE)

expect_true(all(c("by_vars", "BY") %in% names(result_list[["freq"]])), info = "frequencies with excel output and by variables")


# frequencies with excel output and by variables and print_miss
result_list <- dummy_df |>
    frequencies(variables  = age,
                by         = sex,
                print_miss = TRUE,
                means      = TRUE,
                output     = "excel_nostyle",
                print      = FALSE)

expect_true(all(c("by_vars", "BY") %in% names(result_list[["freq"]])), info = "frequencies with excel output and by variables and print_miss")


# frequencies with borders in excel output
result_list <- dummy_df |>
    frequencies(variables = age,
                output    = "excel",
                print     = FALSE,
                style = excel_output_style(header_borders   = TRUE,
                                           box_borders      = TRUE,
                                           cat_col_borders  = TRUE,
                                           table_borders    = TRUE,
                                           column_widths    = c(2, 3),
                                           row_heights      = c(2, 3)))

expect_inherits(result_list, "list", info = "frequencies with borders in excel output")
expect_equal(length(result_list), 2, info = "frequencies with borders in excel output")


# frequencies with set column width/row heights
result_list <- dummy_df |>
    frequencies(variables = age,
                output    = "excel",
                print     = FALSE,
                style = excel_output_style(column_widths = 2,
                                           row_heights   = 3))

expect_inherits(result_list, "list", info = "frequencies with set column width/row heights")
expect_equal(length(result_list), 2, info = "frequencies with set column width/row heights")


# frequencies with set partial row heights
result_list <- dummy_df |>
    frequencies(variables = age,
                output    = "excel",
                titles    = "Hello world",
                footnotes = "This is a footnote",
                print     = FALSE,
                style = excel_output_style(title_heights    = c(10, 10),
                                           header_heights   = c(10, 10),
                                           table_heights    = c(10),
                                           footnote_heights = 10))

expect_inherits(result_list, "list", info = "frequencies with set partial row heights")
expect_equal(length(result_list), 2, info = "frequencies with set partial row heights")


# frequencies with fast none styled excel output
result_list <- dummy_df |>
    frequencies(variables = age,
                output    = "excel_nostyle",
                print     = FALSE)

expect_inherits(result_list, "list", info = "frequencies with fast none styled excel output")
expect_equal(length(result_list), 2, info = "frequencies with fast none styled excel output")


# Invalid output format leads to console output
result_list <- dummy_df |>
        frequencies(variables = age,
                    by        = sex,
                    output    = "test",
                    print     = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "<Output> format 'test' not available. Using 'console' instead.", info = "Invalid output format leads to console output")


# Save frequencies as Excel file
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

dummy_df |>
     frequencies(variables = age,
                 output    = "excel",
                 style     = excel_output_style(save_path = dirname(temp_file),
                                                file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "Save frequencies as Excel file")


set_style_options(as_heatmap = FALSE)


set_no_print()
