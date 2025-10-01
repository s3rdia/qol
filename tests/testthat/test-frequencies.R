###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Different way of passing variables in", {
    result_list1 <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    by        = sex,
                    weight    = weight,
                    print     = FALSE))

    result_list2 <- suppressMessages(dummy_df |>
        frequencies(variables = "age",
                    by        = "sex",
                    weight    = "weight",
                    print     = FALSE))

    expect_identical(result_list1, result_list2)
})


test_that("Simplest form of frequencies", {
    result_list <- suppressMessages(dummy_df |>
          frequencies(variables = sex,
                      print     = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)

    expect_true(all(c("variable", "mean", "sd", "min", "max", "freq", "miss")
                    %in% names(result_list[["mean"]])))
    expect_true(all(c("fused_vars", "TYPE", "TYPE_NR", "DEPTH",
                      "var_sum", "var_pct_group", "var_freq")
                    %in% names(result_list[["freq"]])))
})


test_that("frequencies with titles and footnotes", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = sex,
                        titles    = "Hello world",
                        footnotes = "This is a footnote",
                        print     = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("frequencies with multiple variables", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = c(sex, age),
                        print     = FALSE))

    expect_equal(nrow(result_list[["mean"]]), 2)
    expect_true(all(c("sex", "age") %in% result_list[["mean"]][["variable"]]))
})


test_that("Character variables won't be evaluated in mean tab", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = c(sex, education),
                        print     = FALSE))

    expect_equal(nrow(result_list[["mean"]]), 1)
    expect_true(!"education" %in% result_list[["mean"]][["variable"]])
})


test_that("frequencies with by variables", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = age,
                        by        = sex,
                        print     = FALSE))

    expect_true(all(c("by_vars", "BY") %in% names(result_list[["freq"]])))
})


test_that("frequencies with multiple by variables", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = c(age, education),
                        by        = c(sex, year),
                        print     = FALSE))

    expect_true(all(c("sex", "year") %in% result_list[["freq"]][["BY"]]))
})


test_that("frequencies where by is also part of freq variables is aborted", {
    expect_message(result_list <- dummy_df |>
        frequencies(variables = c(age, sex),
                    by        = c(sex, year),
                    print     = FALSE), " X ERROR: The provided by variable 'sex' is also part of")
})


test_that("frequencies with weighted results", {
    result_list1 <- suppressMessages(dummy_df |>
            frequencies(variables = age,
                        weight    = weight,
                        print     = FALSE))

    result_list2 <- suppressMessages(dummy_df |>
             frequencies(variables = age,
                         print     = FALSE))

    expect_false(identical(result_list1[["freq"]][["var_sum"]],
                           result_list2[["freq"]][["var_sum"]]))
})


test_that("frequencies with NAs removed", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = sex,
                        na.rm     = TRUE,
                        print     = FALSE))

    expect_true(sum(is.na(result_list[["freq"]][["fused_vars"]])) <= 1)
})


test_that("frequencies with character variable omits mean table", {
    result_list <- suppressMessages(dummy_df |>
            frequencies(variables = education,
                        print     = FALSE))

    expect_equal(nrow(result_list[[1]]), 0)
})


test_that("Apply single discrete labels", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_list <- suppressMessages(dummy_df |>
             frequencies(variables = sex,
                         formats   = list(sex = suppressMessages(discrete_format(
                            "Male"   = 1,
                            "Female" = 2))),
                         print     = FALSE))

    no_format_list <- suppressMessages(dummy_df |>
              frequencies(variables = age,
                          print     = FALSE))

    expect_equal(collapse::fsum(format_list[["freq"]][["var_sum"]]),
                 collapse::fsum(no_format_list[["freq"]][["var_sum"]]))
    expect_true(all(c("Male", "Female")
                    %in% format_list[["freq"]][["fused_vars"]]))
})


test_that("Apply single interval labels", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_list <- suppressMessages(dummy_df |>
            frequencies(variables = income,
                        formats   = list(income = suppressMessages(discrete_format(
                            "below 500"          = 0:499,
                            "500 to under 1000"  = 500:999,
                            "1000 to under 2000" = 1000:1999,
                            "2000 and more"      = 2000:99999))),
                        print     = FALSE))

    no_format_list <- suppressMessages(dummy_df |>
               frequencies(variables = age,
                           print     = FALSE))

    expect_equal(collapse::fsum(format_list[["freq"]][["var_sum"]]),
                 collapse::fsum(no_format_list[["freq"]][["var_sum"]]))
    expect_true(all(c("below 500", "2000 and more")
                    %in% format_list[["freq"]][["fused_vars"]]))
})


test_that("Apply discrete multilabel", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_list <- suppressMessages(dummy_df |>
            frequencies(variables = sex,
                        formats   = list(sex = suppressMessages(discrete_format(
                            "Total"  = 1:2,
                            "Male"   = 1,
                            "Female" = 2))),
                        print     = FALSE))

    expect_true(all(c("Total", "Male", "Female")
                    %in% format_list[["freq"]][["fused_vars"]]))
})


test_that("Apply interval multilabel", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    format_list <- suppressMessages(dummy_df |>
            frequencies(variables = income,
                        formats   = list(income = suppressMessages(interval_format(
                            "Total"              = 0:99999,
                            "below 500"          = 0:499,
                            "500 to under 1000"  = 500:999,
                            "1000 to under 2000" = 1000:1999,
                            "2000 and more"      = 2000:99999))),
                        print     = FALSE))

    no_format_list <- suppressMessages(dummy_df |>
               frequencies(variables = income,
                           print     = FALSE))

    expect_true(all(c("Total", "below 500", "2000 and more")
                    %in% format_list[["freq"]][["fused_vars"]]))
})

###############################################################################
# The Excel tests are kept simpler because the whole stat evaluation is the
# same as before. It serves mainly to let the code run through all the formatting
# lines.
###############################################################################

test_that("frequencies with excel output", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    output    = "excel",
                    print     = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("frequencies with titles and footnotes and weight", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = sex,
                    output    = "excel",
                    titles    = "Hello world",
                    footnotes = "This is a footnote",
                    weight    = weight,
                    print     = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("frequencies with excel output and by variables", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    by        = sex,
                    output    = "excel",
                    print     = FALSE))

    expect_true(all(c("by_vars", "BY") %in% names(result_list[["freq"]])))
})


test_that("frequencies with borders in excel output", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    output    = "excel",
                    print     = FALSE,
                    style = excel_output_style(header_borders   = TRUE,
                                               box_borders      = TRUE,
                                               cat_col_borders  = TRUE,
                                               table_borders    = TRUE,
                                               column_widths    = c(2, 3),
                                               row_heights      = c(2, 3))))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("frequencies with set column width/row heights", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    output    = "excel",
                    print     = FALSE,
                    style = excel_output_style(column_widths = 2,
                                               row_heights   = 3)))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("frequencies with set partial row heights", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    output    = "excel",
                    titles    = "Hello world",
                    footnotes = "This is a footnote",
                    print     = FALSE,
                    style = excel_output_style(title_heights    = c(10, 10),
                                               header_heights   = c(10, 10),
                                               table_heights    = c(10),
                                               footnote_heights = 10)))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("frequencies with fast none styled excel output", {
    result_list <- suppressMessages(dummy_df |>
        frequencies(variables = age,
                    output    = "excel_nostyle",
                    print     = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 2)
})


test_that("Invalid output format leads to console output", {
    expect_message(result_list <- dummy_df |>
            frequencies(variables = age,
                        by        = sex,
                        output    = "test",
                        print     = FALSE),
            " ! WARNING: Output format 'test' not available. Using 'console' instead.")
})
