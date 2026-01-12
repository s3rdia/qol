###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

set_style_options(as_heatmap = TRUE)

dummy_df  <- suppressMessages(dummy_data(1000))
dummy_big <- suppressMessages(dummy_data(10000))

dummy_df[["binary"]] <- replicate(nrow(dummy_df), {
    paste0(sample(0:1, 2, replace = TRUE), collapse = "")
})

sum_df   <- suppressMessages(dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = weight,
                   statistics = c("sum"),
                   nesting    = "deepest",
                   na.rm      = TRUE))

sum_df2  <- suppressMessages(dummy_df |>
    summarise_plus(class      = c(year, sex, age),
                   values     = weight,
                   statistics = c("sum"),
                   nesting    = "deepest",
                   na.rm      = TRUE))


test_that("Simplest form of any_table", {
    result_list <- suppressMessages(dummy_big |>
          any_table(rows    = "age",
                    columns = "sex",
                    values  = weight,
                    print   = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
    expect_equal(names(result_list[[1]]), c("row.label", "var1", "weight_sum_1",
                                            "weight_sum_2", "weight_sum_NA"))
    expect_equal(result_list[[1]][["var1"]], c(paste0(0:99), "."))
})


test_that("any_table with combinations", {
    result_list <- suppressMessages(dummy_df |>
          any_table(rows    = "age + education",
                    columns = "sex + year",
                    values  = income,
                    weight  = weight,
                    print   = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table with multiple combinations", {
    result_list <- suppressMessages(dummy_df |>
          any_table(rows    = c("age", "age + education"),
                    columns = c("sex + year", "sex"),
                    values  = weight,
                    print   = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table many combinations don't break", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = c("age", "age + education", "state",
                                  "state + age", "education + age"),
                      columns = c("year", "sex + year", "sex"),
                      values  = weight,
                      print   = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table with titles and footnotes", {
    result_list <- suppressMessages(dummy_df |>
          any_table(rows      = "age",
                    columns   = "sex",
                    values    = weight,
                    titles    = "Hello world link: https://cran.r-project.org/",
                    footnotes = "This is a footnote link: https://cran.r-project.org/",
                    print     = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table with variable and stat labels", {
    result_list <- suppressMessages(dummy_df |>
          any_table(rows        = "age",
                    columns     = "sex",
                    values      = weight,
                    var_labels  = list(age = "Single ages", sex = "Sex", weight = "Population"),
                    stat_labels = list(sum = "Counts"),
                    box         = "Test",
                    print       = FALSE))

    expect_true("Single ages" %in% result_list[["table"]][["row.label"]])
})


test_that("any_table with removed variable and stat labels", {
    result_list <- suppressMessages(dummy_df |>
          any_table(rows        = "age",
                    columns     = "sex",
                    values      = weight,
                    var_labels  = list(age = "", sex = "", weight = ""),
                    stat_labels = list(sum = ""),
                    print       = FALSE))

    expect_true(!"row.label" %in% names(result_list[["table"]]))
})


test_that("any_table with different percentages", {
    result_list <- suppressMessages(dummy_df |>
          any_table(rows       = "age",
                    columns    = "sex",
                    values     = c(probability, weight),
                    statistics = c("sum", "pct_group", "pct_total", "pct_value"),
                    pct_group  = c("age", "sex"),
                    pct_value  = list(rate = "probability / weight"),
                    print      = FALSE))

    expect_true(all(c("weight_pct_group_age_1", "weight_pct_total_1",
                      "rate_pct_value_1") %in% names(result_list[["table"]])))
})


test_that("any_table with a lot of statistics doesn't break", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows       = "age",
                      columns    = "sex",
                      values     = weight,
                      statistics = c("freq", "freq_g0", "mean", "median", "mode", "min", "max",
                                     "first", "last", "sum_wgt", "p1", "p99", "sd", "variance",
                                     "missing"),
                      print      = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table with interleaved order", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows       = "age",
                      columns    = "sex",
                      values     = weight,
                      statistics = c("sum", "freq", "missing"),
                      order_by   = "interleaved",
                      print      = FALSE))

    expect_equal(names(result_list[[1]]),
                       c("row.label", "var1", "weight_sum_1", "weight_freq_1", "weight_missing_1",
                         "weight_sum_2", "weight_freq_2", "weight_missing_2", "weight_sum_NA", "weight_freq_NA",
                         "weight_missing_NA"))
})


test_that("any_table with values order", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows       = "age",
                      columns    = c("sex", "year"),
                      values     = c(weight, income),
                      statistics = "sum",
                      order_by   = "values",
                      print      = FALSE))

    year <- as.integer(format(Sys.Date(), "%Y"))

    expect_equal(names(result_list[[1]]),
                 c("row.label", "var1", "weight_sum_1", "weight_sum_2", "weight_sum_NA",
                   paste0("weight_sum_", year - 2), paste0("weight_sum_", year - 1), "income_sum_1", "income_sum_2",
                   "income_sum_NA", paste0("income_sum_", year - 2), paste0("income_sum_", year - 1)))
})


test_that("any_table with columns order", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows       = "age",
                      columns    = c("sex", "year"),
                      values     = c(weight, income),
                      statistics = "sum",
                      order_by   = "columns",
                      print      = FALSE))

    year <- as.integer(format(Sys.Date(), "%Y"))

    expect_equal(names(result_list[[1]]),
                 c("row.label", "var1", "weight_sum_1", "weight_sum_2", "weight_sum_NA",
                   "income_sum_1", "income_sum_2", "income_sum_NA", paste0("weight_sum_", year - 2),
                   paste0("weight_sum_", year - 1), paste0("income_sum_", year - 2), paste0("income_sum_", year - 1)))
})


test_that("any_table with by variables", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      by      = education,
                      print   = FALSE))

    expect_true("BY" %in% names(result_list[[1]]))
    expect_equal(length(unique(result_list[[1]][["BY"]])), 1)
})


test_that("any_table with multiple by variables", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      by      = c(education, year),
                      print   = FALSE))

    expect_true("BY" %in% names(result_list[["table"]]))
    expect_equal(length(unique(result_list[["table"]][["BY"]])), 2)
})


test_that("any_table with by variables and multiple row and column variables", {
    result_list <- dummy_df |>
           any_table(rows    = c("first_person", "state", "first_person + state"),
                     columns = c("sex", "education", "sex + education"),
                     values  = weight,
                     by      = year,
                     print   = FALSE,
                     na.rm   = TRUE)

    expect_true("BY" %in% names(result_list[["table"]]))
})


test_that("any_table with by variables aborts if by is also part of rows or columns", {
    expect_message(result_list <- dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      by      = sex,
                      print   = FALSE), " X ERROR: The provided <by> variable 'sex' is also part of")
})


test_that("any_table with NAs removed", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      na.rm   = TRUE,
                      print   = FALSE))

    expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0)
})


test_that("any_table with applied single discrete labels", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      formats = list(age = suppressMessages(discrete_format(
                          "under 18"       = 0:17,
                          "18 to under 25" = 18:24,
                          "25 to under 55" = 25:54,
                          "55 to under 65" = 55:64,
                          "65 and older"   = 65:100))),
                      print   = FALSE))

    expect_true(all(c("under 18", "18 to under 25", "25 to under 55",
                      "55 to under 65", "65 and older")
                    %in% result_list[["table"]][["var1"]]))
})


test_that("any_table with applied discrete multilabels", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
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

    expect_true(all(c("under 18", "18 to under 25", "25 to under 55",
                      "55 to under 65", "65 and older")
                    %in% result_list[["table"]][["var1"]]))
})


test_that("any_table with applied interval multilabels", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "income",
                      columns = "sex",
                      values  = weight,
                      formats = list(sex = suppressMessages(discrete_format(
                          "Total"  = 1:2,
                          "Male"   = 1,
                          "Female" = 2)),
                          income = suppressMessages(interval_format(
                              "Total"              = 0:99999,
                              "below 500"          = 0:499,
                              "500 to under 1000"  = 500:999,
                              "1000 to under 2000" = 1000:1999,
                              "2000 and more"      = 2000:99999))),
                      print   = FALSE))

    expect_true(all(c("Total", "below 500", "2000 and more")
                    %in% result_list[["table"]][["var1"]]))
})


test_that("any_table able to apply format on numeric values stored as character (short route)", {
    binary. <- discrete_format(
        "binary1" = c("00", "01"),
        "binary2" = c("10", "11"))

    result_list <- suppressMessages(dummy_df |>
        any_table(rows    = "binary",
                  columns = "sex",
                  values  = weight,
                  formats = list(binary = binary.),
                  print   = FALSE))

    expect_equal(result_list[[1]][["var1"]], c("binary1", "binary2"))
})


test_that("any_table converts numeric values stored as character to numeric, if no format is applied (short route)", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "binary",
                      columns = "sex",
                      values  = weight,
                      print   = FALSE))

    expect_equal(result_list[[1]][["var1"]], c("0", "1", "10", "11"))
})


test_that("any_table able to apply format on numeric values stored as character (long route)", {
    binary. <- discrete_format(
        "binary1" = c("00", "01"),
        "binary2" = c("10", "11"))

    result_list <- suppressMessages(dummy_df |>
            any_table(rows       = "binary",
                      columns    = "sex",
                      statistics = "mean",
                      values     = weight,
                      formats    = list(binary = binary.),
                      print      = FALSE))

    expect_equal(result_list[[1]][["var1"]], c("binary1", "binary2"))
})


test_that("any_table converts numeric values stored as character to numeric, if no format is applied (long route)", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows       = "binary",
                      columns    = "sex",
                      statistics = "mean",
                      values     = weight,
                      print      = FALSE))

    expect_equal(result_list[[1]][["var1"]], c("0", "1", "10", "11"))
})


test_that("any_table with fixed column headers", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      style   = excel_output_style(freeze_col_header = TRUE),
                      na.rm   = TRUE,
                      print   = FALSE))

    expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0)
})


test_that("any_table with fixed row headers", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      style   = excel_output_style(freeze_row_header = TRUE),
                      na.rm   = TRUE,
                      print   = FALSE))

    expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0)
})


test_that("any_table with fixed column and row headers", {
    result_list <- suppressMessages(dummy_df |>
            any_table(rows    = "age",
                      columns = "sex",
                      values  = weight,
                      style   = excel_output_style(freeze_col_header = TRUE,
                                                   freeze_row_header = TRUE),
                      na.rm   = TRUE,
                      print   = FALSE))

    expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0)
})


test_that("any_table warning with wrong output format", {
    expect_message(result_list <- dummy_df |>
                       any_table(rows    = "age",
                                 columns = "sex",
                                 values  = weight,
                                 output  = "Test",
                                 print   = FALSE), " ! WARNING: <Output> format 'Test' not available.")
})


test_that("any_table warning with wrong output format", {
    expect_message(result_list <- dummy_df |>
                       any_table(rows     = "age",
                                 columns  = "sex",
                                 values   = weight,
                                 order_by = "test",
                                 print    = FALSE), " ! WARNING: <Order by> option 'test' doesn't exist")
})


test_that("any_table with pre summarised data", {
    result_list <- suppressMessages(sum_df |>
       any_table(rows       = "year",
                 columns    = "sex",
                 values     = weight_sum,
                 print      = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table with pre summarised data and by variables", {
    result_list <- suppressMessages(sum_df2 |>
                any_table(rows       = "year",
                          columns    = "age",
                          by         = "sex",
                          values     = weight_sum,
                          print      = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table with no column variables", {
    result_list <- suppressMessages(dummy_df |>
           any_table(rows    = "age",
                     values  = weight,
                     print   = FALSE))

    expect_type(result_list, "list")
    expect_equal(length(result_list), 3)
})


test_that("any_table throws a warning, if invalid statistic specified", {
    expect_message(result_list <- dummy_df |>
               any_table(rows       = "age",
                         columns    = "sex",
                         values     = weight,
                         statistics = c("test", "sum"),
                         print      = FALSE), " ! WARNING: <Statistic> 'test' is invalid and will be omitted.")
})


test_that("any_table throws a warning, if no valid statistic specified", {
    expect_message(result_list <- dummy_df |>
               any_table(rows       = "age",
                         columns    = "sex",
                         values     = weight,
                         statistics = "test",
                         print      = FALSE), " ! WARNING: No valid <statistic> selected. 'sum' will be used.")
})


test_that("Save any_table as Excel file", {
    temp_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp_file), add = TRUE)

    suppressMessages(dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  style   = excel_output_style(save_path = dirname(temp_file),
                                               file      = basename(temp_file))))

    expect_true(file.exists(temp_file))
})

###############################################################################
# Abort checks
###############################################################################

test_that("any_table aborts, if column contains a row variable", {
    expect_message(result_list <- dummy_df |>
                       any_table(rows       = "sex",
                                 columns    = "sex",
                                 statistics = c("sum"),
                                 print      = FALSE), " X ERROR: The provided <columns> variable '")
})

test_that("any_table aborts with duplicate column names after pivot", {
    expect_message(result_list <- dummy_df |>
        any_table(rows    = "education",
                  columns = c("sex + age", "age + sex"),
                  values  = weight,
                  print   = FALSE), " X ERROR: Duplicate <columns> names found")
})


test_that("any_table aborts with none existent row variable", {
    expect_message(result_list <- dummy_df |>
          any_table(rows    = c("age", "age + test"),
                    columns = "sex",
                    values  = weight,
                    print   = FALSE), " X ERROR: The provided <rows> variable 'test' is not part of")
})


test_that("any_table aborts with none existent column variable", {
    expect_message(result_list <- dummy_df |>
          any_table(rows    = "age",
                    columns = c("sex + test"),
                    values  = weight,
                    print   = FALSE), " X ERROR: The provided <columns> variable 'test' is not part of")
})


test_that("any_table aborts with no valid row variables", {
    expect_message(result_list <- dummy_df |>
           any_table(rows    = "",
                     columns = "sex",
                     values  = weight,
                     print   = FALSE), " X ERROR: No valid <rows> variables provided")
})


test_that("any_table aborts with invalid by variable", {
    expect_message(result_list <- dummy_df |>
           any_table(rows    = "age",
                     columns = "sex",
                     by      = "test",
                     values  = weight,
                     print   = FALSE), " ! WARNING: The provided <by> variable 'test' is not part of")
})


test_that("any_table aborts with invalid values", {
    expect_message(result_list <- dummy_df |>
           any_table(rows    = "age",
                     columns = "sex",
                     values  = "",
                     print   = FALSE), " X ERROR: No <values> provided.")
})


test_that("any_table aborts with row/column variable part of values", {
    expect_message(result_list <- dummy_df |>
           any_table(rows    = "age",
                     columns = "sex",
                     values  = "sex",
                     print   = FALSE), " x ERROR: The provided <rows>/<columns> variable 'sex' is also part of")
})


test_that("any_table outputs sum values with only invalid pct_value statistic and throws a warning", {
    expect_message(result_list <- dummy_df |>
           any_table(rows       = "age",
                     columns    = "sex",
                     values     = weight,
                     statistics = c("pct_value"),
                     pct_value  = list(rate = "Test1 / Test2"),
                     print      = FALSE), " ! WARNING: Variable 'Test1' not found in the data frame.")

    expect_equal(names(result_list[[1]]), c("row.label", "var1", "weight_sum_1",
                                            "weight_sum_2", "weight_sum_NA"))
})


test_that("any_table aborts with missing statistic extension in pre summarised data", {
    expect_message(result_list <- sum_df |>
           any_table(rows       = "year",
                     columns    = "sex",
                     values     = TYPE_NR,
                     print      = FALSE), " X ERROR: All <values> variables need to have the <statistic> extensions in their variable names")
})


test_that("any_table aborts with missing variable combination in pre summarised data", {
    expect_message(result_list <- sum_df2 |>
           any_table(rows       = c("year", "age"),
                     columns    = "sex",
                     values     = weight_sum,
                     print      = FALSE), " X ERROR: The variable combination of '")
})


test_that("Combine tables into a single workbook", {
    my_style <- excel_output_style(sheet_name = "tab1")

    tab1 <- suppressMessages(dummy_df |>
                 any_table(rows    = "age",
                           columns = "sex",
                           values  = weight,
                           print   = FALSE))

    my_style <- my_style |> modify_output_style(sheet_name = "tab2")

    tab2 <- suppressMessages(dummy_df |>
                 any_table(rows    = "age",
                           columns = "sex",
                           values  = weight,
                           by      = education,
                           print   = FALSE))

    temp_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(temp_file), add = TRUE)

    result <- combine_into_workbook(tab1, tab2, file = temp_file)

    expect_type(result, "environment")
    expect_true(file.exists(temp_file))
})

set_style_options(as_heatmap = FALSE)
