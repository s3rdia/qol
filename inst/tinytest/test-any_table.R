set_no_print(TRUE)
set_style_options(as_heatmap = TRUE)

dummy_df  <- dummy_data(3000)
dummy_big <- dummy_data(10000)

dummy_df[["binary"]] <- replicate(nrow(dummy_df), {
    paste0(sample(0:1, 2, replace = TRUE), collapse = "")
})

sum_df   <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = weight,
                   statistics = c("sum"),
                   nesting    = "deepest",
                   na.rm      = TRUE)

sum_df2  <- dummy_df |>
    summarise_plus(class      = c(year, sex, age),
                   values     = weight,
                   statistics = c("sum"),
                   nesting    = "deepest",
                   na.rm      = TRUE)

age. <- discrete_format(
    "under 50"    = 0:49,
    "50 and more" = 50:100)

state. <- discrete_format(
    "West" = 1:10,
    "East" = 11:16)

sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

education. <- discrete_format(
    "low"    = "low",
    "middle" = "middle",
    "high"   = "high")


# Simplest form of any_table
result_list <- dummy_big |>
      any_table(rows    = "age",
                columns = "sex",
                values  = weight,
                print   = FALSE)

expect_inherits(result_list, "list", info = "Simplest form of any_table")
expect_equal(length(result_list), 3, info = "Simplest form of any_table")
expect_equal(names(result_list[[1]]), c("row.label", "var1", "weight_sum_1",
                                        "weight_sum_2", "weight_sum_NA"), info = "Simplest form of any_table")
expect_equal(result_list[[1]][["var1"]][1:90], as.character(0:89), info = "Simplest form of any_table")
expect_equal(result_list[[1]][["var1"]][1:90], as.character(0:89), info = "Simplest form of any_table")


# any_table with combinations
result_list <- dummy_df |>
      any_table(rows    = "age + education",
                columns = "sex + year",
                values  = income,
                weight  = weight,
                output  = "excel_nostyle",
                print   = FALSE)

expect_inherits(result_list, "list", info = "any_table with combinations")
expect_equal(length(result_list), 3, info = "any_table with combinations")


# any_table with multiple combinations
result_list <- dummy_df |>
      any_table(rows    = c("age", "age + education"),
                columns = c("sex + year", "sex"),
                values  = weight,
                output  = "excel_nostyle",
                print   = FALSE)

expect_inherits(result_list, "list", info = "any_table with multiple combinations")
expect_equal(length(result_list), 3, info = "any_table with multiple combinations")


# any_table many combinations don't break
result_list <- dummy_df |>
        any_table(rows    = c("age", "age + education", "state",
                              "state + age", "education + age"),
                  columns = c("year", "sex + year", "sex"),
                  values  = weight,
                  output  = "excel_nostyle",
                  print   = FALSE)

expect_inherits(result_list, "list", info = "any_table many combinations don't break")
expect_equal(length(result_list), 3, info = "any_table many combinations don't break")


# any_table with titles and footnotes
set_style_options(header_stat_merging = "all")
result_list <- dummy_df |>
      any_table(rows      = "age",
                columns   = "sex",
                values    = weight,
                titles    = "Hello world link: https://cran.r-project.org/",
                footnotes = "This is a footnote link: https://cran.r-project.org/",
                print     = FALSE)

expect_inherits(result_list, "list", info = "any_table with titles and footnotes")
expect_equal(length(result_list), 3, info = "any_table with titles and footnotes")


# any_table with multiple titles and footnotes
set_style_options(header_stat_merging = "none",
                  title_font_color    = c("FF00FF", "00FF00"),
                  title_font_size     = c(10, 11),
                  title_font_bold     = c(TRUE, FALSE),
                  footnote_font_color = c("FF00FF", "00FF00"),
                  footnote_font_size  = c(10, 11),
                  footnote_font_bold  = c(TRUE, FALSE))

result_list <- dummy_df |>
    any_table(rows      = "age",
              columns   = "sex",
              values    = weight,
              titles    = c("Hello world1", "Hello world2"),
              footnotes = c("This is a footnote1", "This is a footnote2"),
              print     = FALSE)

expect_inherits(result_list, "list", info = "any_table with multiple titles and footnotes")
expect_equal(length(result_list), 3, info = "any_table with multiple titles and footnotes")


# any_table with variable and stat labels
result_list <- dummy_df |>
      any_table(rows        = "age",
                columns     = "sex",
                values      = weight,
                var_labels  = list(age = "Single ages", sex = "Sex", weight = "Population"),
                stat_labels = list(sum = "Counts"),
                box         = "Test",
                print       = FALSE)

expect_true("Single ages" %in% result_list[["table"]][["row.label"]], info = "any_table with variable and stat labels")


# any_table with removed variable and stat labels
result_list <- dummy_df |>
      any_table(rows        = "age",
                columns     = "sex",
                values      = weight,
                var_labels  = list(age = "", sex = "", weight = ""),
                stat_labels = list(sum = ""),
                print       = FALSE)

expect_true(!"row.label" %in% names(result_list[["table"]]), info = "any_table with removed variable and stat labels")


# any_table with different percentages
result_list <- dummy_df |>
      any_table(rows       = "age",
                columns    = "sex",
                values     = c(probability, weight),
                statistics = c("sum", "pct_total"),
                pct_group  = c("age", "sex"),
                pct_value  = list(rate = "probability / weight",
                                  sex  = 1),
                output  = "excel_nostyle",
                print      = FALSE)

expect_true(all(c("weight_pct_group_age_1", "weight_pct_total_1",
                  "rate_pct_value_1", "sex_pct_value_1") %in% names(result_list[["table"]])), info = "any_table with different percentages")


# any_table with keywords for row and column percentages
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = "sex",
                  values     = c(probability, weight),
                  statistics = c("pct_group"),
                  pct_group  = c("row_pct", "col_pct"),
                  output     = "excel_nostyle",
                  print      = FALSE)

expect_true(all(c("weight_pct_group_row_1", "probability_pct_group_row_2",
                  "weight_pct_group_col_1", "probability_pct_group_col_2")
                %in% names(result_list[["table"]])), info = "any_table with keywords for row and column percentages")


# any_table with block row percentages
result_list <- dummy_df |>
    any_table(rows      = "age + (sex education)",
              columns   = "state",
              values    = weight,
              pct_block = "rows",
              output    = "excel_nostyle",
              formats   = list(state = state., age = age., sex = sex., education = education.),
              print     = FALSE,
              na.rm     = TRUE)

result_df <- result_list[["table"]] |> if.(var2 == "Total")
result_df[["weight_pct_block_rows_West"]] <- round(result_df[["weight_pct_block_rows_West"]])
result_df[["weight_pct_block_rows_East"]] <- round(result_df[["weight_pct_block_rows_East"]])

expect_equal(collapse::funique(result_df[["weight_pct_block_rows_West"]]), 100, info = "any_table with block row percentages")
expect_equal(collapse::funique(result_df[["weight_pct_block_rows_East"]]), 100, info = "any_table with block row percentages")


# any_table with block column percentages
result_list <- dummy_df |>
    any_table(rows      = "state",
              columns   = "age + (sex education)",
              values    = weight,
              pct_block = "columns",
              output    = "excel_nostyle",
              formats   = list(state = state., age = age., sex = sex., education = education.),
              order_by  = "blocks",
              print     = FALSE,
              na.rm     = TRUE)

result_df <- result_list[["table"]]
result_df[["weight_pct_block_columns_under 50_Total"]] <- round(result_df[["weight_pct_block_columns_under 50_Total"]])

expect_equal(collapse::funique(result_df[["weight_pct_block_columns_under 50_Total"]]), 100, info = "any_table with block column percentages")
expect_equal(names(result_df),
             c("row.label", "var1", "weight_pct_block_columns_under 50_Total",
               "weight_pct_block_columns_under 50_Male", "weight_pct_block_columns_under 50_Female", "weight_pct_block_columns_under 50_low",
               "weight_pct_block_columns_under 50_middle", "weight_pct_block_columns_under 50_high", "weight_pct_block_columns_50 and more_Total",
               "weight_pct_block_columns_50 and more_Male", "weight_pct_block_columns_50 and more_Female", "weight_pct_block_columns_50 and more_low",
               "weight_pct_block_columns_50 and more_middle", "weight_pct_block_columns_50 and more_high"), info = "any_table with block order")


# any_table with a lot of statistics doesn't break
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = "sex",
                  values     = weight,
                  statistics = c("freq", "freq_g0", "mean", "median", "mode", "min", "max",
                                 "first", "last", "sum_wgt", "p1", "p99", "sd", "variance",
                                 "missing"),
                  output     = "excel_nostyle",
                  print      = FALSE)

expect_inherits(result_list, "list", info = "any_table with a lot of statistics doesn't break")
expect_equal(length(result_list), 3, info = "any_table with a lot of statistics doesn't break")


# any_table with interleaved order
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = "sex",
                  values     = weight,
                  statistics = c("sum", "freq", "missing"),
                  order_by   = "interleaved",
                  output     = "excel_nostyle",
                  formats    = list(age = age., sex = sex.),
                  print      = FALSE,
                  na.rm      = TRUE)

expect_equal(names(result_list[[1]]),
               c("row.label", "var1", "weight_sum_Total", "weight_freq_Total", "weight_missing_Total",
                 "weight_sum_Male", "weight_freq_Male", "weight_missing_Male",
                 "weight_sum_Female", "weight_freq_Female", "weight_missing_Female"),
             info = "any_table with interleaved order")


# any_table with values order
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = c("sex", "state"),
                  values     = c(weight, income),
                  statistics = "sum",
                  order_by   = "values",
                  output     = "excel_nostyle",
                  formats    = list(age = age., sex = sex., state = state.),
                  print      = FALSE,
                  na.rm      = TRUE)

expect_equal(names(result_list[[1]]),
             c("row.label", "var1", "weight_sum_Total", "weight_sum_Male", "weight_sum_Female",
               "weight_sum_West", "weight_sum_East",
               "income_sum_Total", "income_sum_Male", "income_sum_Female", "income_sum_West",
               "income_sum_East"), info = "any_table with values order")


# any_table with columns order
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = c("sex", "state"),
                  values     = c(weight, income),
                  statistics = "sum",
                  order_by   = "columns",
                  output     = "excel_nostyle",
                  formats    = list(age = age., sex = sex., state = state.),
                  print      = FALSE,
                  na.rm      = TRUE)

expect_equal(names(result_list[[1]]),
             c("row.label", "var1", "weight_sum_Total", "weight_sum_Male", "weight_sum_Female",
               "income_sum_Total", "income_sum_Male", "income_sum_Female",
               "weight_sum_West", "weight_sum_East", "income_sum_West",
               "income_sum_East"), info = "any_table with columns order")


# any_table with by variables
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = "sex",
                  values     = weight,
                  by         = education,
                  print_miss = TRUE,
                  print      = FALSE)

expect_true("BY" %in% names(result_list[[1]]), info = "any_table with by variables")
expect_equal(length(unique(result_list[[1]][["BY"]])), 1, info = "any_table with by variables")


# any_table with multiple by variables
result_list <- dummy_df |>
        any_table(rows       = "age",
                  columns    = "sex",
                  values     = weight,
                  by         = c(education, year),
                  output     = "excel_nostyle",
                  print_miss = TRUE,
                  print      = FALSE)

expect_true("BY" %in% names(result_list[["table"]]), info = "any_table with multiple by variables")
expect_equal(length(unique(result_list[["table"]][["BY"]])), 2, info = "any_table with multiple by variables")


# any_table with by variables and multiple row and column variables
result_list <- dummy_df |>
       any_table(rows    = c("first_person", "state", "first_person + state"),
                 columns = c("sex", "education", "sex + education"),
                 values  = weight,
                 by      = year,
                 print   = FALSE,
                 output  = "excel_nostyle",
                 na.rm   = TRUE)

expect_true("BY" %in% names(result_list[["table"]]), info = "any_table with by variables and multiple row and column variables")


# any_table with by variables as subheaders
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  by      = education,
                  style   = excel_output_style(by_as_subheaders = TRUE),
                  print   = FALSE)

expect_true("BY" %in% names(result_list[[1]]), info = "any_table with by variables as subheaders")
expect_equal(length(unique(result_list[[1]][["BY"]])), 1, info = "any_table with by variables as subheaders")


# any_table with by variables aborts if by is also part of rows or columns
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  by      = sex,
                  print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <by> variable 'sex' is also part of",
             info = "any_table with by variables aborts if by is also part of rows or columns")


# any_table with NAs removed
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  output  = "excel_nostyle",
                  na.rm   = TRUE,
                  print   = FALSE)

expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0, info = "any_table with NAs removed")


# any_table with applied single discrete labels
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  formats = list(age = discrete_format(
                      "under 18"       = 0:17,
                      "18 to under 25" = 18:24,
                      "25 to under 55" = 25:54,
                      "55 to under 65" = 55:64,
                      "65 and older"   = 65:100)),
                  output = "excel_nostyle",
                  print  = FALSE)

expect_true(all(c("under 18", "18 to under 25", "25 to under 55",
                  "55 to under 65", "65 and older")
                %in% result_list[["table"]][["var1"]]), info = "any_table with applied single discrete labels")


# any_table with applied discrete multilabels
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  formats = list(sex = discrete_format(
                      "Total"  = 1:2,
                      "Male"   = 1,
                      "Female" = 2),
                      age = discrete_format(
                      "Total"          = 0:100,
                      "under 18"       = 0:17,
                      "18 to under 25" = 18:24,
                      "25 to under 55" = 25:54,
                      "55 to under 65" = 55:64,
                      "65 and older"   = 65:100)),
                  output = "excel_nostyle",
                  print  = FALSE)

expect_true(all(c("under 18", "18 to under 25", "25 to under 55",
                  "55 to under 65", "65 and older")
                %in% result_list[["table"]][["var1"]]), info = "any_table with applied discrete multilabels")


# any_table with applied interval multilabels
result_list <- dummy_df |>
        any_table(rows    = "income",
                  columns = "sex",
                  values  = weight,
                  formats = list(sex = discrete_format(
                      "Total"  = 1:2,
                      "Male"   = 1,
                      "Female" = 2),
                      income = interval_format(
                          "Total"              = 0:99999,
                          "below 500"          = 0:499,
                          "500 to under 1000"  = 500:999,
                          "1000 to under 2000" = 1000:1999,
                          "2000 and more"      = 2000:99999)),
                  output = "excel_nostyle",
                  print  = FALSE)

expect_true(all(c("Total", "below 500", "2000 and more")
                %in% result_list[["table"]][["var1"]]), info = "any_table with applied interval multilabels")


# any_table can silence format expressions
sex. <- discrete_format(
    "!Total" = 1:2,
    "Male"   = 1,
    "Female" = 2)

result_list <- dummy_df |>
    any_table(rows       = "first_person",
              columns    = "sex",
              values     = weight,
              statistics = "sum",
              output     = "excel_nostyle",
              formats    = list(sex = sex.),
              print      = FALSE)

expect_false(any(grepl("!", names(result_list[["table"]]))), info = "any_table can silence format expressions")


# any_table able to apply format on numeric values stored as character (short route)
binary. <- discrete_format(
    "binary1" = c("00", "01"),
    "binary2" = c("10", "11"))

result_list <- dummy_df |>
    any_table(rows    = "binary",
              columns = "sex",
              values  = weight,
              formats = list(binary = binary.),
              output  = "excel_nostyle",
              print   = FALSE)

expect_equal(result_list[[1]][["var1"]], c("binary1", "binary2"), info = "any_table able to apply format on numeric values stored as character (short route)")


# any_table doesn't convert numeric values stored as character (short route)
result_list <- dummy_df |>
        any_table(rows    = "binary",
                  columns = "sex",
                  values  = weight,
                  output  = "excel_nostyle",
                  print   = FALSE)

expect_equal(result_list[[1]][["var1"]], c("00", "01", "10", "11"), info = "any_table doesn't convert numeric values stored as character (short route)")


# any_table able to apply format on numeric values stored as character (long route)
binary. <- discrete_format(
    "binary1" = c("00", "01"),
    "binary2" = c("10", "11"))

result_list <- dummy_df |>
        any_table(rows       = "binary",
                  columns    = "sex",
                  statistics = "mean",
                  values     = weight,
                  formats    = list(binary = binary.),
                  output     = "excel_nostyle",
                  print      = FALSE)

expect_equal(result_list[[1]][["var1"]], c("binary1", "binary2"), info = "any_table able to apply format on numeric values stored as character (long route)")


# any_table doesn't convert numeric values stored as character (long route)
result_list <- dummy_df |>
        any_table(rows       = "binary",
                  columns    = "sex",
                  statistics = "mean",
                  values     = weight,
                  output     = "excel_nostyle",
                  print      = FALSE)

expect_equal(result_list[[1]][["var1"]], c("00", "01", "10", "11"), info = "any_table doesn't convert numeric values stored as character (long route)")


# any_table with fixed column headers
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  style   = excel_output_style(freeze_col_header = TRUE),
                  na.rm   = TRUE,
                  output  = "excel_nostyle",
                  print   = FALSE)

expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0, info = "any_table with fixed column headers")


# any_table with fixed row headers
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  style   = excel_output_style(freeze_row_header = TRUE),
                  na.rm   = TRUE,
                  output  = "excel_nostyle",
                  print   = FALSE)

expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0, info = "any_table with fixed row headers")


# any_table with fixed column and row headers
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  style   = excel_output_style(freeze_col_header = TRUE,
                                               freeze_row_header = TRUE),
                  na.rm   = TRUE,
                  output  = "excel_nostyle",
                  print   = FALSE)

expect_true(sum(is.na(result_list[["table"]][["var1"]])) == 0, info = "any_table with fixed column and row headers")


# any_table warning with wrong output format
result_list <- dummy_df |>
                   any_table(rows    = "age",
                             columns = "sex",
                             values  = weight,
                             output  = "Test",
                             print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "<Output> format 'Test' not available.", info = "any_table warning with wrong output format")


# any_table warning with wrong output format
result_list <- dummy_df |>
                   any_table(rows     = "age",
                             columns  = "sex",
                             values   = weight,
                             order_by = "test",
                             output   = "excel_nostyle",
                             print    = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "<Order by> option 'test' doesn't exist", info = "any_table warning with wrong output format")


# any_table with pre summarised data
result_list <- sum_df |>
   any_table(rows       = "year",
             columns    = "sex",
             values     = weight_sum,
             output     = "excel_nostyle",
             print      = FALSE)

expect_inherits(result_list, "list", info = "any_table with pre summarised data")
expect_equal(length(result_list), 3, info = "any_table with pre summarised data")


# any_table with pre summarised data and by variables
result_list <- sum_df2 |>
                any_table(rows       = "age",
                          columns    = "year",
                          by         = "sex",
                          values     = weight_sum,
                          output     = "excel_nostyle",
                          print_miss = TRUE,
                          print      = FALSE)

expect_inherits(result_list, "list", info = "any_table with pre summarised data and by variables")
expect_equal(length(result_list), 3, info = "any_table with pre summarised data and by variables")


# any_table with no column variables
result_list <- dummy_df |>
       any_table(rows    = "age",
                 values  = weight,
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_inherits(result_list, "list", info = "any_table with no column variables")
expect_equal(length(result_list), 3, info = "any_table with no column variables")


# any_table throws a warning, if invalid statistic specified
result_list <- dummy_df |>
                   any_table(rows       = "age",
                             columns    = "sex",
                             values     = weight,
                             statistics = c("test", "sum"),
                             output     = "excel_nostyle",
                             print      = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "<Statistic> 'test' is invalid and will be omitted.",
               info = "any_table throws a warning, if invalid statistic specified")


# any_table throws a warning, if no valid statistic specified
result_list <- dummy_df |>
                   any_table(rows       = "age",
                             columns    = "sex",
                             values     = weight,
                             statistics = "test",
                             output     = "excel_nostyle",
                             print      = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "No valid <statistic> selected. 'sum' will be used.",
               info = "any_table throws a warning, if no valid statistic specified")


# Save any_table as Excel file
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = weight,
              output  = "excel_nostyle",
              style   = excel_output_style(save_path = dirname(temp_file),
                                           file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "Save any_table as Excel file")


# Combine tables into a single workbook
my_style <- excel_output_style(sheet_name = "tab1")

tab1 <- dummy_df |>
     any_table(rows    = "age",
               columns = "sex",
               values  = weight,
               output  = "excel_nostyle",
               print   = FALSE)

my_style <- my_style |> modify_output_style(sheet_name = "tab2")

tab2 <- dummy_df |>
     any_table(rows    = "age",
               columns = "sex",
               values  = weight,
               by      = education,
               output  = "excel_nostyle",
               print   = FALSE)

temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

result <- combine_into_workbook(tab1, tab2, file = temp_file)

expect_inherits(result, c("wbWorkbook", "R6"), info = "Combine tables into a single workbook")
expect_true(file.exists(temp_file), info = "Combine tables into a single workbook")


# any_table throws a warning with missing statistic extension in pre summarised data
result_list <- sum_df |>
       any_table(rows    = "year",
                 columns = "sex",
                 values  = DEPTH,
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "All <values> variables need to have the <statistic> extension in their variable name.",
               info = "any_table throws a warning with missing statistic extension in pre summarised data")

expect_inherits(result_list, "list", info = "any_table throws a warning with missing statistic extension in pre summarised data")
expect_equal(length(result_list), 3, info = "any_table throws a warning with missing statistic extension in pre summarised data")


# any_table auto generates missing TYPE variable in pre summarised data
result_list <- sum_df |>
       any_table(rows    = "year",
                 columns = "sex",
                 values  = weight_sum,
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_inherits(result_list, "list", info = "any_table auto generates missing TYPE variable in pre summarised data")
expect_equal(length(result_list), 3, info = "any_table auto generates missing TYPE variable in pre summarised data")


# any_table outputs unweighted results without values variable
result_list <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              output  = "excel_nostyle",
              print   = FALSE)

expect_inherits(result_list, "list", info = "any_table outputs unweighted results without values variable")
expect_equal(length(result_list), 3, info = "any_table outputs unweighted results without values variable")


# any_table throws a warning, if invalid format is used
result_list <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = weight,
              output  = "excel_nostyle",
              formats = list(age = age., sex = test),
              print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Format for variable 'sex' does not exist and can't be applied.",
               info = "any_table throws a warning, if invalid format is used")


###############################################################################
# Abort checks
###############################################################################

# any_table aborts, if column contains a row variable
result_list <- dummy_df |>
                   any_table(rows       = "sex",
                             columns    = "sex",
                             statistics = c("sum"),
                             print      = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <columns> variable '",
             info = "any_table aborts, if column contains a row variable")


# any_table aborts with duplicate column names after pivot
result_list <- dummy_df |>
    any_table(rows    = "education",
              columns = c("sex + age", "age + sex"),
              values  = weight,
              print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "Duplicate <columns> names found",
             info = "any_table aborts with duplicate column names after pivot")


# any_table aborts with none existent row variable
result_list <- dummy_df |>
      any_table(rows    = c("age", "age + test"),
                columns = "sex",
                values  = weight,
                print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <rows> variable 'test' is not part of",
             info = "any_table aborts with none existent row variable")


# any_table aborts with none existent column variable
result_list <- dummy_df |>
      any_table(rows    = "age",
                columns = c("sex + test"),
                values  = weight,
                print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <columns> variable 'test' is not part of",
             info = "any_table aborts with none existent column variable")


# any_table aborts with no valid row variables
result_list <- dummy_df |>
       any_table(rows    = "",
                 columns = "sex",
                 values  = weight,
                 print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "No valid <rows> variables provided",
             info = "any_table aborts with no valid row variables")


# any_table aborts with invalid by variable
result_list <- dummy_df |>
       any_table(rows    = "age",
                 columns = "sex",
                 by      = "test",
                 values  = weight,
                 print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable 'test' is not part of",
               info = "any_table aborts with invalid by variable")


# any_table aborts with row/column variable part of values
result_list <- dummy_df |>
       any_table(rows    = "age",
                 columns = "sex",
                 values  = "sex",
                 print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <rows>/<columns> variable 'sex' is also part of",
             info = "any_table aborts with row/column variable part of values")


# any_table outputs sum values with only invalid pct_value statistic and throws a warning
result_list <- dummy_df |>
       any_table(rows       = "age",
                 columns    = "sex",
                 values     = weight,
                 statistics = c("pct_value"),
                 pct_value  = list(rate = "Test1 / Test2"),
                 formats    = list(age = age.),
                 print      = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Variable 'Test1' not found in the data frame.",
               info = "any_table outputs sum values with only invalid pct_value statistic and throws a warning")

expect_equal(names(result_list[[1]]), c("row.label", "var1", "weight_sum_1",
                                        "weight_sum_2", "weight_sum_NA"), info = "any_table outputs sum values with only invalid pct_value statistic and throws a warning")


# any_table aborts with missing variable combination in pre summarised data
result_list <- sum_df2 |>
       any_table(rows    = c("year", "age"),
                 columns = "sex",
                 values  = weight_sum,
                 print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The variable combination of '",
             info = "any_table aborts with missing variable combination in pre summarised data")


# Combine tables into a single workbook aborts, if no any_table or export_with_style object was found
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

result <- combine_into_workbook(1, file = temp_file)
expect_error(print_stack_as_messages("ERROR"), "Unknown object found. Provide <any_table> or <export_with_style> results.",
             info = "Combine tables into a single workbook aborts, if no any_table object was found")

result <- combine_into_workbook(list(1), file = temp_file)
expect_error(print_stack_as_messages("ERROR"), "Unknown object found. Provide <any_table> or <export_with_style> results.",
             info = "Combine tables into a single workbook aborts, if no any_table or export_with_style object was found")

expect_true(!file.exists(temp_file), info = "Combine tables into a single workbook aborts, if no any_table or export_with_style object was found")


# any_table aborts with no valid values after calculating the results
dummy_df |>
    any_table(rows       = "year",
              columns    = "sex",
              values     = weight,
              statistics = "pct_value",
              pct_value  = list(sex = "test",
                                age = "test"),
              print      = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Variable 'age' not found in the data frame",
            info = "any_table aborts with no valid values after calculating the results")
expect_warning(print_stack_as_messages("WARNING"), "Subsetting variable 'sex' by 'test' results in an empty data frame",
            info = "any_table aborts with no valid values after calculating the results")
expect_error(print_stack_as_messages("ERROR"), "After calculating the results, there are no valid values",
            info = "any_table aborts with no valid values after calculating the results")


set_style_options(as_heatmap = FALSE)
set_no_print()
