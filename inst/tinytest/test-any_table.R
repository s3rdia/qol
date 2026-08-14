set_no_print(TRUE)

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

age2. <- discrete_format(
    "Total"       = 0:100,
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

expect_inherits(result_list, "qol_table", info = "Simplest form of any_table")
expect_equal(length(result_list), 4, info = "Simplest form of any_table")
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
                full_precision = TRUE,
                output  = "excel_nostyle",
                print   = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table with combinations")
expect_equal(length(result_list), 4, info = "any_table with combinations")


# any_table with multiple combinations
result_list <- dummy_df |>
      any_table(rows    = c("age", "age + education"),
                columns = c("sex + year", "sex"),
                values  = weight,
                output  = "excel_nostyle",
                print   = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table with multiple combinations")
expect_equal(length(result_list), 4, info = "any_table with multiple combinations")


# any_table many combinations don't break
result_list <- dummy_df |>
        any_table(rows    = c("age", "age + education", "state",
                              "state + age", "education + age"),
                  columns = c("year", "sex + year", "sex"),
                  values  = weight,
                  output  = "excel_nostyle",
                  print   = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table many combinations don't break")
expect_equal(length(result_list), 4, info = "any_table many combinations don't break")


# any_table with linked titles and footnotes
txt_file  <- system.file("extdata", "qol_example_data_txt.txt",   package = "qol")

set_titles("Hello world1",
           "Hello world2 link: https://cran.r-project.org/",
           "Hello world3 cell: A8",
           "Hello world4 file: txt_file")
set_footnotes("This is a footnote1",
              "This is a footnote2 link: https://cran.r-project.org/",
              "This is a footnote3 cell: A8",
              "This is a footnote4 file: txt_file")

set_style_options(as_heatmap          = TRUE,
                  header_stat_merging = "all")

result_list <- dummy_df |>
      any_table(rows      = "age",
                columns   = "sex",
                values    = weight,
                titles    = "Hello world link: https://cran.r-project.org/",
                footnotes = "This is a footnote link: https://cran.r-project.org/",
                print     = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table with linked titles and footnotes")
expect_equal(length(result_list), 4, info = "any_table with linked titles and footnotes")

# any_table with multiple titles and footnotes
set_titles("Hello world1", "Hello world2")
set_footnotes("This is a footnote1", "This is a footnote2")

set_style_options(header_stat_merging = "none",
                  title_font_color    = c("FF00FF", "00FF00"),
                  title_font_size     = c(10, 11),
                  title_font_bold     = c(TRUE, FALSE),
                  footnote_font_color = c("FF00FF", "00FF00"),
                  footnote_font_size  = c(10, 11),
                  footnote_font_bold  = c(TRUE, FALSE),
                  as_heatmap          = FALSE)

result_list <- dummy_df |>
    any_table(rows      = "age",
              columns   = "sex",
              values    = weight,
              print     = FALSE)

reset_style_options()

expect_inherits(result_list, "qol_table", info = "any_table with multiple titles and footnotes")
expect_equal(length(result_list), 4, info = "any_table with multiple titles and footnotes")


# any_table with multiple titles and footnotes makes style options scale accordingly
set_titles("Hello world1", "Hello world2", "Hello world3")
set_footnotes("This is a footnote1", "This is a footnote2", "This is a footnote3")

result_list <- dummy_df |>
    any_table(rows      = "age",
              columns   = "sex",
              values    = weight,
              print     = FALSE)

style <- result_list[["meta"]][["style"]]

reset_style_options()

expect_equal(length(style[["title_font_color"]]),    3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["title_font_size"]]),     3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["title_font_bold"]]),     3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["title_alignment"]]),     3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["footnote_font_color"]]), 3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["footnote_font_size"]]),  3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["footnote_font_bold"]]),  3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")
expect_equal(length(style[["footnote_alignment"]]),  3, info = "any_table with multiple titles and footnotes makes style options scale accordingly")


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
                output     = "excel_nostyle",
                print      = FALSE)

expect_true(all(c("weight_pct_group_age_1", "weight_pct_total_1",
                  "rate_pct_value_1", "sex_pct_value_1") %in% names(result_list[["table"]])),
            info = "any_table with different percentages")


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
    any_table(rows      = "age + (sex, education)",
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
              columns   = "age + (sex, education)",
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


# any_table can expand combinations
result_list <- dummy_df |>
    any_table(rows       = "age + (state, first_person + education)",
              columns    = "sex",
              values     = weight,
              output     = "excel_nostyle",
              formats    = list(state = state., age = age., sex = sex., education = education.),
              print      = FALSE)

expect_true(all(c("West", "East", "0", "1") %in% result_list[["table"]][["var2"]]), info = "any_table can expand combinations")
expect_true(all(c("low", "middle", "high", "") %in% result_list[["table"]][["var3"]]), info = "any_table can expand combinations")


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

expect_inherits(result_list, "qol_table", info = "any_table with a lot of statistics doesn't break")
expect_equal(length(result_list), 4, info = "any_table with a lot of statistics doesn't break")


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


# any_table with by variables throws a warning if by is also part of rows or columns
result_list <- dummy_df |>
        any_table(rows    = "age",
                  columns = "sex",
                  values  = weight,
                  by      = sex,
                  print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable 'sex' is also part of",
             info = "any_table with by variables throws a warning if by is also part of rows or columns")


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
                          "Total"              =    0:100000,
                          "below 500"          =    0:500,
                          "500 to under 1000"  =  500:1000,
                          "1000 to under 2000" = 1000:2000,
                          "2000 and more"      = 2000:100000)),
                  output = "excel_nostyle",
                  print  = FALSE)

expect_true(all(c("Total", "below 500", "2000 and more")
                %in% result_list[["table"]][["var1"]]), info = "any_table with applied interval multilabels")


# any_table can silence format expressions
sex2. <- discrete_format(
    "!Total" = 1:2,
    "Male"   = 1,
    "Female" = 2)

result_list <- dummy_df |>
    any_table(rows       = "first_person",
              columns    = "sex",
              values     = weight,
              statistics = "sum",
              output     = "excel_nostyle",
              formats    = list(sex = sex2.),
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
                                               freeze_row_header = TRUE,
                                               background_color  = "FF00FF"),
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

expect_inherits(result_list, "qol_table", info = "any_table with pre summarised data")
expect_equal(length(result_list), 4, info = "any_table with pre summarised data")


# any_table with pre summarised data and by variables
result_list <- sum_df2 |>
                any_table(rows       = "age",
                          columns    = "year",
                          by         = "sex",
                          values     = weight_sum,
                          output     = "excel_nostyle",
                          print_miss = TRUE,
                          print      = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table with pre summarised data and by variables")
expect_equal(length(result_list), 4, info = "any_table with pre summarised data and by variables")


# any_table with pre summarised data can use formats
result_list <- sum_df2 |>
    any_table(rows       = "year + age",
              columns    = "sex",
              values     = weight_sum,
              formats    = list(sex = sex., age = age.),
              output     = "excel_nostyle",
              print      = FALSE)

expect_equal(collapse::funique(result_list[["table"]][["var2"]]), c("under 50", "50 and more"), info = "any_table with pre summarised data can use formats")
expect_true(all(c("weight_sum_Total", "weight_sum_Male", "weight_sum_Female") %in% names(result_list[["table"]])),
            info = "any_table with pre summarised data can use formats")


# any_table with no column variables
result_list <- dummy_df |>
       any_table(rows    = "age",
                 values  = weight,
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table with no column variables")
expect_equal(length(result_list), 4, info = "any_table with no column variables")


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
               style   = my_style,
               print   = FALSE)

my_style <- my_style |> modify_output_style(sheet_name = "tab2")

tab2 <- dummy_df |>
     any_table(rows    = "age",
               columns = "sex",
               values  = weight,
               by      = education,
               output  = "excel_nostyle",
               style   = my_style,
               print   = FALSE)

temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

result <- combine_into_workbook(tab1, tab2, style = excel_output_style(save_path = dirname(temp_file),
                                                                       file      = basename(temp_file)))

expect_inherits(result, c("wbWorkbook", "R6"), info = "Combine tables into a single workbook")
expect_true(file.exists(temp_file), info = "Combine tables into a single workbook")

result_list <- dummy_df |>
    any_table(rows     = "age",
              columns  = "sex",
              values   = weight,
              output   = "excel_nostyle",
              workbook = tab1,
              style    = my_style,
              print    = FALSE)

expect_inherits(result_list, "qol_table", info = "Combine tables into a single workbook")
expect_equal(length(result_list), 4, info = "Combine tables into a single workbook")


# Combine tables into a single workbook with table of contents
my_style <- excel_output_style(sheet_name = "tab1")

tab1 <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = weight,
              output  = "excel_nostyle",
              style   = my_style,
              print   = FALSE)

my_style <- my_style |> modify_output_style(sheet_name = "tab2")

tab2 <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = weight,
              by      = education,
              output  = "excel_nostyle",
              style   = my_style,
              print   = FALSE)

result <- combine_into_workbook(tab1, tab2,
                                table_of_contents = TRUE,
                                subheaders        = list("First Subheader"  = "tab1",
                                                         "Second Subheader" = "tab21"),
                                subheader_colors  = c("FF0000", "00FF00", "0000FF"),
                                colored_tabs      = TRUE,
                                print             = FALSE)

expect_inherits(result, c("wbWorkbook", "R6"), info = "Combine tables into a single workbook with table of contents")
expect_true("Contents" %in% openxlsx2::wb_get_sheet_names(result),
            info = "Combine tables into a single workbook with table of contents")

result <- combine_into_workbook(tab1, tab2,
                                table_of_contents = TRUE,
                                subheaders        = list("First Subheader"  = "tab1",
                                                         "Second Subheader" = "tab2"),
                                subheader_colors  = c("FF0000", "00FF00", "0000FF"),
                                colored_tabs      = TRUE,
                                print             = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The following sheet name provided in the subheaders is invalid:",
             info = "Combine tables into a single workbook with table of contents")
expect_true(!"Contents" %in% openxlsx2::wb_get_sheet_names(result),
            info = "Combine tables into a single workbook with table of contents")


# Create and save table of contents standalone
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

my_style <- excel_output_style(sheet_name = "tab1")

tab1 <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = weight,
              output  = "excel_nostyle",
              style   = my_style,
              print   = FALSE)

my_style <- my_style |> modify_output_style(sheet_name = "tab2")

tab2 <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = weight,
              by      = education,
              output  = "excel_nostyle",
              style   = my_style,
              print   = FALSE)

result <- combine_into_workbook(tab1, tab2, print = FALSE)

create_table_of_contents(result,
                         subheaders        = list("First Subheader"  = "tab1",
                                                  "Second Subheader" = "tab21"),
                         subheader_colors  = c("FF0000", "00FF00", "0000FF"),
                         colored_tabs      = TRUE,
                         style             = excel_output_style(save_path = dirname(temp_file),
                                                                file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "Create and save table of contents standalone")


# any_table throws a warning with missing statistic extension in pre summarised data
result_list <- sum_df |>
       any_table(rows    = "year",
                 columns = "sex",
                 values  = DEPTH,
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "All <values> variables need to have the <statistic> extension in their variable name.",
               info = "any_table throws a warning with missing statistic extension in pre summarised data")

expect_inherits(result_list, "qol_table", info = "any_table throws a warning with missing statistic extension in pre summarised data")
expect_equal(length(result_list), 4, info = "any_table throws a warning with missing statistic extension in pre summarised data")


# any_table auto generates missing TYPE variable in pre summarised data
result_list <- sum_df |>
       any_table(rows    = "year",
                 columns = "sex",
                 values  = weight_sum,
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table auto generates missing TYPE variable in pre summarised data")
expect_equal(length(result_list), 4, info = "any_table auto generates missing TYPE variable in pre summarised data")


# any_table outputs unweighted results without values variable
result_list <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              output  = "excel_nostyle",
              print   = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table outputs unweighted results without values variable")
expect_equal(length(result_list), 4, info = "any_table outputs unweighted results without values variable")


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


# any_table can compute values on the fly
result_list <- dummy_df |>
    any_table(rows       = "first_person",
              columns    = "sex",
              values     = "probability",
              statistics = c("sum", "sum_wgt"),
              compute    = list(percent    = probability_sum * 100 / sum_wgt,
                                pct_square = percent ^ 2),
              weight     = weight,
              output     = "excel_nostyle",
              na.rm      = TRUE,
              print      = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table can compute values on the fly")
expect_equal(length(result_list), 4, info = "any_table can compute values on the fly")
expect_equal(names(result_list[[1]]), c("row.label", "var1", "probability_sum_1",
                                        "probability_sum_2", "sum_wgt_1", "sum_wgt_2",
                                        "percent_sum_1", "percent_sum_2", "pct!!!square_sum_1", "pct!!!square_sum_2"),
             info = "any_table can compute values on the fly")


# any_table orders individual variables by name
result_list <- dummy_df |>
    any_table(rows       = "first_person",
              columns    = "sex",
              values     = "probability",
              statistics = c("sum", "sum_wgt"),
              compute    = list(percent    = probability_sum * 100 / sum_wgt,
                                pct_square = percent ^ 2),
              weight     = weight,
              order_by   = c("pct_square", "percent", "sum_wgt"),
              output     = "excel_nostyle",
              na.rm      = TRUE,
              print      = FALSE)

expect_inherits(result_list, "qol_table", info = "any_table orders individual variables by name")
expect_equal(length(result_list), 4, info = "any_table orders individual variables by name")
expect_equal(names(result_list[[1]]), c("row.label", "var1", "pct!!!square_sum_1", "pct!!!square_sum_2",
                                        "percent_sum_1", "percent_sum_2", "sum_wgt_1", "sum_wgt_2",
                                        "probability_sum_1",  "probability_sum_2"),
             info = "any_table orders individual variables by name")
expect_message(print_stack_as_messages("NOTE"), "The following variables generated by compute are missing",
               info = "any_table orders individual variables by name")


# any_table is able to output specific statistics per variable
result_list <- dummy_df |>
    any_table(rows       = "age",
              columns    = "sex",
              statistics = list("sum"       = income,
                                "pct_group" = weight),
              na.rm      = TRUE,
              print      = FALSE)

expect_equal(collapse::fncol(result_list[[1]]), 6, info = "any_table is able to output specific statistics per variable")
expect_true(all(c("income_sum_1", "income_sum_2", "weight_pct_group_sex_1", "weight_pct_group_sex_2") %in% names(result_list[[1]])),
            info = "any_table is able to output specific statistics per variable")
expect_true(!all(c("income_pct_group_1", "income_pct_group_2", "weight_sum_1", "weight_sum_2") %in% names(result_list[[1]])),
            info = "any_table is able to output specific statistics per variable")

result_list <- dummy_df |>
    any_table(rows       = "age",
              columns    = "sex",
              statistics = list("sum"       = c(income, expenses),
                                "pct_group" = weight),
              na.rm      = TRUE,
              print      = FALSE)

expect_equal(collapse::fncol(result_list[[1]]), 8, info = "any_table is able to output specific statistics per variable")
expect_true(all(c("income_sum_1", "income_sum_2", "expenses_sum_1", "expenses_sum_2",
                  "weight_pct_group_sex_1", "weight_pct_group_sex_2") %in% names(result_list[[1]])),
            info = "any_table is able to output specific statistics per variable")
expect_true(!all(c("income_pct_group_1", "income_pct_group_2", "expenses_pct_group_1",
                   "expenses_pct_group_2", "weight_sum_1", "weight_sum_2") %in% names(result_list[[1]])),
            info = "any_table is able to output specific statistics per variable")


# any_table can handle duplicate column names without NA values
result_list <- dummy_df |>
    any_table(rows    = "education",
              columns = c("sex", "age"),
              values  = weight,
              na.rm   = TRUE,
              print   = FALSE)

expect_true(all(c("weight_sum_1.dup1", "weight_sum_2.dup1",
                  "weight_sum_1.dup2", "weight_sum_1.dup2") %in% names(result_list[[1]])),
            info = "any_table can handle duplicate column names without NA values")


result_list <- dummy_df |>
    any_table(rows    = "education",
              columns = c("sex", "age"),
              values  = weight,
              formats = list(sex = sex., age = age2.),
              na.rm   = TRUE,
              print   = FALSE)

expect_true(all(c("weight_sum_Total.dup1", "weight_sum_Total.dup2") %in% names(result_list[[1]])),
            info = "any_table can handle duplicate column names without NA values")


# any_table throws a warning with invalid by variable
result_list <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              by      = "test",
              values  = weight,
              print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable 'test' is not part of",
               info = "any_table throws a warning with invalid by variable")


# any_table throws a warning with values stored as character
result_list <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = education,
              print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The following <values> are stored as character variables in the data frame",
               info = "any_table throws a warning with values stored as character")

result_list <- dummy_df |>
    any_table(rows    = "age",
              columns = "sex",
              values  = c(education, income_class),
              print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The following <values> are stored as character variables in the data frame",
               info = "any_table throws a warning with values stored as character")

###############################################################################
# Html checks
###############################################################################

# any_table can render output in html format
result_html <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = c("sum", "pct_group"),
              pct_group  = "sex",
              formats    = list(sex = sex., age = age.),
              by         = "education",
              titles     = "My Title",
              footnotes  = "My Footnote",
              na.rm      = TRUE,
              print      = FALSE,
              output     = "html",
              style      = excel_output_style(by_as_subheaders = TRUE,
                                              as_heatmap       = TRUE))

html_out <- result_html[["html"]]

expect_inherits(result_html, "qol_table",                         info = "any_table can render output in html format")
expect_true(grepl("<!DOCTYPE html>",     html_out, fixed = TRUE), info = "any_table html render returns a html document")
expect_true(grepl("<thead>",             html_out, fixed = TRUE), info = "any_table html render contains a table header")
expect_true(grepl("<tbody>",             html_out, fixed = TRUE), info = "any_table html render contains a table body")
expect_true(grepl("class=\"box\"",       html_out, fixed = TRUE), info = "any_table html render renders the top left box")
expect_true(grepl("class=\"subheader\"", html_out, fixed = TRUE), info = "any_table html render renders by variable blocks as subheaders")
expect_true(grepl("My Title",            html_out, fixed = TRUE), info = "any_table html render renders the titles")
expect_true(grepl("My Footnote",         html_out, fixed = TRUE), info = "any_table html render renders the footnotes")
expect_true(grepl("</div>\n<div class=\"qol-spacer\"></div>\n               <div class=\"qol-wrap\">", html_out),
            info = "any_table html render puts the blank row between the titles and the table")
expect_true(grepl("</div>\n               <div class=\"qol-spacer\"></div>\n<div class=\"qol-footnotes qol-footnotes-first\"", html_out),
            info = "any_table html render puts the blank row between the table and the footnotes")
expect_true(grepl(".qol-spacer {", html_out, fixed = TRUE), info = "any_table html render styles the blank row")
expect_true(grepl('class="qol-footnotes qol-footnotes-first"', html_out, fixed = TRUE),
            info = "any_table html render marks the top footnote for the separating line")
expect_true(grepl('.qol-footnotes-first::before', html_out, fixed = TRUE),
            info = "any_table html render draws the footnote line via a pseudo element")
expect_true(grepl('width: var(--qol-fn-line, 0px)', html_out, fixed = TRUE),
            info = "any_table html render sizes the footnote line via a css variable")
expect_true(grepl('--qol-fn-line", catWidth + "px"', html_out, fixed = TRUE),
            info = "any_table html render measures the row header width for the footnote line")
expect_true(grepl('class="qol-sheet" style="margin-left: 8ch"', html_out, fixed = TRUE),
            info = "any_table html render offsets the sheet by one empty column with start_column = 2")
expect_true(grepl("<body>\n               <div class=\"qol-spacer\"></div>\n<div class=\"qol-sheet\"", html_out),
            info = "any_table html render inserts one empty row before the titles with start_row = 2")
expect_false(grepl('style="height:', html_out, fixed = TRUE), info = "any_table html render writes no inline row heights with row_heights = auto")


# any_table html render inserts no empty columns or rows on start at 1,1
result_offset <- dummy_df |>
    any_table(rows       = "sex",
              columns    = "year",
              output     = "html",
              print      = FALSE,
              style      = excel_output_style(start_row = 1, start_column = 1))

html_offset <- result_offset[["html"]]

expect_equal(lengths(regmatches(html_offset, gregexpr("class=\"qol-spacer\"", html_offset))), 0L,
             info = "any_table html render inserts no empty columns or rows on start at 1,1")
expect_false(grepl("margin-left:", html_offset),
             info = "any_table html render inserts no empty columns or rows on start at 1,1")


# any_table html render inserts custom statistic labels
result_stats <- dummy_df |>
    any_table(rows        = "sex",
              columns     = "year",
              values      = weight,
              statistics  = c("sum", "mean"),
              formats     = list(sex = sex.),
              stat_labels = list(sum = "1000", mean = "Average"),
              output      = "html",
              print       = FALSE)

html_stats <- result_stats[["html"]]

expect_true(grepl(">1000<",    html_stats, fixed = TRUE), info = "any_table html render inserts custom labels")
expect_true(grepl(">Average<", html_stats, fixed = TRUE), info = "any_table html render inserts custom labels")
expect_false(grepl(">sum<",    html_stats),               info = "any_table html render inserts custom labels")
expect_false(grepl(">mean<",   html_stats),               info = "any_table html render inserts custom labels")


# any_table html render sets individual column widths
result_widths <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex., age = age.),
              output     = "html",
              style      = excel_output_style(column_widths = c(2, 20, 20, 20, 10)),
              print      = FALSE)

html_widths <- result_widths[["html"]]

colgroup <- regmatches(html_widths, regexpr("<colgroup>.*?</colgroup>", html_widths))

expect_true(grepl("width: 20ch",          colgroup,    fixed = TRUE), info = "any_table html render sets individual column widths")
expect_true(grepl('style="width: 100ch"', html_widths, fixed = TRUE), info = "any_table html render sets individual column widths")
expect_true(grepl('class="qol-sheet" style="margin-left: 2ch"', html_widths, fixed = TRUE), info = "any_table html render sets individual column widths")
expect_true(grepl("table-layout: fixed",  html_widths, fixed = TRUE), info = "any_table html render sets individual column widths")


# any_table html render sets individual row heights
result_heights <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex., age = age.),
              titles     = "My Title",
              footnotes  = "My Footnote",
              output     = "html",
              style      = excel_output_style(row_heights = c(10, 20, 30, 40, 50, 60, 70)),
              print      = FALSE)

html_heights <- result_heights[["html"]]

expect_true(grepl('<div class="qol-spacer" style="height: 10pt"></div>', html_heights, fixed = TRUE),
            info = "aany_table html render sets individual row heights")
expect_true(grepl('<div class="qol-titles" style="height: 20pt[^"]*">My Title</div>', html_heights),
            info = "any_table html render applies the second row height to the title")
expect_true(grepl('<div class="qol-spacer" style="height: 30pt"></div>', html_heights, fixed = TRUE),
            info = "any_table html render applies the third row height to the blank row below the title")
expect_true(grepl('<tr style="height: 40pt">', html_heights, fixed = TRUE),
            info = "any_table html render applies a row height to the first column header row")
expect_true(grepl('<tr style="height: 50pt">', html_heights, fixed = TRUE),
            info = "any_table html render applies a row height to the second column header row")
expect_true(grepl('<tr style="height: 60pt">', html_heights, fixed = TRUE),
            info = "any_table html render applies a row height to the table body rows")
expect_true(grepl('<div class="qol-spacer" style="height: 70pt"></div>', html_heights, fixed = TRUE),
            info = "any_table html render applies a row height to the blank row above the footnotes")
expect_true(grepl('<div class="qol-footnotes qol-footnotes-first" style="height: 70pt[^"]*">My Footnote</div>', html_heights),
            info = "any_table html render applies the last row height to the footnote")


# any_table html render part-specific heights apply only to the parts that exist
result_parts <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex., age = age.),
              titles     = c("Title One", "Title Two"),
              footnotes  = c("Note One", "Note Two"),
              output     = "html",
              style      = excel_output_style(title_heights    = c(20, 30),
                                              footnote_heights = c(12, 14)),
              print      = FALSE)

html_parts <- result_parts[["html"]]

expect_true(grepl('<div class="qol-titles" style="height: 20pt[^"]*">Title One</div>', html_parts),
            info = "any_table html render applies the first title height to the first title")
expect_true(grepl('<div class="qol-titles" style="height: 30pt[^"]*">Title Two</div>', html_parts),
            info = "any_table html render applies the second title height to the second title")
expect_true(grepl('<div class="qol-footnotes qol-footnotes-first" style="height: 12pt[^"]*">Note One</div>', html_parts),
            info = "any_table html render applies the first footnote height to the first footnote")
expect_true(grepl('<div class="qol-footnotes" style="height: 14pt[^"]*">Note Two</div>', html_parts),
            info = "any_table html render applies the second footnote height to the second footnote")
expect_false(grepl('style="height:', gsub("<div class=\"qol-titles\"[^>]*>|<div class=\"qol-footnotes[^\"]*\"[^>]*>", "", html_parts), fixed = TRUE),
             info = "any_table html render part-specific heights leave header and body rows untouched")


# any_table html render part-specific heights override the global row_heights for their own part only
result_override <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex., age = age.),
              titles     = "My Title",
              output     = "html",
              style      = excel_output_style(row_heights    = c(10, 20, 30, 40, 50, 60),
                                              title_heights  = 99,
                                              header_heights = 25,
                                              table_heights  = 18),
              print      = FALSE)

html_override <- result_override[["html"]]

expect_true(grepl('<div class="qol-titles" style="height: 99pt[^"]*">My Title</div>', html_override),
            info = "any_table html render title_heights override row_heights for the title")
expect_true(grepl('<tr style="height: 25pt">', html_override, fixed = TRUE),
            info = "any_table html render header_heights override row_heights for the header rows")
expect_true(grepl('<tr style="height: 18pt">', html_override, fixed = TRUE),
            info = "any_table html render table_heights override row_heights for the body rows")
expect_false(grepl('<tr style="height: 40pt">|<tr style="height: 50pt">|<tr style="height: 60pt">', html_override, fixed = TRUE),
             info = "any_table html render part-specific heights suppress the matching row_heights slices")


# any_table html render subheader_heights apply to the by block banners
result_subheader <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex., age = age.),
              by         = "education",
              output     = "html",
              style      = excel_output_style(by_as_subheaders  = TRUE,
                                              subheader_heights = 22,
                                              table_heights    = 15),
              print      = FALSE)

html_subheader <- result_subheader[["html"]]

expect_true(grepl('<tr style="height: 22pt">', html_subheader, fixed = TRUE),
            info = "any_table html render subheader_heights apply to the by block banners")
expect_true(grepl('<tr style="height: 15pt">', html_subheader, fixed = TRUE),
            info = "any_table html render table_heights apply to the data rows of by blocks")


# any_table html render renders every by expression as its own table
result_by_blocks <- dummy_df |>
    any_table(rows       = c("sex", "age"),
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex., age = age.),
              by         = "education",
              titles     = "My Title",
              footnotes  = "My Footnote [by_var]",
              output     = "html",
              print      = FALSE)

html_by_blocks <- result_by_blocks[["html"]]

n_sheets <- lengths(regmatches(html_by_blocks, gregexpr("class=\"qol-sheet\"",        html_by_blocks)))
n_tables <- lengths(regmatches(html_by_blocks, gregexpr("<table class=\"qol-table\"", html_by_blocks)))

expect_true(n_sheets == n_tables && n_tables > 1, info = "any_table html render renders every by expression as its own table")
expect_false(grepl("class=\"subheader\"", html_by_blocks, fixed = TRUE), info = "any_table html render renders no subheaders when by expressions are individual tables")
expect_true(all(vapply(c("education = low", "education = middle", "education = high", "education = ."),
                       function(by_info) grepl(by_info, html_by_blocks, fixed = TRUE),
                       logical(1))), info = "any_table html render adds the by expression to the titles of every table")
expect_true(grepl("My Footnote low",    html_by_blocks, fixed = TRUE) &&
            grepl("My Footnote middle", html_by_blocks, fixed = TRUE) &&
            grepl("My Footnote high",   html_by_blocks, fixed = TRUE), info = "any_table html render replaces the [by_var] placeholder in the footnotes")
expect_true(grepl("</div>\n<div class=\"qol-spacer\"></div>\n<div class=\"qol-sheet\"", html_by_blocks),
            info = "any_table html render separates the individual tables by a blank row")


# any_table html render adds no width measurement script without titles or footnotes
result_plain <- dummy_df |>
    any_table(rows       = "sex",
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex.),
              output     = "html",
              style      = excel_output_style(),
              print      = FALSE)

html_plain <- result_plain[["html"]]

expect_false(grepl("<script>", html_plain, fixed = TRUE), info = "any_table html render adds no width measurement script without titles or footnotes")


# any_table html render displays the decimals from the number format style
result_decimals <- dummy_df |>
    any_table(rows       = "sex",
              columns    = "year",
              values     = income,
              statistics = "sum",
              formats    = list(sex = sex.),
              output     = "html",
              style      = excel_output_style(
                  number_formats = number_format_style(
                      sum_excel    = "#,###,##0", # 0 decimals in the excel format
                      sum_decimals = 2)),         # but 2 decimals to display
              print      = FALSE)

html_decimals  <- result_decimals[["html"]]
cells_decimals <- regmatches(html_decimals, gregexpr('<td class="data"[^>]*>[^<]+</td>', html_decimals))[[1]]

expect_true(any(grepl(",[0-9]{2}</td>$",  cells_decimals)), info = "any_table html render displays the decimals from the number format style")
expect_false(any(grepl(",[0-9]{3}</td>$", cells_decimals)), info = "any_table html render displays the decimals from the number format style")
expect_true(any(grepl("^<td[^>]*>[0-9]{2,3}([.][0-9]{3})+,[0-9]{2}</td>$", cells_decimals)), info = "any_table html render uses a dot as thousand separator")
expect_false(any(grepl("^<td[^>]*>[0-9]{1,3},[0-9]{3}", cells_decimals)), info = "any_table html render does not use the separators of the excel formats")


# The outer borders stay open even when all inner borders are switched on
result_full_borders <- dummy_df |>
    any_table(rows       = "sex",
              columns    = "year",
              values     = weight,
              statistics = "sum",
              formats    = list(sex = sex.),
              output     = "html",
              style      = excel_output_style(header_borders    = TRUE,
                                              box_borders       = TRUE,
                                              cat_col_borders   = TRUE,
                                              table_borders     = TRUE,
                                              subheader_borders = TRUE),
              print      = FALSE)

html_full_borders <- result_full_borders[["html"]]

expect_true(grepl("table.qol-table th.box { border-left: none; }", html_full_borders, fixed = TRUE),
            info = "any_table html render keeps the outer left edge open when all borders are on")
expect_true(grepl("table.qol-table td.data { border-right: none; border-bottom: none; }", html_full_borders, fixed = TRUE),
            info = "any_table html render keeps the outer right and bottom edges open when all borders are on")


# Hyperlink keywords are translated into html links. "link:" points to a webpage,
# "file:" to a file on disk and "cell:" is a pseudo hyperlink that leads nowhere.
# All links are styled like the excel links with a blue font color and underline.
result_html_links <- dummy_df |>
    any_table(rows      = "age",
              columns   = "sex",
              values    = weight,
              titles    = c("Hello world1 link: https://cran.r-project.org/",
                            "Hello world2 cell: A8",
                            "Hello world3 file: test.txt"),
              footnotes = c("This is a footnote1 link: https://cran.r-project.org/",
                            "This is a footnote2 cell: A8",
                            "This is a footnote3 file: test.txt"),
              output    = "html",
              print     = FALSE)

html_links    <- result_html_links[["html"]]
link_file_url <- paste0("file:///", gsub("\\\\", "/", "test.txt"))

expect_true(grepl('<a href="https://cran.r-project.org/" target="_blank">Hello world1</a>', html_links, fixed = TRUE),
            info = "any_table html render renders a link: keyword in a title as a hyperlink")
expect_true(grepl('<a href="#" onclick="return false;">Hello world2</a>', html_links, fixed = TRUE),
            info = "any_table html render renders a cell: keyword in a title as a pseudo hyperlink")
expect_true(grepl(paste0('<a href="', link_file_url, '" target="_blank">Hello world3</a>'), html_links, fixed = TRUE),
            info = "any_table html render renders a file: keyword in a title as a hyperlink to the file")
expect_true(grepl('<a href="https://cran.r-project.org/" target="_blank">This is a footnote1</a>', html_links, fixed = TRUE),
            info = "any_table html render renders a link: keyword in a footnote as a hyperlink")
expect_true(grepl('<a href="#" onclick="return false;">This is a footnote2</a>', html_links, fixed = TRUE),
            info = "any_table html render renders a cell: keyword in a footnote as a pseudo hyperlink")
expect_true(grepl(paste0('<a href="', link_file_url, '" target="_blank">This is a footnote3</a>'), html_links, fixed = TRUE),
            info = "any_table html render renders a file: keyword in a footnote as a hyperlink to the file")
expect_true(grepl('Hello world1', html_links, fixed = TRUE),
            info = "any_table html render leaves titles without a hyperlink keyword unmodified")
expect_true(grepl(".qol-titles a { color: #0000FF; text-decoration: underline; }", html_links, fixed = TRUE),
            info = "any_table html render styles title links like the excel links")
expect_true(grepl(".qol-footnotes a { color: #0000FF; text-decoration: underline; }", html_links, fixed = TRUE),
            info = "any_table html render styles footnote links like the excel links")


# any_table html render styles titles and footnotes
result_styled <- dummy_df |>
    any_table(rows      = "sex",
              columns   = "year",
              values    = weight,
              statistics = "sum",
              formats   = list(sex = sex.),
              titles    = c("Title A", "Title B"),
              footnotes = c("Note A", "Note B"),
              output    = "html",
              style     = excel_output_style(),
              print     = FALSE)

html_styled <- result_styled[["html"]]

expect_true(grepl('<div class="qol-titles" style="font-size: 10pt; color: #000000; font-weight: bold; text-align: left">Title A</div>',
                  html_styled, fixed = TRUE), info = "any_table html render bolds the titles by default")
expect_true(grepl('<div class="qol-titles" style="font-size: 10pt; color: #000000; font-weight: bold; text-align: left">Title B</div>',
                  html_styled, fixed = TRUE), info = "any_table html render bolds every title with the default title_font_bold")
expect_true(grepl('<div class="qol-footnotes qol-footnotes-first" style="font-size: 8pt; color: #000000; font-weight: normal; text-align: left">Note A</div>',
                  html_styled, fixed = TRUE), info = "any_table html render does not bold the footnotes by default")

result_per_title <- dummy_df |>
    any_table(rows      = "sex",
              columns   = "year",
              values    = weight,
              statistics = "sum",
              formats   = list(sex = sex.),
              titles    = c("Title A", "Title B", "Title C"),
              footnotes = c("Note A", "Note B"),
              output    = "html",
              print     = FALSE,
              style     = excel_output_style(title_font_color    = c("FF00FF", "00FF00", "0000FF"),
                                             title_font_size     = c(10, 11, 12),
                                             title_font_bold     = c(TRUE, FALSE, TRUE),
                                             title_alignment     = c("left", "center", "right"),
                                             footnote_font_color = c("FF0000", "000000"),
                                             footnote_font_bold  = c(TRUE, FALSE)))

html_per_title <- result_per_title[["html"]]

expect_true(grepl('<div class="qol-titles" style="font-size: 10pt; color: #FF00FF; font-weight: bold; text-align: left">Title A</div>',
                  html_per_title, fixed = TRUE), info = "any_table html render applies the first title font attributes to the first title")
expect_true(grepl('<div class="qol-titles" style="font-size: 11pt; color: #00FF00; font-weight: normal; text-align: center">Title B</div>',
                  html_per_title, fixed = TRUE), info = "any_table html render applies the second title font attributes to the second title")
expect_true(grepl('<div class="qol-titles" style="font-size: 12pt; color: #0000FF; font-weight: bold; text-align: right">Title C</div>',
                  html_per_title, fixed = TRUE), info = "any_table html render applies the third title font attributes to the third title")
expect_true(grepl('<div class="qol-footnotes qol-footnotes-first" style="font-size: 8pt; color: #FF0000; font-weight: bold; text-align: left">Note A</div>',
                  html_per_title, fixed = TRUE), info = "any_table html render applies the first footnote font attributes to the first footnote")
expect_true(grepl('<div class="qol-footnotes" style="font-size: 8pt; color: #000000; font-weight: normal; text-align: left">Note B</div>',
                  html_per_title, fixed = TRUE), info = "any_table html render applies the second footnote font attributes to the second footnote")


# Subheader wrap and cell indents are translated into css
expect_true(grepl('td.subheader { background: #FFFFFF; color: #000000; font-size: 10pt; font-weight: bold; text-align: center; border: 1px solid #000000; white-space: normal; }',
                  html_styled, fixed = TRUE),
            info = "any_table html render wraps the subheader text by default like excel")
expect_true(grepl('th.cat { background: #FFFFFF; color: #000000; font-size: 10pt; font-weight: normal; text-align: left; border: 1px solid #000000; white-space: normal; padding-left: 0.6em; }',
                  html_styled, fixed = TRUE),
            info = "any_table html render indents the category column cells by default like excel")
expect_true(grepl('td.data { background: #FFFFFF; color: #000000; font-size: 10pt; font-weight: normal; text-align: right; border: none; padding-left: 0.6em; }',
                  html_styled, fixed = TRUE),
            info = "any_table html render indents the table cells by default like excel")

result_no_indent <- dummy_df |>
    any_table(rows      = "sex",
              columns   = "year",
              values    = weight,
              statistics = "sum",
              formats   = list(sex = sex.),
              titles    = "T",
              output    = "html",
              print     = FALSE,
              style     = excel_output_style(cat_col_indent = 0, table_indent = 0))

html_no_indent <- result_no_indent[["html"]]

expect_false(grepl("padding-left", html_no_indent, fixed = TRUE),
            info = "any_table writes no indents with indent levels of 0")

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


# any_table aborts with duplicate column names because of NA values
result_list <- dummy_df |>
    any_table(rows    = "education",
              columns = c("sex", "age"),
              values  = weight,
              print   = FALSE)

expect_error(print_stack_as_messages("ERROR"), "Duplicate <columns> names found",
             info = "any_table aborts with duplicate column names because of NA values")


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

result <- combine_into_workbook(1, style = excel_output_style(save_path = dirname(temp_file),
                                                              file      = basename(temp_file)))
expect_error(print_stack_as_messages("ERROR"), "Unknown object found. Provide <any_table> or <export_with_style> results.",
             info = "Combine tables into a single workbook aborts, if no any_table object was found")

result <- combine_into_workbook(list(1), style = excel_output_style(save_path = dirname(temp_file),
                                                                    file      = basename(temp_file)))
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


# any_table aborts, if invalid workbook is passed
result_list <- dummy_df |>
    any_table(rows     = "age",
              columns  = "sex",
              workbook = list("test" = "test"),
              print    = FALSE)

expect_error(print_stack_as_messages("ERROR"), "Workbook object is invalid. You have to provide a workbook object",
             info = "any_table aborts, if invalid workbook is passed")


set_style_options(as_heatmap = FALSE)
set_no_print()
