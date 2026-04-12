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
result_df1 <- dummy_df |>
     crosstabs(rows    = age,
               columns = sex,
               by      = education,
               weight  = weight,
               print   = FALSE)

result_df2 <- dummy_df |>
     crosstabs(rows    = "age",
               columns = "sex",
               by      = "education",
               weight  = "weight",
               print   = FALSE)

expect_identical(result_df1, result_df2, info = "Different way of passing variables in")


# Simplest form of crosstabs
result_df <- dummy_df |>
    crosstabs(rows    = age,
              columns = sex,
              print   = FALSE)

expect_inherits(result_df, "data.table", info = "Simplest form of crosstabs")

values <- length(unique(dummy_df[["sex"]]))
expect_equal(collapse::fncol(result_df), (values * 4) + 1, info = "Simplest form of crosstabs")


# crosstabs with titles and footnotes
result_df <- dummy_df |>
    crosstabs(rows      = age,
              columns   = sex,
              titles    = "Hello world",
              footnotes = "This is a footnote",
              print     = FALSE)

expect_inherits(result_df, "data.table", info = "crosstabs with titles and footnotes")

values <- length(unique(dummy_df[["sex"]]))
expect_equal(collapse::fncol(result_df), (values * 4) + 1, info = "crosstabs with titles and footnotes")


# crosstabs not allowed with multiple row variables
result_df <- dummy_df |>
    crosstabs(rows    = c(age, state),
              columns = sex,
              print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Only one variable for <rows> allowed. First variable will be used.", info = "crosstabs not allowed with multiple row variables")


# crosstabs not allowed with multiple column variables
result_df <- dummy_df |>
     crosstabs(rows    = sex,
               columns = c(age, state),
               print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Only one variable for <columns> allowed. First variable will be used.", info = "crosstabs not allowed with multiple column variables")


# crosstabs with by variables
result_df <- dummy_df |>
    crosstabs(rows    = age,
              columns = sex,
              by      = education,
              print   = FALSE)

expect_true("BY" %in% names(result_df), info = "crosstabs with by variables")
expect_equal(length(unique(result_df[["BY"]])), 1, info = "crosstabs with by variables")


# crosstabs with multiple by variables
result_df <- dummy_df |>
    crosstabs(rows    = age,
              columns = sex,
              by      = c(education, year),
              print   = FALSE)

expect_true("BY" %in% names(result_df), info = "crosstabs with multiple by variables")
expect_equal(length(unique(result_df[["BY"]])), 2, info = "crosstabs with multiple by variables")


# crosstabs where by is also part of rows or columns is aborted
result_list <- dummy_df |>
       crosstabs(rows    = age,
                 columns = sex,
                 by      = c(sex, year),
                 print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable '", info = "crosstabs where by is also part of rows or columns is aborted")


# crosstabs with weighted results
result_df1 <- dummy_df |>
     crosstabs(rows    = age,
               columns = sex,
               weight  = weight,
               print   = FALSE)

result_df2 <- dummy_df |>
     crosstabs(rows    = age,
               columns = sex,
               print   = FALSE)

expect_false(identical(as.numeric(result_df1[["var_sum_1"]]),
                       as.numeric(result_df2[["var_sum_1"]])), info = "crosstabs with weighted results")


# crosstabs with NAs removed
result_df <- dummy_df |>
    crosstabs(rows    = age,
              columns = sex,
              na.rm   = TRUE,
              print   = FALSE)

expect_true(sum(is.na(result_df[["fused_vars"]])) == 0, info = "crosstabs with NAs removed")


# crosstabs with multiple statistics
result_df <- dummy_df |>
      crosstabs(rows       = age,
                columns    = sex,
                statistics = c("sum", "freq", "pct_row", "pct_column", "pct_total"),
                print      = FALSE)

values <- length(unique(dummy_df[["sex"]]))
expect_equal(collapse::fncol(result_df), (values * 4) + 1, info = "crosstabs with multiple statistics")


# Apply single discrete labels
format_df <- dummy_df |>
    crosstabs(rows    = sex,
              columns = age,
              formats = list(sex = discrete_format(
                  "Male"   = 1,
                  "Female" = 2)),
              print   = FALSE)

no_format_df <- dummy_df |>
   crosstabs(rows    = sex,
             columns = age,
             print   = FALSE)

expect_equal(collapse::fsum(format_df[["var_sum_1"]]),
             collapse::fsum(no_format_df[["var_sum_1"]]), info = "Apply single discrete labels")
expect_true(all(c("Male", "Female")
                %in% format_df[["sex"]]), info = "Apply single discrete labels")


# Apply discrete multilabel
format_df <- dummy_df |>
    crosstabs(rows    = sex,
              columns = age,
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
              print   = FALSE)

expect_true(all(c("Total", "Male", "Female")
                %in% format_df[["sex"]]), info = "Apply discrete multilabel")


# Apply interval multilabel
format_df <- dummy_df |>
    crosstabs(rows    = income,
              columns = sex,
              formats = list(income = interval_format(
                    "Total"              = 0:99999,
                    "below 500"          = 0:499,
                    "500 to under 1000"  = 500:999,
                    "1000 to under 2000" = 1000:1999,
                    "2000 and more"      = 2000:99999)),
              print   = FALSE)

expect_true(all(c("Total", "below 500", "2000 and more")
                %in% format_df[["income"]]), info = "Apply interval multilabel")


# crosstabs row multilabel leads to notification
format_df <- dummy_df |>
      crosstabs(rows    = sex,
                columns = age,
                formats = list(sex = discrete_format(
                    "Total"  = 1:2,
                    "Male"   = 1,
                    "Female" = 2)),
                statistics = c("pct_row"),
                print   = FALSE)

expect_message(print_stack_as_messages("NOTE"), "The format for variable 'sex' is a multilabel", info = "crosstabs row multilabel leads to notification")


# crosstabs row multilabel leads to notification with by variables
format_df <- dummy_df |>
       crosstabs(rows    = sex,
                 columns = age,
                 by      = year,
                 formats = list(sex = discrete_format(
                     "Total"  = 1:2,
                     "Male"   = 1,
                     "Female" = 2)),
                 statistics = c("pct_row"),
                 print   = FALSE)

expect_message(print_stack_as_messages("NOTE"), "The format for variable 'sex' is a multilabel", info = "crosstabs row multilabel leads to notification with by variables")

###############################################################################
# The Excel tests are kept simpler because the whole stat evaluation is the
# same as before. It serves mainly to let the code run through all the formatting
# lines.
###############################################################################

# crosstabs with excel output
result_df <- dummy_df |>
    crosstabs(rows    = age,
              columns = sex,
              output  = "excel_nostyle",
              print   = FALSE)

expect_inherits(result_df, "data.table", info = "crosstabs with excel output")

values <- length(unique(dummy_df[["sex"]]))
expect_equal(collapse::fncol(result_df), (values * 4) + 1, info = "crosstabs with excel output")


# crosstabs with titles and footnotes and weight (excel)
result_df <- dummy_df |>
    crosstabs(rows      = age,
              columns   = sex,
              output    = "excel",
              titles    = "Hello world",
              footnotes = "This is a footnote",
              weight    = weight,
              print     = FALSE)

expect_inherits(result_df, "data.table", info = "crosstabs with titles and footnotes and weight (excel)")

values <- length(unique(dummy_df[["sex"]]))
expect_equal(collapse::fncol(result_df), (values * 4) + 1, info = "crosstabs with titles and footnotes and weight (excel)")


# crosstabs with excel output and by variables (excel)
result_df <- dummy_df |>
    crosstabs(rows    = age,
              columns = sex,
              by      = education,
              output  = "excel_nostyle",
              print_miss = TRUE, # Has no value here but the test runs faster
              print   = FALSE)

expect_true("BY" %in% names(result_df), info = "crosstabs with excel output and by variables (excel)")


# crosstabs with fast none styled excel output (excel)
result_df <- dummy_df |>
      crosstabs(rows    = age,
                columns = sex,
                output  = "excel_nostyle",
                print   = FALSE)

expect_inherits(result_df, "data.table", info = "crosstabs with fast none styled excel output (excel)")

values <- length(unique(dummy_df[["sex"]]))
expect_equal(collapse::fncol(result_df), (values * 4) + 1, info = "crosstabs with fast none styled excel output (excel)")


# crosstabs row multilabel leads to notification (excel)
format_df <- dummy_df |>
       crosstabs(rows    = sex,
                 columns = age,
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
                 statistics = c("sum", "freq", "pct_row", "pct_column", "pct_total"),
                 output  = "excel_nostyle",
                 print   = FALSE)

expect_message(print_stack_as_messages("NOTE"), "The format for variable 'sex' is a multilabel", info = "crosstabs row multilabel leads to notification (excel)")


# crosstabs row multilabel leads to notification (excel)
format_df <- dummy_df |>
       crosstabs(rows    = sex,
                 columns = age,
                 formats = list(sex = discrete_format(
                     "Total"  = 1:2,
                     "Male"   = 1,
                     "Female" = 2)),
                 statistics = c("pct_row"),
                 output  = "excel",
                 print   = FALSE)

expect_message(print_stack_as_messages("NOTE"), "The format for variable 'sex' is a multilabel", info = "crosstabs row multilabel leads to notification (excel)")


# crosstabs row multilabel leads to notification with by variables (excel)
format_df <- dummy_df |>
       crosstabs(rows    = sex,
                 columns = age,
                 by      = year,
                 formats = list(sex = discrete_format(
                     "Total"  = 1:2,
                     "Male"   = 1,
                     "Female" = 2)),
                 statistics = c("pct_row"),
                 output     = "excel_nostyle",
                 print_miss = TRUE,
                 print      = FALSE)

expect_message(print_stack_as_messages("NOTE"), "The format for variable 'sex' is a multilabel", info = "crosstabs row multilabel leads to notification with by variables (excel)")


# Invalid output format leads to console output
result_df <- dummy_df |>
       crosstabs(rows    = age,
                 columns = sex,
                 by      = education,
                 output  = "test",
                 print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "<Output> format 'test' not available. Using 'console' instead.", info = "Invalid output format leads to console output")


# Save crosstabs as Excel file
temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

dummy_df |>
     crosstabs(rows    = age,
               columns = sex,
               output  = "excel",
               style   = excel_output_style(save_path = dirname(temp_file),
                                            file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "Save crosstabs as Excel file")


set_style_options(as_heatmap = FALSE)


set_no_print()
