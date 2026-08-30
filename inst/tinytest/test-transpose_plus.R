set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

dummy_df      <- dummy_data(1000)
no_na_df      <- dummy_data(1000, insert_na = FALSE)
dummy_wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = c("sex", "education"),
                   values   = income,
                   na.rm    = TRUE,
                   formats  = list(sex = discrete_format(
                       "Total"  = 1:2,
                       "Male"   = 1,
                       "Female" = 2)))


# Simple long to wide transposition
dummy_df <- dummy_df |> sort_plus(by = sex)

wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = "sex",
                   values   = income,
                   weight   = weight)

expect_equal(names(wide_df), c("year", "1", "2", "NA"), info = "Simple long to wide transposition")


# Long to wide without values supplied creates counts
wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = "sex",
                   na.rm    = TRUE)

expect_true(all(c("year", "1", "2") %in% names(wide_df)),        info = "Long to wide without values supplied creates counts")
expect_true(all(wide_df[["1"]] >= 1) & all(wide_df[["2"]] >= 1), info = "Long to wide without values supplied creates counts")


# Long to wide without summarise transposes actual values
wide_df <- dummy_df |>
    transpose_plus(preserve  = year,
                   pivot     = "sex",
                   summarise = FALSE,
                   na.rm     = TRUE)

expect_true(all(c("year", "1", "2") %in% names(wide_df)),        info = "Long to wide without summarise transposes values")
expect_true(all(wide_df[["1"]] == 1) & all(wide_df[["2"]] == 1), info = "Long to wide without summarise transposes values")

wide_df <- dummy_df |>
    transpose_plus(preserve  = year,
                   pivot     = "person_id",
                   values    = c("sex", "education"),
                   summarise = FALSE,
                   na.rm     = TRUE)

wide_sex_cols  <- grep("^sex",       names(wide_df), value = TRUE)
wide_edu_cols  <- grep("^education", names(wide_df), value = TRUE)

expect_true(length(wide_sex_cols) > 1, info = "Long to wide without summarise transposes actual values")
expect_true(length(wide_edu_cols) > 1, info = "Long to wide without summarise transposes actual values")


# Invalid value variable falls back to producing counts
wide_df <- dummy_df |>
    transpose_plus(pivot  = "sex",
                   values = test)

expect_true(all(wide_df[["1"]] >= 1) & all(wide_df[["2"]] >= 1), info = "Invalid value variable falls back to producing counts")


# Variable combinations inside a multiple pivot transposition add up to toal individually
no_na_df <- no_na_df |> sort_plus(by = c(sex, education))
sex. <- discrete_format("Total"  = 1:2,
                        "Male"   = 1,
                        "Female" = 2)

wide_df <- no_na_df |>
    transpose_plus(preserve = year,
                   pivot    = c("sex", "education", "sex + education"),
                   na.rm    = TRUE,
                   formats  = list(sex = sex.))

expect_true(all(wide_df[["Total"]] - (wide_df[["Male"]] + wide_df[["Female"]]) == 0),
            info = "Single variables inside a multiple pivot transposition add up to toal individually")
expect_true(all(wide_df[["Total"]] - (wide_df[["low"]] + wide_df[["middle"]] + wide_df[["high"]]) == 0),
            info = "Single variables inside a multiple pivot transposition add up to toal individually")
expect_true(all(wide_df[["Total"]] - (wide_df[["Total_low"]] + wide_df[["Total_middle"]] + wide_df[["Total_high"]]) == 0),
            info = "Single variables inside a multiple pivot transposition add up to toal individually")


# Side by side long to wide transposition
dummy_df <- dummy_df |> sort_plus(by = c(sex, education))

wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = c("sex", "education"),
                   values   = income,
                   na.rm    = TRUE)

expect_equal(names(wide_df), c("year", "1", "2", "high", "low", "middle"), info = "Side by side long to wide transposition")


# Nested long to wide transposition
dummy_df <- dummy_df |> sort_plus(by = c(sex, education))

wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = "sex + education",
                   values   = income,
                   na.rm    = TRUE)

expect_equal(names(wide_df), c("year", "1_high", "1_low", "1_middle",
                                       "2_high", "2_low", "2_middle"), info = "Nested long to wide transposition")


# Transpose multiple value variables
dummy_df <- dummy_df |> sort_plus(by = sex)
wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = "sex",
                   values   = c(income, weight),
                   na.rm    = TRUE)

expect_equal(names(wide_df), c("year", "income_1", "income_2", "weight_1", "weight_2"), info = "Transpose multiple value variables")


# Using formats in long to wide transposition
wide_df <- dummy_df |>
    transpose_plus(preserve = year,
                   pivot    = "sex",
                   values   = income,
                   na.rm    = TRUE,
                   formats  = list(sex =
                       discrete_format("Total"  = 1:2,
                                       "Male"   = 1,
                                       "Female" = 2)))

expect_equal(names(wide_df), c("year", "Total", "Male", "Female"), info = "Using formats in long to wide transposition")


# Tranpose keeps statistic extensions, if multiple statistics are selected
result_df <- dummy_df |>
    transpose_plus(preserve   = year,
                   pivot      = "sex",
                   values     = weight,
                   statistics = c("sum", "freq"))

expect_true(all(c("weight_sum_1", "weight_sum_2", "weight_sum_NA", "weight_freq_1", "weight_freq_2", "weight_freq_NA") %in% names(result_df)),
            info = "Tranpose keeps statistic extensions, if multiple statistics are selected")


# Tranpose is able to output specific statistics per variable
result_df <- dummy_df |>
    transpose_plus(preserve   = year,
                   pivot      = "sex",
                   statistics = list("sum"       = weight,
                                     "pct_group" = income))

expect_true(all(c("weight_1", "weight_2", "weight_NA", "income_1", "income_2", "income_NA") %in% names(result_df)),
            info = "Tranpose is able to output specific statistics per variable")
expect_equal(collapse::funique(round(result_df[["income_1"]] + result_df[["income_2"]] + result_df[["income_NA"]])), 100,
             info = "Tranpose is able to output specific statistics per variable")


# Simple wide to long transposition
wide_to_long <- dummy_wide_df |>
        transpose_plus(preserve = year,
                       pivot    = list(sex = c("Total", "Male", "Female")))

expect_equal(names(wide_to_long), c("year", "sex", "VALUE"), info = "Simple wide to long transposition")
expect_equal(collapse::funique(wide_to_long[["sex"]]), c("Total", "Male", "Female"), info = "Simple wide to long transposition")


# Transpose multiple variables from wide to long
wide_to_long <- dummy_wide_df |>
         transpose_plus(preserve = year,
                        pivot    = list(sex       = c("Male", "Female"),
                                        education = c("low", "middle", "high")),
                        formats  = list(sex =
                            discrete_format("Total"  = c("Male", "Female"),
                                            "Male"   = "Male",
                                            "Female" = "Female")))

expect_equal(names(wide_to_long), c("year", "BY", "VARIABLE", "VALUE"), info = "Transpose multiple variables from wide to long")
expect_equal(as.character(collapse::funique(wide_to_long[["VARIABLE"]])), c("Total", "Male", "Female", "high", "low", "middle"),
             info = "Simple wide to long transposition")


# Transpose multiple variables from wide to long (side by side)
wide_to_long <- dummy_wide_df |>
    transpose_plus(preserve = year,
                   pivot    = list(sex = c("Total", "Male", "Female"),
                                   sex = c("low", "middle", "high")))

expect_equal(names(wide_to_long), c("year", "sex", "value1", "value2"),
             info = "Transpose multiple variables from wide to long (side by side)")

wide_to_long <- dummy_wide_df |>
    transpose_plus(preserve = year,
                   pivot    = list(sex = c("Total", "Male", "Female"),
                                   sex = c("low", "middle", "high")),
                   values   = c(hello, world))

expect_equal(names(wide_to_long), c("year", "sex", "hello", "world"),
             info = "Transpose multiple variables from wide to long (side by side)")



###############################################################################
# Warning checks
###############################################################################

# Wide to long transposition doesn't support value parameter transposition
wide_df <- dummy_wide_df |>
    transpose_plus(preserve = year,
                   pivot    = list(sex = c("Male", "Female")),
                   values   = "Total")

expect_message(print_stack_as_messages("NOTE"), "<Values> parameter has no effect in wide to long transposition, when results are stacked.",
			   info = "Wide to long transposition doesn't support value parameter transposition")


# Wide to long transposition doesn't support weight parameter transposition
wide_df <- dummy_wide_df |>
   transpose_plus(preserve = year,
                  pivot    = list(sex = c("Male", "Female")),
                  weight   = "Total")

expect_message(print_stack_as_messages("NOTE"), "<Weight> parameter has no effect in wide to long transposition.",
               info = "Wide to long transposition doesn't support weight parameter transposition")


# Preserve variable in transposition is not part of the data frame
wide_df <- dummy_df |>
   transpose_plus(preserve = "test",
                  pivot    = "sex",
                  values   = income)

expect_warning(print_stack_as_messages("WARNING"), "The provided <preserve> variable",
               info = "Preserve variable in transposition is not part of the data frame")


# Value variable in transposition is also part of preserve
wide_df <- dummy_df |>
   transpose_plus(preserve = sex,
                  pivot    = "age",
                  values   = sex)

expect_warning(print_stack_as_messages("WARNING"), "The provided <values> variable",
               info = "Value variable in transposition is also part of preserve")

###############################################################################
# Abort checks
###############################################################################

# Wide to long transposition doesn't support nesting variables
wide_df <- dummy_wide_df |>
			transpose_plus(preserve = year,
						   pivot    = list(sex = "Male + Female"))

expect_error(print_stack_as_messages("ERROR"), "Nesting <pivot> variables in a wide to long transposition is not possible.",
             info = "Wide to long transposition doesn't support nesting variables")


# Abort transposition if pivot variable is part of preserve
wide_df <- dummy_wide_df |>
			transpose_plus(preserve = year,
						   pivot    = "year",
						   values   = income)

expect_error(print_stack_as_messages("ERROR"), "The provided <pivot> variable",
             info = "Abort transposition if pivot variable is part of preserve")


# Abort if value variable in transposition is also part of pivot
wide_df <- dummy_df |>
			transpose_plus(pivot  = "sex",
						   values = sex)

expect_error(print_stack_as_messages("ERROR"), "The provided <values> variable",
             info = "Abort if value variable in transposition is also part of pivot")


# Abort on duplicate variable names after transposition
wide_df <- dummy_df |>
			transpose_plus(preserve = year,
						   pivot    = c("sex", "education"),
						   values   = income)

expect_error(print_stack_as_messages("ERROR"), "Duplicate column names found:",
             info = "Abort on duplicate variable names after transposition")


# Abort if no valid pivot variable is provided in transposition
wide_df <- dummy_df |>
			transpose_plus(pivot  = "test",
						   values = income)

expect_error(print_stack_as_messages("ERROR"), "The provided <pivot> variable",
             info = "Abort if no valid pivot variable is provided in transposition")


# Abort side by side transposition, if list entries are of unequal lengths
wide_df <- dummy_wide_df |>
    transpose_plus(preserve = year,
                   pivot    = list(sex = c("Total", "Male", "Female", "low"),
                                   sex = c("low", "middle", "high")))

expect_error(print_stack_as_messages("ERROR"), "Every <pivot> list entry has to have the same number of variables for a",
             info = "Abort side by side transposition, if list entries are of unequal lengths")


# Abort side by side transposition, if list entries contain a unique variable name but also others
wide_df <- dummy_wide_df |>
    transpose_plus(preserve = year,
                   pivot    = list(sex  = c("Total", "Male", "Female", "low"),
                                   sex  = c("low", "middle", "high"),
                                   test = c("low", "middle", "high")))

expect_error(print_stack_as_messages("ERROR"), "The new result columns can only be set side by side in a wide",
             info = "Abort side by side transposition, if list entries contain a unique variable name but also others")


set_no_print()
