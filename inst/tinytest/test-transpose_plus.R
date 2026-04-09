set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

dummy_df      <- dummy_data(1000)
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

expect_equal(names(wide_df), c("year", "1", "2", NA), info = "Simple long to wide transposition")


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
expect_equal(as.character(collapse::funique(wide_to_long[["VARIABLE"]])), c("Total", "Male", "Female", "high", "low", "middle"), info = "Simple wide to long transposition")

###############################################################################
# Warning checks
###############################################################################


# Wide to long transposition doesn't support value parameter transposition
wide_df <- dummy_wide_df |>
    transpose_plus(preserve = year,
                   pivot    = list(sex = c("Male", "Female")),
                   values   = "Total")

expect_message(print_stack_as_messages("NOTE"), "<Values> parameter has no effect in wide to long transposition.", info = "Wide to long transposition doesn't support value parameter transposition")


# Wide to long transposition doesn't support weight parameter transposition
wide_df <- dummy_wide_df |>
   transpose_plus(preserve = year,
                  pivot    = list(sex = c("Male", "Female")),
                  weight   = "Total")

expect_message(print_stack_as_messages("NOTE"), "<Weight> parameter has no effect in wide to long transposition.", info = "Wide to long transposition doesn't support weight parameter transposition")


# Preserve variable in transposition is not part of the data frame
wide_df <- dummy_df |>
   transpose_plus(preserve = "test",
                  pivot    = "sex",
                  value    = income)

expect_warning(print_stack_as_messages("WARNING"), "The provided <preserve> variable", info = "Preserve variable in transposition is not part of the data frame")


# Value variable in transposition is also part of preserve
wide_df <- dummy_df |>
   transpose_plus(preserve = sex,
                  pivot    = "age",
                  value    = sex)

expect_warning(print_stack_as_messages("WARNING"), "The provided <values> variable", info = "Value variable in transposition is also part of preserve")

###############################################################################
# Abort checks
###############################################################################

# Wide to long transposition doesn't support nesting variables
wide_df <- dummy_wide_df |>
			transpose_plus(preserve = year,
						   pivot    = list(sex = "Male + Female"))

expect_error(print_stack_as_messages("ERROR"), "Nesting <pivot> variables in a wide to long transposition is not possible.", info = "Wide to long transposition doesn't support nesting variables")


# Abort transposition if pivot variable is part of preserve
wide_df <- dummy_wide_df |>
			transpose_plus(preserve = year,
						   pivot    = "year",
						   value    = income)

expect_error(print_stack_as_messages("ERROR"), "The provided <pivot> variable", info = "Abort transposition if pivot variable is part of preserve")


# Abort transposition if no valid value variable is provided
wide_df <- dummy_df |>
			transpose_plus(preserve = year,
						   pivot    = "sex")

expect_error(print_stack_as_messages("ERROR"), "No <values> provided. Transposition will be aborted.", info = "Abort transposition if no valid value variable is provided")


# Abort transposition if value variable is not part of the data frame
wide_df <- dummy_df |>
			transpose_plus(pivot    = "sex",
						   value    = test)

expect_error(print_stack_as_messages("ERROR"), "No valid <values> to transpose provided. Transposition will be aborted.", info = "Abort transposition if value variable is not part of the data frame")


# Abort if value variable in transposition is also part of pivot
wide_df <- dummy_df |>
			transpose_plus(pivot    = "sex",
						   value    = sex)

expect_error(print_stack_as_messages("ERROR"), "The provided <values> variable", info = "Abort if value variable in transposition is also part of pivot")


# Abort on duplicate variable names after transposition
wide_df <- dummy_df |>
			transpose_plus(preserve = year,
						   pivot    = c("sex", "education"),
						   values   = income)

expect_error(print_stack_as_messages("ERROR"), "Duplicate column names found:", info = "Abort on duplicate variable names after transposition")


# Abort if no valid pivot variable is provided in transposition
wide_df <- dummy_df |>
			transpose_plus(pivot = "test",
						   value = income)

expect_error(print_stack_as_messages("ERROR"), "The provided <pivot> variable", info = "Abort if no valid pivot variable is provided in transposition")


set_no_print()
