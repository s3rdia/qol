set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- dummy_data(100)
dup_df   <- data.frame(age = 1, AGE = 2, Age = 3,
                       sex = 1, SeX = 2,
                       Height_male = 1, Height_female = 1,
                       weight = 2)

test_df <- data.frame(var1 = c(-2.5, -2.4, 2.4, 2.5),
                      var2 = c(2.4, NA, NA, 2.5),
                      var3 = c("a", "b", "c", "d"))


# Add new NA column range with helper function
added_df <- dummy_df |>
      add_variable_range(status1:status3)

expect_equal(utils::tail(names(added_df), 3), c("status1", "status2", "status3"), info = "Add new NA column range with helper function")


# Get length of integer variable
expect_equal(get_integer_length(1L), 1,   info = "Get length of integer variable")
expect_equal(get_integer_length(10L), 2,  info = "Get length of integer variable")
expect_equal(get_integer_length(100L), 3, info = "Get length of integer variable")


# Get duplicate variable names
expect_equal(get_duplicate_var_names(dup_df),
             c("age", "AGE", "Age", "sex", "SeX"), info = "Get duplicate variable names")


# Get duplicate variable count
expect_equal(get_duplicate_var_count(dup_df), 2, info = "Get duplicate variable count")


# Round values half up
values_to_round <- c(-2.5, -2.4, -0.5, -0.4, 0.4, 0.5, 2.4, 2.5)

expect_equal(round_values(values_to_round), c(-3, -2, -1, 0, 0, 1, 2, 3), info = "Round values half up")


# Round to multiples
values_to_round <- c(-7.5, -7.4, -2.5, -2.4, -0.5, -0.4, 0.4, 0.5, 2.4, 2.5, 7.4, 7.5)

expect_equal(round_values(values_to_round, multiple = 5), c(-10, -5, -5, 0, 0, 0, 0, 0, 0, 5, 5, 10),
             info = "Round to multiples")


# Round multiple variables in a data frame half up
new_df <- test_df |> round_multi(c(var1, var2))

expect_equal(new_df[["var1"]], c(-3, -2, 2, 3), info = "Round multiple variables in a data frame half up")
expect_equal(new_df[["var2"]], c(2, NA, NA, 3), info = "Round multiple variables in a data frame half up")


# Round multiple variables in a data frame to multiples
new_df <- test_df |> round_multi(c(var1, var2), multiple = 5)

expect_equal(new_df[["var1"]], c(-5, 0, 0, 5), info = "Round multiple variables in a data frame to multiples")
expect_equal(new_df[["var2"]], c(0, NA, NA, 5), info = "Round multiple variables in a data frame to multiples")


# Round multiple variables in a data frame and add them as new variables
new_df <- test_df |> round_multi(variables = c(var1, var2), new_names = c(round1, round2))

expect_equal(new_df[["round1"]], c(-3, -2, 2, 3),   info = "Round multiple variables in a data frame and add them as new variables")
expect_equal(new_df[["round2"]], c(2, NA, NA, 3),   info = "Round multiple variables in a data frame and add them as new variables")
expect_true(collapse::fsum(new_df[["var1"]] == new_df[["round1"]]) == 0,
            info = "Round multiple variables in a data frame and add them as new variables")
expect_true(collapse::fsum(new_df[["var2"]] == new_df[["round2"]]) == 0,
            info = "Round multiple variables in a data frame and add them as new variables")

###############################################################################
# Warning checks
###############################################################################


# Warning in length of integer if numeric value provided
value <- 100.123
get_integer_length(value)

expect_warning(print_stack_as_messages("WARNING"), "Variable is not an integer and will be floored. Decimal places won't count.", info = "Warning in length of integer if numeric value provided")

expect_equal(get_integer_length(value), 3, info = "Warning in length of integer if numeric value provided")

###############################################################################
# Abort checks
###############################################################################

# Adding NA variables to data frame aborts on wrong pattern
added_df <- dummy_df |>
     add_variable_range(status1:age3)

expect_error(print_stack_as_messages("ERROR"), "Variable range has to be provided in the form 'var_name1:var_name10'.", info = "Adding NA variables to data frame aborts on wrong pattern")


# Adding NA variables aborts, if variables are already part of data frame
added_df <- dummy_df |>
     add_variable_range(status1:status3)

added_df2 <- added_df |>
     add_variable_range(status1:status3)

expect_error(print_stack_as_messages("ERROR"), "Some variables are already part of the data frame: ", info = "Adding NA variables aborts, if variables are already part of data frame")


# Abort length of integer if character variable provided
get_integer_length("Test")

expect_error(print_stack_as_messages("ERROR"), "Only numeric values allowed.", info = "Abort length of integer if character variable provided")


# Abort round values if values are not numeric
round_values("Test")

expect_error(print_stack_as_messages("ERROR"), "Only numeric values allowed.", info = "Abort round values if values are not numeric")


# Round multiple variables in a data frame aborts on unequal variable name lengths
new_df <- test_df |> round_multi(variables = c(var1, var2), new_names = c(round1, round2, round3))

expect_error(print_stack_as_messages("ERROR"), "<Variables> and <new_names> are of unequal length. Rounding will be aborted.",
             info = "Round multiple variables in a data frame aborts on unequal variable name lengths")


# Round multiple variables in a data frame throws a warning on non numeric variables
new_df <- test_df |> round_multi(variables = c(var1, var2, var3), new_names = c(round1, round2, round3))

expect_warning(print_stack_as_messages("WARNING"), "Only numeric values allowed. var3 is not numeric, and will",
             info = "Round multiple variables in a data frame throws a warning on non numeric variables")


set_no_print()
