###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(100))
dup_df <- data.frame(age = 1, AGE = 2, Age = 3,
                     sex = 1, SeX = 2,
                     Height_male = 1, Height_female = 1,
                     weight = 2)


test_that("Add new NA column range with helper function", {
    added_df <- suppressMessages(dummy_df |>
          add_variable_range(status1:status3))

    expect_equal(utils::tail(names(added_df), 3), c("status1", "status2", "status3"))
})


test_that("Get length of integer variable", {
    expect_equal(get_integer_length(1L), 1)
    expect_equal(get_integer_length(10L), 2)
    expect_equal(get_integer_length(100L), 3)
})


test_that("Get duplicate variable names", {
    expect_equal(get_duplicate_var_names(dup_df),
                 c("age", "AGE", "Age", "sex", "SeX"))
})


test_that("Get duplicate variable count", {
    expect_equal(get_duplicate_var_count(dup_df), 2)
})


test_that("Round values half up", {
    values_to_round <- c(-2.5, -2.4, -0.5, -0.4, 0.4, 0.5, 2.4, 2.5)
	
    expect_equal(round_values(values_to_round), c(-3, -2, -1, 0, 0, 1, 2, 3))
})

###############################################################################
# Warning checks
###############################################################################


test_that("Warning in length of integer if numeric value provided", {
    value <- 100.123
    expect_message(get_integer_length(value), " ! WARNING: Variable is not an integer and will be floored. Decimal places won't count.")

    expect_equal(get_integer_length(value), 3)
})

###############################################################################
# Abort checks
###############################################################################

test_that("Adding NA variables to data frame aborts on wrong pattern", {
    expect_message(added_df <- dummy_df |>
         add_variable_range(status1:age3), " X ERROR: Variable range has to be provided in the form 'var_name1:var_name10'.")
})


test_that("Adding NA variables aborts, if variables are already part of data frame", {
    added_df <- suppressMessages(dummy_df |>
         add_variable_range(status1:status3))

    expect_message(added_df2 <- added_df |>
         add_variable_range(status1:status3), " X ERROR: Some variables are already part of the data frame: ")
})


test_that("Abort length of integer if character variable provided", {
    expect_message(get_integer_length("Test"), " X ERROR: Only numeric values allowed.")
})


test_that("Abort round values if values are not numeric", {
    expect_message(round_values("Test"), " X ERROR: Only numeric values allowed.")
})