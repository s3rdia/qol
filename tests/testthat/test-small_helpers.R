###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(100))


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
