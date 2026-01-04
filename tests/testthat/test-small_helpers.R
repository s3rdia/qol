###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df1 <- suppressMessages(dummy_data(10))
dummy_df2 <- suppressMessages(dummy_data(10))
dummy_df3 <- suppressMessages(dummy_data(10))


test_that("Stack data fames", {
    new_df1 <- set(dummy_df1, dummy_df2, dummy_df3)
    new_df2 <- set(dummy_df1, dummy_df2, dummy_df3, compress = "factor")

    expect_equal(nrow(new_df1), 30)
    expect_equal(nrow(new_df2), 30)
    expect_equal(class(new_df1[["education"]]), "character")
    expect_equal(class(new_df2[["education"]]), "factor")
})


test_that("Stack data fames with id column", {
    new_df <- set(dummy_df1, dummy_df2, dummy_df3, id = TRUE)

    expect_equal(max(new_df[["ID"]]), 3)
})


test_that("Add new NA column range with helper function", {
    added_df <- suppressMessages(dummy_df1 |>
          add_variable_range(status1:status3))

    expect_equal(utils::tail(names(added_df), 3), c("status1", "status2", "status3"))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Adding NA variables to data frame aborts on wrong pattern", {
    expect_message(added_df <- dummy_df1 |>
         add_variable_range(status1:age3), " X ERROR: Variable range has to be provided in the form 'var_name1:var_name10'.")
})


test_that("Adding NA variables aborts, if variables are already part of data frame", {
    added_df <- suppressMessages(dummy_df1 |>
         add_variable_range(status1:status3))

    expect_message(added_df2 <- added_df |>
         add_variable_range(status1:status3), " X ERROR: Some variables are already part of the data frame: ")
})
