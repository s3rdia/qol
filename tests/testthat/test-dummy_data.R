###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_that("Create dummy data with 1 observations", {
    test_df <- suppressMessages(dummy_data(1))
    expect_equal(nrow(test_df), 1)
})


test_that("Create dummy data with 10 observations", {
    test_df <- suppressMessages(dummy_data(10))
    expect_equal(nrow(test_df), 10)
})


test_that("Create dummy data with 100 observations", {
    test_df <- suppressMessages(dummy_data(100))
    expect_equal(nrow(test_df), 100)
})


test_that("Create dummy data with 100 observations", {
    test_df <- suppressMessages(dummy_data(1000))
    expect_equal(nrow(test_df), 1000)
})
