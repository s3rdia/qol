###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df1 <- suppressMessages(dummy_data(10))
dummy_df2 <- suppressMessages(dummy_data(10))
dummy_df3 <- suppressMessages(dummy_data(10))

test_that("Stack data fames", {
    new_df <- set(dummy_df1, dummy_df2, dummy_df3)

    expect_equal(nrow(new_df), 30)
})

test_that("Stack data fames with id column", {
    new_df <- set(dummy_df1, dummy_df2, dummy_df3, id = TRUE)

    expect_equal(max(new_df[["ID"]]), 3)
})
