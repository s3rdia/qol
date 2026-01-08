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