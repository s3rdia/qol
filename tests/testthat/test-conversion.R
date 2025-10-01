###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))
dummy_df[["age"]] <- as.character(dummy_df[["age"]])


test_that("if. can convert values conditionally", {
    test_df1 <- dummy_df |> convert_numeric("age")
    test_df2 <- dummy_df |> convert_numeric("education")

    expect_true(is.numeric(test_df1[["age"]]))
    expect_true(is.character(test_df2[["age"]]))
    expect_true(is.character(test_df2[["education"]]))
})
