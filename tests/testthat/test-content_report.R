###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(10))


test_that("multiplication works", {
    result_list <- content_report(dummy_df)

    expect_equal(names(result_list), c("global", "variables"))
})
