###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Add extensions to limited variables", {
    new_names_df <- dummy_df |> add_extension(5, c("sum", "pct"))

    expect_true(all(c("state_sum", "sex_pct") %in% names(new_names_df)))
})


test_that("Add extensions to all remaining variables", {
    new_names_df <- dummy_df |> add_extension(5, c("sum", "pct"), reuse = "last")

    expect_true("weight_pct" %in% names(new_names_df))
})
