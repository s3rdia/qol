###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Get inverse variable namesfrom data frame works correct", {
    var_names     <- c("state", "age", "sex", "education")
    inverse_names <- dummy_df |> inverse(var_names)

    var_count <- length(names(dummy_df))

    expect_equal(length(var_names) + length((inverse_names)), var_count)
    expect_true(!all(var_names %in% inverse_names))
})


test_that("Get inverse variable namesfrom data frame works correct", {
    run_df <- dummy_df |> running_number(by = year)

    expect_true("run_nr" %in% names(run_df))
})


test_that("Add extensions to limited variables", {
    new_names_df <- dummy_df |> add_extension(5, c("sum", "pct"))

    expect_true(all(c("state_sum", "sex_pct") %in% names(new_names_df)))
})


test_that("Add extensions to all remaining variables", {
    new_names_df <- dummy_df |> add_extension(5, c("sum", "pct"), reuse = "last")

    expect_true("weight_pct" %in% names(new_names_df))
})
