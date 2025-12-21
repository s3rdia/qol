###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Get inverse variable names from data frame works correct", {
    var_names     <- c("state", "age", "sex", "education")
    inverse_names <- dummy_df |> inverse(var_names)

    var_count <- length(names(dummy_df))

    expect_equal(length(var_names) + length((inverse_names)), var_count)
    expect_true(!all(var_names %in% inverse_names))
})


test_that("Get variable names between two variables", {
    var_names <- dummy_df |> vars_between(state, income)

    expect_equal(length(var_names), 5)
})


test_that("Get variable names from variable to the end", {
    var_names <- dummy_df |> vars_between(state)

    expect_equal(length(var_names), 7)
})


test_that("Get variable names from beginning to position", {
    var_names <- dummy_df |> vars_between(to = state)

    expect_equal(length(var_names), 5)
})

###############################################################################
# Warning checks
###############################################################################

test_that("Providing not existing from-variable throws a warning", {
    expect_message(var_names <- dummy_df |> vars_between(test, state), " ! WARNING: 'from' variable '")

    expect_equal(length(var_names), 5)
})


test_that("Providing not existing to-variable throws a warning", {
    expect_message(var_names <- dummy_df |> vars_between(state, test), " ! WARNING: 'to' variable '")

    expect_equal(length(var_names), 7)
})


test_that("Providing more than one from or to variable throws a ntoe", {
    expect_message(var_names <- dummy_df |> vars_between(c(state, age), income),
                   " ~ NOTE: Only single variable names allowed for 'from' and 'to'. The respective\n")

    expect_equal(length(var_names), 5)
})
