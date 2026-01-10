###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(10))


test_that("Add extensions to limited variables", {
    new_names_df <- dummy_df |> add_extension(5, c("sum", "pct"))

    expect_true(all(c("state_sum", "sex_pct") %in% names(new_names_df)))
})


test_that("Add extensions to all remaining variables", {
    new_names_df <- dummy_df |> add_extension(5, c("sum", "pct"), reuse = "last")

    expect_true("weight_pct" %in% names(new_names_df))
})


test_that("Renaming multiple variables", {
    new_names_df <- dummy_df |> rename_multi("sex" = "var1", "age" = "var2")

    expect_true(all(c("var1", "var2") %in% names(new_names_df)))
    expect_true(!all(c("sex", "age") %in% names(new_names_df)))
})


test_that("Renaming based on first row in data frame", {
    test_df <- data.frame(
                  var1 = c("id", 1, 2, 3),
                  var2 = c(NA, "a", "b", "c"),
                  var3 = c("value", 1, 2, 3),
                  var4 = c("", "a", "b", "c"),
                  var5 = c(1, 2, 3, 4))

    result_df <- test_df |> first_row_as_names()

    expect_equal(names(result_df), c("id", "var2", "value", "var4", "var5"))
    expect_equal(nrow(test_df) - 1, nrow(result_df))
})

###############################################################################
# Abort checks
###############################################################################

test_that("Renaming aborts if symbol cannot be evaluated", {
    expect_message(new_names_df <- dummy_df |> rename_multi(sex = var1),
                   'X ERROR: Unknown object found. Provide variables in quotation marks, like: "old_var" = "new_var".')
})


test_that("Renaming aborts if old variable name not found in data frame", {
    expect_message(new_names_df <- dummy_df |> rename_multi("var1" = "var2"),
                   " X ERROR: The provided <old name> '")
})


test_that("Renaming aborts if new variable name is already data frame", {
    expect_message(new_names_df <- dummy_df |> rename_multi("sex" = "age"),
                   " X ERROR: The provided <new name> '")
})
