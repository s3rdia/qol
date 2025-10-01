###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Different way of passing variables in", {
    split_list1 <- suppressMessages(dummy_df |> split_by_var(sex))
    split_list2 <- suppressMessages(dummy_df |> split_by_var("sex"))

    expect_identical(split_list1, split_list2)
})


test_that("Split data frame by variable", {
    split_list <- suppressMessages(dummy_df |> split_by_var(sex))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 3)

    expect_equal(nrow(split_list[[1]]) + nrow(split_list[[2]]) + nrow(split_list[[3]]),
                 nrow(dummy_df))

    expect_true(!2 %in% split_list[["1"]][["sex"]])
    expect_true(!1 %in% split_list[["2"]][["sex"]])
})


test_that("Split data frame by condition", {
    split_list <- suppressMessages(dummy_df |> split_by_condition(sex == 1))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 1)
    expect_true(!2 %in% split_list[["sex == 1"]][["sex"]])
})


test_that("Split data frame by multiple conditions", {
    split_list <- suppressMessages(dummy_df |>
           split_by_condition(sex == 1 & income > 2000, age < 18))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 2)
    expect_true(min(split_list[[1]][["income"]]) > 2000)
    expect_true(max(split_list[[2]][["age"]]) < 18)
})


test_that("Split with inverse group", {
    split_list <- suppressMessages(dummy_df |>
           split_by_condition(sex == 1, inverse = TRUE))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 2)
    expect_true(!2 %in% split_list[["sex == 1"]][["sex"]])
    expect_true(!1 %in% split_list[["inverse"]][["sex"]])
})
