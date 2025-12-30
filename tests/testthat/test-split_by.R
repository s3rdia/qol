###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("Different way of passing variables in", {
    split_list1 <- suppressMessages(dummy_df |> split_by(sex))
    split_list2 <- suppressMessages(dummy_df |> split_by("sex"))

    expect_identical(split_list1, split_list2)
})


test_that("Split data frame by variable", {
    split_list <- suppressMessages(dummy_df |> split_by(sex))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 2)

    expect_true(!2 %in% split_list[["1"]][["sex"]])
    expect_true(!1 %in% split_list[["2"]][["sex"]])
})


test_that("Split data frame by condition", {
    split_list <- suppressMessages(dummy_df |> split_by(sex == 1))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 1)
    expect_true(!2 %in% split_list[["sex == 1"]][["sex"]])
})


test_that("Split data frame by multiple conditions", {
    split_list <- suppressMessages(dummy_df |>
           split_by(sex == 1 & income > 2000,
                    age < 18))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 2)
    expect_true(min(split_list[[1]][["income"]]) > 2000)
    expect_true(max(split_list[[2]][["age"]]) < 18)
})


test_that("Split with inverse group", {
    split_list <- suppressMessages(dummy_df |>
           split_by(sex == 1, inverse = TRUE))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 1)
    expect_true(!1 %in% split_list[["not (sex == 1)"]][["sex"]])
})


test_that("Split data frame by variable and condition", {
    split_list <- suppressMessages(dummy_df |>
           split_by(sex,
                    age < 18))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 3)

    expect_true(!2 %in% split_list[["1"]][["sex"]])
    expect_true(!1 %in% split_list[["2"]][["sex"]])
    expect_true(max(split_list[[3]][["age"]]) < 18)
})


test_that("Split data frame by variable using formats", {
    split_list <- suppressMessages(dummy_df |>
           split_by(sex,
                    formats = list(sex = suppressMessages(discrete_format(
                        "Total"  = 1:2,
                        "Male"   = 1,
                        "Female" = 2)))))

    expect_type(split_list, "list")
    expect_equal(length(split_list), 3)

    expect_equal(names(split_list), c("Total", "Male", "Female"))
})


###############################################################################
# Abort checks
###############################################################################


test_that("Split data frame aborts if variable is not part of the data frame", {
    expect_message(split_list <- dummy_df |>
           split_by(test), " X ERROR: Variable 'test' not found in the input data frame. Splitting will be aborted.")
})


test_that("Split data frame aborts if duplicate list entry names are produced", {
    expect_message(split_list <- dummy_df |>
          split_by(state, sex), " X ERROR: Variable 'sex' caused duplicate list entry names.")
})


test_that("Split data frame aborts if no valid split element is passed", {
    expect_message(split_list <- dummy_df |>
          split_by(1), " X ERROR: Only single variables or conditions allowed. Splitting will be aborted.")
})
