###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


test_that("if. can convert values conditionally", {
    test_df <- dummy_df |>
             if.(age < 18,             age_group = "under 18") |>
        else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
        else.   (                      age_group = "65 and older")

    expect_true("age_group" %in% names(test_df))
    expect_true(all(c("under 18", "18 to under 65", "65 and older") %in% test_df[["age_group"]]))
})


test_that("else_if. doesn't work without if.", {
    test_df <- dummy_df |>
        else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
        else.   (                      age_group = "65 and older")

    expect_true(!"age_group" %in% names(test_df))
})


test_that("else. doesn't work without if.", {
    test_df <- dummy_df |>
        else.(age_group = "65 and older")

    expect_true(!"age_group" %in% names(test_df))
})


test_that("else_if. only alters NA values conditionally", {
    test_df1 <- dummy_df |>
             if.(age >= 18 & age < 65, age_group = "under 18") |>
        else_if.(age >= 18 & age < 65, age_group = "18 to under 65")

    test_df2 <- dummy_df |>
             if.(age < 0,              age_group = "under 18") |>
        else_if.(age >= 18 & age < 65, age_group = "18 to under 65")

    expect_true("age_group" %in% names(test_df1))
    expect_true("age_group" %in% names(test_df2))
    expect_true(!"18 to under 65" %in% test_df1[["age_group"]])
    expect_true(!"under 18" %in% test_df2[["age_group"]])
    expect_true(NA %in% test_df2[["age_group"]])
})


test_that("else. only alters every other NA value", {
    test_df1 <- dummy_df |>
        collapse::fsubset(!is.na(age)) |>
          if.(age < 101, age_group = "under 100") |>
        else.(           age_group = "100 and older")

    test_df2 <- dummy_df |>
        collapse::fsubset(!is.na(age)) |>
          if.(age < 0, age_group = "under 18") |>
        else.(         age_group = "18 and older")

    expect_true("age_group" %in% names(test_df1))
    expect_true("age_group" %in% names(test_df2))
    expect_true(!"100 and older" %in% test_df1[["age_group"]])
    expect_true(!NA %in% test_df2[["age_group"]])
})


test_that("if. and else_if. are not the same", {
    state_df <- dummy_df |>
             if.(state == 1, state_a = "State 1") |>
        else_if.(state < 11, state_a = "West") |>
        else.   (            state_a = "East")

    state_df <- dummy_df |>
          if.(state == 1, state_b = "State 1") |>
          if.(state < 11, state_b = "West") |>
        else.(            state_b = "East")

    expect_false(identical(state_df[["state_a"]], state_df[["state_b"]]))
})


test_that("Type conversion in if. block on type mismatch", {
    expect_message(test_df1 <- dummy_df |>
             if.(age < 18,             age_group = "under 18") |>
        else_if.(age >= 18 & age < 65, age_group = 1) |>
        else.   (                      age_group = "65 and older"), " ! WARNING: Type mismatch")

        expect_message(test_df2 <- dummy_df |>
             if.(age < 18,             age_group = 1) |>
        else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
        else.   (                      age_group = 2), " ! WARNING: Type mismatch")
})

###############################################################################
# if. for subsetting
###############################################################################

test_that("Subset data frame with if.", {
    test_df <- dummy_df |> if.(sex == 1)

    expect_true(nrow(test_df) < nrow(dummy_df))
    expect_true(!2 %in% test_df[["sex"]])
})


test_that("Subset data frame with if., when only providing single variable", {
    test_df <- dummy_df |> if.(sex)

    expect_true(nrow(test_df) < nrow(dummy_df))
    expect_true(!NA %in% test_df[["sex"]])
})


test_that("Subset data frame with if., when only providing single variable as character", {
    test_df <- dummy_df |> if.("sex")

    expect_true(nrow(test_df) < nrow(dummy_df))
    expect_true(!NA %in% test_df[["sex"]])
})


test_that("Abort subset with if., if variable is not part of the data frame", {
    expect_message(test_df <- dummy_df |> if.("test"),
                   " X ERROR: No variable for subsetting provided. Data frame remains as is.")

    expect_equal(test_df, dummy_df)
})


test_that("Abort subset with if., if multiple variables are provided", {
    expect_message(test_df <- dummy_df |> if.(c("age", "sex")),
                   " X ERROR: Only single variables and conditions allowed. Data frame remains as is.")

    expect_equal(test_df, dummy_df)
})
