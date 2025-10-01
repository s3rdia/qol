###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- suppressMessages(dummy_data(1000))


test_that("Recode discrete values into groups with all values covered", {
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    recode_df <- suppressMessages(test_df |> recode("age_group", age = age.))

    expect_true(ncol(recode_df) == ncol(test_df) + 1)

    expect_true("age_group" %in% names(recode_df))

    expect_true(length(unique(recode_df[["age_group"]])) <= 6)
})


test_that("Recode interval values into groups with all values covered", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    test_df   <- test_df |> collapse::fsubset(!is.na(income))
    recode_df <- suppressMessages(test_df |> recode("income_group", income = income.))

    expect_true(ncol(recode_df) == ncol(test_df) + 1)

    expect_true("income_group" %in% names(recode_df))

    expect_true(length(unique(recode_df[["income_group"]])) <= 5)
})


test_that("Recode interval values with NA values", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expect_message(recode_df <- test_df |> recode("income_group", income = income.), " ! WARNING: Variable 'income' has NA values")

    expect_true("income_group" %in% names(recode_df))
})


test_that("Recode discrete values into groups with not all values covered", {
    age. <- suppressMessages(discrete_format(
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    test_df   <- test_df |> collapse::fsubset(!is.na(income))
    recode_df <- suppressMessages(test_df |> recode("age_group", age = age.))

    expect_true(ncol(recode_df) == ncol(test_df) + 1)

    expect_true("age_group" %in% names(recode_df))

    unique_age       <- length(unique(recode_df[["age"]]))
    unique_age_group <- length(unique(recode_df[["age_group"]]))
    expect_true(unique_age_group >= 4 & unique_age_group < unique_age)
})


test_that("Recode interval values into groups with not all values covered", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999))

    test_df   <- test_df |> collapse::fsubset(!is.na(income))
    recode_df <- suppressMessages(test_df |> recode("income_group", income = income.))

    expect_true(ncol(recode_df) == ncol(test_df) + 1)

    expect_true("income_group" %in% names(recode_df))

    unique_income       <- length(unique(recode_df[["income"]]))
    unique_income_group <- length(unique(recode_df[["income_group"]]))
    expect_true(unique_income_group >= 4 & unique_income_group < unique_income)
})


test_that("Providing none format data frames", {
    age1 <- c(1, 2, 3, 4)
    age2 <- c("a", "b", "c", "d")

    expect_message(recode_df1 <- test_df |> recode("age_group", age = age1), " X ERROR: The format for 'age' must be a data table")
    expect_message(recode_df2 <- test_df |> recode("age_group", age = age2), " X ERROR: The format for 'age' must be a data table")

    expect_identical(recode_df1, test_df)
    expect_identical(recode_df2, test_df)

    expect_true(!"age_group" %in% names(recode_df1))
    expect_true(!"age_group" %in% names(recode_df2))
})


test_that("Given variable is no in data frame", {
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    expect_message(recode_df <- test_df |> recode("age_group", dog = age.), " X ERROR: Variable 'dog' not found in the input data frame")

    expect_identical(recode_df, test_df)

    expect_true(!"age_group" %in% names(recode_df))
})


test_that("Entering none character as new variable name", {
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    recode_df <- suppressMessages(test_df |> recode(123, age = age.))

    expect_true(ncol(recode_df) == ncol(test_df) + 1)

    expect_true("123" %in% names(recode_df))
})


test_that("Recoding with multilabel gives a warning", {
    age. <- suppressMessages(discrete_format(
        "Total"          = 0:100,
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    expect_message(recode_df <- test_df |> recode("age_group", age = age.), " ! WARNING: The format for 'age' is a multilabel")
})


test_that("Recode won't overwrite existing variable", {
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    expect_message(recode_df <- test_df |> recode("age", age = age.), " X ERROR: Variable 'age' already exists")
})

###############################################################################
# Multi recode
###############################################################################

test_that("Recode a variable with a multilabel format (discrete)", {
    age. <- suppressMessages(discrete_format(
        "Total"          = 0:100,
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    recode_df <- test_df |> recode_multi(age = age.)

    expect_true(ncol(recode_df) == ncol(test_df))
    expect_true(nrow(recode_df) > nrow(test_df))
})


test_that("Recode a variable with a multilabel format (interval)", {
    income. <- suppressMessages(interval_format(
        "Total"              = 0:99999,
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    recode_df <- test_df |> recode_multi(income = income.)

    expect_true(ncol(recode_df) == ncol(test_df))
    expect_true(nrow(recode_df) > nrow(test_df))
})


test_that("Recode multiple variables at once", {
    age. <- suppressMessages(discrete_format(
        "Total"          = 0:100,
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    income. <- suppressMessages(interval_format(
        "Total"              = 0:99999,
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    recode_df <- test_df |> recode_multi(age = age., income = income.)

    expect_true(ncol(recode_df) == ncol(test_df))
    expect_true(nrow(recode_df) > nrow(test_df))
})

