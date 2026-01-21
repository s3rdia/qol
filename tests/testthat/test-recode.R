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

    test_df[["age_group"]] <- suppressMessages(test_df |> recode(age = age.))

    expect_true("age_group" %in% names(test_df))

    expect_true(length(unique(test_df[["age_group"]])) <= 6)
})


test_that("Recode interval values into groups with all values covered", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    test_df <- test_df |> collapse::fsubset(!is.na(income))
    test_df[["income_group"]] <- suppressMessages(test_df |> recode(income = income.))

    expect_true("income_group" %in% names(test_df))

    expect_true(length(unique(test_df[["income_group"]])) <= 5)
})


test_that("Recode interval values with NA values", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    expect_message(test_df[["income_group"]] <- test_df |> recode(income = income.), " X ERROR: Variable 'income' has NA values")
})


test_that("Recode discrete values into groups with not all values covered", {
    age. <- suppressMessages(discrete_format(
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    test_df <- test_df |> collapse::fsubset(!is.na(income))
    test_df[["age_group"]] <- suppressMessages(test_df |> recode(age = age.))

    expect_true("age_group" %in% names(test_df))

    unique_age       <- length(unique(test_df[["age"]]))
    unique_age_group <- length(unique(test_df[["age_group"]]))
    expect_true(unique_age_group >= 4 & unique_age_group < unique_age)
})


test_that("Recode interval values into groups with not all values covered", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999))

    test_df <- test_df |> collapse::fsubset(!is.na(income))
    test_df[["income_group"]] <- suppressMessages(test_df |> recode(income = income.))

    expect_true("income_group" %in% names(test_df))

    unique_income       <- length(unique(test_df[["income"]]))
    unique_income_group <- length(unique(test_df[["income_group"]]))
    expect_true(unique_income_group >= 4 & unique_income_group < unique_income)
})


test_that("Providing none format data frames", {
    age1 <- c(1, 2, 3, 4)
    age2 <- c("a", "b", "c", "d")

    expect_message(test_df[["age_group"]] <- test_df |> recode(age = age1), " X ERROR: The format for 'age' must be a data table")
    expect_message(test_df[["age_group"]] <- test_df |> recode(age = age2), " X ERROR: The format for 'age' must be a data table")

    expect_true(!"age_group" %in% names(test_df))
    expect_true(!"age_group" %in% names(test_df))
})


test_that("Given variable is not in data frame", {
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    expect_message(test_df[["age_group"]] <- test_df |> recode(dog = age.), " X ERROR: Variable 'dog' not found in the input data frame")

    expect_true(!"age_group" %in% names(test_df))
})


test_that("Recoding with multilabel gives a warning", {
    age. <- suppressMessages(discrete_format(
        "Total"          = 0:100,
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    expect_message(test_df[["age_group"]] <- test_df |> recode(age = age.), " ! WARNING: The format for 'age' is a multilabel")
})


test_that("Recode will overwrite existing variable", {
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

    test_df[["age"]] <- test_df |> recode(age = age.)

    expect_true(all(c("under 18",
                      "18 to under 25",
                      "25 to under 55",
                      "55 to under 65",
                      "65 and older") %in% test_df[["age"]]))
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

    expect_true(collapse::fncol(recode_df) == collapse::fncol(test_df))
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

    expect_true(collapse::fncol(recode_df) == collapse::fncol(test_df))
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

    expect_true(collapse::fncol(recode_df) == collapse::fncol(test_df))
    expect_true(nrow(recode_df) > nrow(test_df))
})
