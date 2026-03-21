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


test_that("if. can handle vectors", {
    test_df <- dummy_df |>
             if.(age < 18,             income_new = income) |>
        else_if.(age >= 18 & age < 65, income_new = income) |>
        else.   (                      income_new = income)

    expect_true("income_new" %in% names(test_df))
    expect_equal(test_df[["income_new"]], test_df[["income"]])
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


test_that("if. can check for variable expressions starting with letter", {
    letter_df <- dummy_df |> if.(education == "m:", edu = 1)
    test_df   <- letter_df |> if.(edu)

    expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)))
    expect_true(collapse::funique(test_df[["education"]]) == "middle")
})


test_that("if. can check for variable expressions ending with letter", {
    letter_df <- dummy_df |> if.(education == ":w", edu = 1)
    test_df   <- letter_df |> if.(edu)

    expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)))
    expect_true(collapse::funique(test_df[["education"]]) == "low")
})


test_that("if. can check for variable expressions containing a letter", {
    letter_df <- dummy_df |> if.(education == ":g:", edu = 1)
    test_df   <- letter_df |> if.(edu)

    expect_true(all(collapse::funique(letter_df[["edu"]]) %in% c(1, NA)))
    expect_true(collapse::funique(test_df[["education"]]) == "high")
})


test_that("if. as do over loop", {
    vars1  <- c("income", "balance")
    vars2  <- c("VAR1", "VAR2")
    values <- c(1, 2)

    do_over_df <- dummy_df |> if.(vars1 > 0, vars2 = values)

    expect_true(all(c("VAR1", "VAR2") %in% names(do_over_df)))
    expect_true(all(collapse::funique(do_over_df[["VAR1"]]) %in% c(1, NA)))
    expect_true(all(collapse::funique(do_over_df[["VAR2"]]) %in% c(2, NA)))

    do_over_df <- do_over_df |> else_if.(vars1 > 500, vars2 = 3)

    expect_true(all(collapse::funique(do_over_df[["VAR1"]]) %in% c(1, 3, NA)))
    expect_true(all(collapse::funique(do_over_df[["VAR2"]]) %in% c(2, 3, NA)))

    do_over_df <- do_over_df |> else.(vars2 = 4)

    expect_true(all(collapse::funique(do_over_df[["VAR1"]]) %in% c(1, 3, 4)))
    expect_true(all(collapse::funique(do_over_df[["VAR2"]]) %in% c(2, 3, 4)))
})


test_that("do_if blocks", {
    test_df <- dummy_df |>
        do_if(sex == 1) |>
                 if.(age < 18,             age_group = 1) |>
            else_if.(age >= 18 & age < 65, age_group = 2) |>
            else.   (                      age_group = 3) |>
        else_do() |>
                 if.(age < 18,             age_group = 4) |>
            else_if.(age >= 18 & age < 65, age_group = 5) |>
            else.   (                      age_group = 6) |>
        end_do()

      male_df <- test_df |> collapse::fsubset(sex == 1)
    female_df <- test_df |> collapse::fsubset(sex == 2)

    expect_true(all(collapse::funique(male_df[["age_group"]]) %in% c(1, 2, 3)))
    expect_true(all(collapse::funique(female_df[["age_group"]]) %in% c(4, 5, 6)))
})


test_that("do_if blocks with end_all_do", {
    test_df <- dummy_df |>
        do_if(sex == 1) |>
        end_all_do()

    expect_equal(test_df, dummy_df)
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

###############################################################################
# Where.
###############################################################################

test_that("Filter data frame with where.", {
    test_df <- dummy_df |> where.(sex == 1, c("sex", "age"))


    expect_equal(names(test_df), c("sex", "age"))
    expect_true(!any(c(2, NA) %in% test_df[["sex"]]))
})

###############################################################################
# Warning checks
###############################################################################

test_that("if. as do over loop throws a warning when trying to make more than one variable assignment", {
    vars1  <- c("income", "balance")
    vars2  <- c("VAR1", "VAR2")
    values <- c(1, 2)

    expect_message(do_over_df <- dummy_df |> if.(vars1 > 0, vars2 = values, test = 1),
                   " ! WARNING: When using vectors in conditions or variable assignments, only one")
})


test_that("else_do throws a warning when there is no active filter", {
    expect_message(test_df <- dummy_df |> else_do(),
                   " ! WARNING: No active filter variable found.")
})


test_that("end_do throws a warning when there is no active filter", {
    expect_message(test_df <- dummy_df |> end_do(),
                   " ! WARNING: No active filter variable found.")
})


test_that("end_all_do throws a warning when there is no active filter", {
    expect_message(test_df <- dummy_df |> end_all_do(),
                   " ! WARNING: No active filter variable found.")
})

###############################################################################
# Abort checks
###############################################################################

test_that("if. as do over loop aborts on vectors of unequal lengths", {
    vars1  <- c("income", "balance")
    vars2  <- c("VAR1", "VAR2", "VAR3")
    values <- c(1, 2)

    expect_message(do_over_df <- dummy_df |> if.(vars1 > 0, vars2 = values),
                   " X ERROR: Passed vectors are of unequal lengths.")

    expect_message(do_over_df <- dummy_df |> else_if.(vars1 > 0, vars2 = values),
                   " X ERROR: Passed vectors are of unequal lengths.")

    expect_message(do_over_df <- dummy_df |> else.(vars1 = values, vars2 = values),
                   " X ERROR: Passed vectors are of unequal lengths.")
})


test_that("if. as do over loop aborts without variable assignment", {
    vars1  <- c("income", "balance")
    vars2  <- c("VAR1", "VAR2")
    values <- c(1, 2)

    expect_message(do_over_df <- dummy_df |> if.(vars1 > 0),
                   " X ERROR: When using vectors in conditions, there must be a variable assignment.")

    expect_message(do_over_df <- dummy_df |> else_if.(vars1 > 0),
                   " X ERROR: When using vectors in conditions, there must be a variable assignment.")
})
