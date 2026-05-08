set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- dummy_data(1000)


# Recode discrete values into groups with all values covered
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

test_df2 <- test_df
test_df2[["age_group"]] <- test_df2 |> recode.(age = age.)

expect_true("age_group" %in% names(test_df2), info = "Recode discrete values into groups with all values covered")

expect_true(length(unique(test_df2[["age_group"]])) <= 6, info = "Recode discrete values into groups with all values covered")


# Recode interval values into groups with all values covered
income. <- interval_format(
    "below 500"          = 0:500,
    "500 to under 1000"  = 500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

test_df2 <- test_df
test_df2 <- test_df2 |> collapse::fsubset(!is.na(income))
test_df2[["income_group"]] <- test_df2 |> recode.(income = income.)

expect_true("income_group" %in% names(test_df2), info = "Recode interval values into groups with all values covered")

expect_true(length(unique(test_df2[["income_group"]])) <= 5, info = "Recode interval values into groups with all values covered")


# Recode interval values with NA values
income. <- interval_format(
    "00. no income"          = 0,
    "01.         under  500" = c(0.000001, 500),
    "02.  500 to under 1000" =  500:1000,
    "03. 1000 to under 1500" = 1000:1500,
    "04. 1500 to under 2000" = 1500:2000,
    "05. 2000 to under 2500" = 2000:2500,
    "06. 2500 to under 3000" = 2500:3000,
    "07. 3000 to under 3500" = 3000:3500,
    "08. 3500 to under 4000" = 3500:4000,
    "09. 4000 to under 4500" = 4000:4500,
    "10. 4500 to under 5000" = 4500:5000,
    "11. 5000 and more"      = c(5000, "high"),
    "99. Total"              = c("low", "high"))

test_df2 <- test_df
test_df2[["income_group"]] <- test_df2 |> recode.(income = income.)

expect_equal(test_df2[["income_class"]], test_df2[["income_group"]],
             info = "Recode interval values into groups with all values covered")
expect_warning(print_stack_as_messages("WARNING"), "The format for 'income' is a multilabel. A multilabel can't be fully applied in recode.",
               info = "Recode interval values with NA values")


# Recode discrete values into groups with not all values covered
age. <- discrete_format(
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

test_df2 <- test_df
test_df2 <- test_df2 |> collapse::fsubset(!is.na(income))
test_df2[["age_group"]] <- test_df2 |> recode.(age = age.)

expect_true("age_group" %in% names(test_df2), info = "Recode discrete values into groups with not all values covered")

unique_age       <- length(unique(test_df2[["age"]]))
unique_age_group <- length(unique(test_df2[["age_group"]]))
expect_true(unique_age_group >= 4 & unique_age_group < unique_age, info = "Recode discrete values into groups with not all values covered")


# Recode interval values into groups with not all values covered
income. <- interval_format(
    "below 500"          = 0:500,
    "500 to under 1000"  = 500:1000,
    "1000 to under 2000" = 1000:2000)

test_df2 <- test_df
test_df2 <- test_df2 |> collapse::fsubset(!is.na(income))
test_df2[["income_group"]] <- test_df2 |> recode.(income = income.)

expect_true("income_group" %in% names(test_df2), info = "Recode interval values into groups with not all values covered")

unique_income       <- length(unique(test_df2[["income"]]))
unique_income_group <- length(unique(test_df2[["income_group"]]))
expect_true(unique_income_group >= 4 & unique_income_group < unique_income, info = "Recode interval values into groups with not all values covered")


# Providing none format data frames
age1 <- c(1, 2, 3, 4)
age2 <- c("a", "b", "c", "d")

test_df2 <- test_df
test_df2[["age_group"]] <- test_df2 |> recode.(age = age1)

expect_error(print_stack_as_messages("ERROR"), "The format for 'age' must be a data table", info = "Providing none format data frames")
test_df2[["age_group"]] <- test_df2 |> recode.(age = age2)

expect_error(print_stack_as_messages("ERROR"), "The format for 'age' must be a data table", info = "Providing none format data frames")

expect_true(!"age_group" %in% names(test_df2), info = "Providing none format data frames")
expect_true(!"age_group" %in% names(test_df2), info = "Providing none format data frames")


# Given variable is not in data frame
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

test_df2 <- test_df
test_df2[["age_group"]] <- test_df2 |> recode.(dog = age.)

expect_error(print_stack_as_messages("ERROR"), "Variable 'dog' not found in the input data frame", info = "Given variable is not in data frame")

expect_true(!"age_group" %in% names(test_df2), info = "Given variable is not in data frame")


# Recoding with multilabel gives a warning
age. <- discrete_format(
    "Total"          = 0:100,
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

test_df2 <- test_df
test_df2[["age_group"]] <- test_df2 |> recode.(age = age.)

expect_warning(print_stack_as_messages("WARNING"), "The format for 'age' is a multilabel", info = "Recoding with multilabel gives a warning")


# Recode will overwrite existing variable
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

test_df2 <- test_df
test_df2[["age"]] <- test_df2 |> recode.(age = age.)

expect_true(all(c("under 18",
                  "18 to under 25",
                  "25 to under 55",
                  "55 to under 65",
                  "65 and older") %in% test_df2[["age"]]), info = "Recode will overwrite existing variable")

###############################################################################
# Multi recode
###############################################################################

# Recode a variable with a multilabel format (discrete)
age. <- discrete_format(
    "Total"          = 0:100,
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

recode_df <- test_df |> recode_multi(age = age.)

expect_true(collapse::fncol(recode_df) == collapse::fncol(test_df), info = "Recode a variable with a multilabel format (discrete)")
expect_true(collapse::fnrow(recode_df) > collapse::fnrow(test_df),  info = "Recode a variable with a multilabel format (discrete)")


# Recode a variable with a multilabel format (interval)
income. <- interval_format(
    "Total"              = 0:100000,
    "below 500"          = 0:500,
    "500 to under 1000"  = 500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

recode_df <- test_df |> recode_multi(income = income.)

expect_true(collapse::fncol(recode_df) == collapse::fncol(test_df), info = "Recode a variable with a multilabel format (interval)")
expect_true(collapse::fnrow(recode_df) > collapse::fnrow(test_df),  info = "Recode a variable with a multilabel format (interval)")


# Recode multiple variables at once
age. <- discrete_format(
    "Total"          = 0:100,
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

income. <- interval_format(
    "Total"              = 0:100000,
    "below 500"          = 0:500,
    "500 to under 1000"  = 500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

recode_df <- test_df |> recode_multi(age = age., income = income.)

expect_true(collapse::fncol(recode_df) == collapse::fncol(test_df), info = "Recode multiple variables at once")
expect_true(collapse::fnrow(recode_df) > collapse::fnrow(test_df),  info = "Recode multiple variables at once")


set_no_print()
