set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- dummy_data(10)

###############################################################################
# Keep
###############################################################################

# Different way of passing variables in keep
keep_df1 <- test_df |> keep(year)
keep_df2 <- test_df |> keep("year")

expect_identical(keep_df1, keep_df2, info = "Different way of passing variables in keep")


# Keep only one variable
keep_df <- test_df |> keep(year)

expect_equal(ncol(keep_df), 1, info = "Keep only one variable")

expect_true("year" %in% names(keep_df), info = "Keep only one variable")

expect_identical(keep_df[["year"]], test_df[["year"]], info = "Keep only one variable")


# Keep more than one variable
keep_df <- test_df |> keep(year, age, sex)

expect_equal(ncol(keep_df), 3, info = "Keep more than one variable")

expect_true(all(c("year", "sex", "age") %in% names(keep_df)), info = "Keep more than one variable")

expect_identical(keep_df[["year"]], test_df[["year"]], info = "Keep more than one variable")
expect_identical(keep_df[["age"]], test_df[["age"]], info = "Keep more than one variable")
expect_identical(keep_df[["sex"]], test_df[["sex"]], info = "Keep more than one variable")


# Keep range of variables
keep_df <- test_df |> keep(age:education)

expect_equal(ncol(keep_df), 3, info = "Keep range of variables")

expect_true(all(c("age", "sex", "education") %in% names(keep_df)), info = "Keep range of variables")

expect_identical(keep_df[["education"]], test_df[["education"]], info = "Keep range of variables")
expect_identical(keep_df[["age"]], test_df[["age"]], info = "Keep range of variables")


# Keep variables starting with letter
keep_df <- test_df |> keep("s:")

expect_equal(ncol(keep_df), 2, info = "Keep variables starting with letter")

expect_true(all(c("state", "sex") %in% names(keep_df)), info = "Keep variables starting with letter")


# Keep variables ending with letter
keep_df <- test_df |> keep(":id")

expect_equal(ncol(keep_df), 2, info = "Keep variables ending with letter")

expect_true(all(c("household_id", "person_id") %in% names(keep_df)), info = "Keep variables ending with letter")


# Keep variables containing letter
keep_df <- test_df |> keep(":on:")

expect_equal(ncol(keep_df), 4, info = "Keep variables containing letter")

expect_true(all(c("person_id", "first_person", "education") %in% names(keep_df)), info = "Keep variables containing letter")


# Variables to keep contain a variable name that isn't part of the data frame
keep_df1 <- test_df |> keep(year, age, sex, cats, dogs)
expect_warning(print_stack_as_messages("WARNING"), "The provided variable to keep", info = "Variables to keep contain a variable name that isn't part of the data frame")

keep_df2 <- test_df |> keep(year, age, sex)

expect_equal(ncol(keep_df1), 3, info = "Variables to keep contain a variable name that isn't part of the data frame")
expect_equal(ncol(keep_df2), 3, info = "Variables to keep contain a variable name that isn't part of the data frame")

expect_identical(keep_df1, keep_df2, info = "Variables to keep contain a variable name that isn't part of the data frame")

expect_true(all(c("year", "sex", "age") %in% names(keep_df1)), info = "Variables to keep contain a variable name that isn't part of the data frame")
expect_true(!all(c("cats", "dogs") %in% names(keep_df1)), info = "Variables to keep contain a variable name that isn't part of the data frame")


# Keep only variables that are not part of the data frame
keep_df <- test_df |> keep(cats, dogs)

expect_warning(print_stack_as_messages("WARNING"), "The provided variable to keep", info = "Keep only variables that are not part of the data frame")

expect_true(nrow(keep_df) == 0, info = "Keep only variables that are not part of the data frame")
expect_true(ncol(keep_df) == 0, info = "Keep only variables that are not part of the data frame")

expect_true(!all(c("cats", "dogs") %in% names(keep_df)), info = "Keep only variables that are not part of the data frame")


# Keep without any variables provided
keep_df <- test_df |> keep()

expect_identical(keep_df, test_df, info = "Keep without any variables provided")


# Keep with sorted variables
unsorted <- test_df |> keep(weight, age)
sorted   <- test_df |> keep(weight, age, order_vars = TRUE)

expect_equal(names(unsorted)[1], "age", info = "Keep with sorted variables")
expect_equal(names(sorted)[1], "weight", info = "Keep with sorted variables")

###############################################################################
# Drop
###############################################################################

# Different way of passing variables in drop
drop_df1 <- test_df |> dropp(year)
drop_df2 <- test_df |> dropp("year")

expect_identical(drop_df1, drop_df2, info = "Different way of passing variables in drop")


# Drop only one variable
drop_df <- test_df |> dropp(year)

expect_equal(ncol(drop_df), ncol(test_df) - 1, info = "Drop only one variable")

expect_true(!"year" %in% names(drop_df), info = "Drop only one variable")


# Drop more than one variable
drop_df <- test_df |> dropp(year, age, sex)

expect_equal(ncol(drop_df), ncol(test_df) - 3, info = "Drop more than one variable")

expect_true(!all(c("year", "sex", "age") %in% names(drop_df)), info = "Drop more than one variable")


# Drop range of variables
drop_df <- test_df |> dropp(age:education)

expect_equal(ncol(drop_df), ncol(test_df) - 3, info = "Drop range of variables")

expect_true(!all(c("education", "sex", "age") %in% names(drop_df)), info = "Drop range of variables")


# Drop variables starting with letter
drop_df <- test_df |> dropp("s:")

expect_true(!all(c("state", "sex") %in% names(drop_df)), info = "Drop variables starting with letter")


# Drop variables ending with letter
drop_df <- test_df |> dropp(":id")

expect_true(!all(c("household_id", "person_id") %in% names(drop_df)), info = "Drop variables ending with letter")


# Drop variables containing letter
drop_df <- test_df |> dropp(":on:")

expect_true(!all(c("person_id", "first_person", "education") %in% names(drop_df)), info = "Drop variables containing letter")


# Variables to drop contain a variable name that isn't part of the data frame
drop_df1 <- test_df |> dropp(year, age, sex, cats, dogs)

expect_warning(print_stack_as_messages("WARNING"), "The provided variable to drop", info = "Variables to drop contain a variable name that isn't part of the data frame")
drop_df2 <- test_df |> dropp(year, age, sex)

expect_equal(ncol(drop_df1), ncol(test_df) - 3, info = "Variables to drop contain a variable name that isn't part of the data frame")
expect_equal(ncol(drop_df2), ncol(test_df) - 3, info = "Variables to drop contain a variable name that isn't part of the data frame")

expect_identical(drop_df1, drop_df2, info = "Variables to drop contain a variable name that isn't part of the data frame")

expect_true(!all(c("year", "sex", "age") %in% names(drop_df1)), info = "Variables to drop contain a variable name that isn't part of the data frame")
expect_true(!all(c("cats", "dogs") %in% names(test_df)), info = "Variables to drop contain a variable name that isn't part of the data frame")


# Drop only variables that are not part of the data frame
drop_df <- test_df |> dropp(cats, dogs)

expect_warning(print_stack_as_messages("WARNING"), "The provided variable to drop", info = "Drop only variables that are not part of the data frame")

expect_identical(drop_df, test_df, info = "Drop only variables that are not part of the data frame")


# Drop without any variables provided
drop_df <- test_df |> dropp()

expect_identical(drop_df, test_df, info = "Drop without any variables provided")


set_no_print()
