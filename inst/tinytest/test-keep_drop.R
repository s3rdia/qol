set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- dummy_data(10)

alt_df <- as.data.frame(matrix(0, nrow = 3, ncol = 20,
                               dimnames = list(NULL, as.vector(rbind(paste0("age", 1:10),
                                                                     paste0("sex", 1:10))))))

hyphen_df <- alt_df
hyphen_df[["jan-2026"]] <- 1
hyphen_df[["dec-2026"]] <- 2

vector_df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, e = 13:15)

vector_names <- c("a", "b")
other_names  <- c("c", "d")

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


# Keep a pattern based range, which selects by name and not by position
keep_alt <- alt_df |> keep(age1-age10)

expect_identical(names(keep_alt), paste0("age", 1:10), info = "Keep a pattern based range, which selects by name and not by position")
expect_identical(alt_df |> keep("age1-age10"), keep_alt, info = "Keep a pattern based range, which selects by name and not by position")

# The provided variable order is kept
keep_order <- alt_df |> keep(age1-age2, sex9-sex10, order_vars = TRUE)

expect_identical(names(keep_order), c("age1", "age2", "sex9", "sex10"), info = "The provided variable order is kept")


# Keep variables with hyphen
expect_identical(names(hyphen_df |> keep("jan-2026")), "jan-2026", info = "Keep variables with hyphen")
expect_identical(names(hyphen_df |> keep(jan-2026)), "jan-2026", info = "Keep variables with hyphen")


# Keep range of hyphenated variables
expect_identical(names(hyphen_df |> keep("jan-2026:dec-2026")), c("jan-2026", "dec-2026"), info = "Keep range of hyphenated variables")

# Keep ignores ranges with two invalid variables
range_keep <- test_df |> keep(AB0302P:TL0102L)

expect_warning(print_stack_as_messages("WARNING"), "No variables found with the given pattern", info = "Keep ignores ranges with two invalid variables")
expect_identical(names(range_keep), names(test_df), info = "Keep ignores ranges with two invalid variables")


# Keep accepts vectors of variable names
expect_identical(names(vector_df |> keep(vector_names)), c("a", "b"), info = "Keep accepts vectors of variable names")
expect_identical(names(vector_df |> keep(vector_names, other_names)), c("a", "b", "c", "d"),
                 info = "Keep accepts vectors of variable names")


# Keep can mix single variable names and vectors
expect_identical(names(vector_df |> keep(vector_names, e)), c("a", "b", "e"),
                 info = "Keep can mix single variable names and vectors")
expect_identical(names(vector_df |> keep(e, vector_names)), c("a", "b", "e"),
                 info = "Keep can mix single variable names and vectors")
expect_identical(names(vector_df |> keep(vector_names, c, other_names)), c("a", "b", "c", "d"),
                 info = "Keep can mix single variable names and vectors")


# Keep does not fail when the same variable is provided multiple times
expect_identical(names(vector_df |> keep(vector_names, a)), c("a", "b"),
                 info = "Keep does not fail when the same variable is provided multiple times")
expect_identical(names(vector_df |> keep(vector_names, "a")), c("a", "b"),
                 info = "Keep does not fail when the same variable is provided multiple times")
expect_identical(names(vector_df |> keep(vector_names, a:c)), c("a", "b", "c"),
                 info = "Keep does not fail when the same variable is provided multiple times")

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


# Drop a pattern based range, which selects by name and not by position
drop_alt <- alt_df |> dropp(age1-age10)

expect_identical(names(drop_alt), paste0("sex", 1:10), info = "Drop a pattern based range, which selects by name and not by position")


# Drop variables with hyphen
expect_true(!"jan-2026" %in% names(hyphen_df |> dropp("jan-2026")), info = "Drop variables with hyphen")
expect_true(!"jan-2026" %in% names(hyphen_df |> dropp(jan-2026)), info = "Drop variables with hyphen")


# Drop range of hyphenated variables
expect_true(!all(c("jan-2026", "dec-2026") %in% names(hyphen_df |> dropp("jan-2026:dec-2026"))), info = "Drop range of hyphenated variables")


# Drop ignores ranges with two invalid variables
range_drop <- test_df |> dropp(AB0302P:TL0102L)

expect_warning(print_stack_as_messages("WARNING"), "No variables found with the given pattern", info = "Drop ignores ranges with two invalid variables")
expect_identical(names(range_drop), names(test_df), info = "Drop ignores ranges with two invalid variables")


# Drop accepts vectors of variable names
expect_identical(names(vector_df |> dropp(vector_names)), c("c", "d", "e"),
                 info = "Drop accepts vectors of variable names")
expect_identical(names(vector_df |> dropp(vector_names, other_names)), "e",
                 info = "Drop accepts vectors of variable names")


# Drop can mix single variable names and vectors
expect_identical(names(vector_df |> dropp(vector_names, e)), c("c", "d"),
                 info = "Drop can mix single variable names and vectors")
expect_identical(names(vector_df |> dropp(e, vector_names)), c("c", "d"),
                 info = "Drop can mix single variable names and vectors")
expect_identical(names(vector_df |> dropp(vector_names, c, other_names)), "e",
                 info = "Drop can mix single variable names and vectors")


# Drop does not fail when the same variable is provided multiple times
expect_identical(names(vector_df |> dropp(vector_names, a)), c("c", "d", "e"),
                 info = "Drop does not fail when the same variable is provided multiple times")
expect_identical(names(vector_df |> dropp(vector_names, "a")), c("c", "d", "e"),
                 info = "Drop does not fail when the same variable is provided multiple times")
expect_identical(names(vector_df |> dropp(vector_names, a:c)), c("d", "e"),
                 info = "Drop does not fail when the same variable is provided multiple times")


set_no_print()
