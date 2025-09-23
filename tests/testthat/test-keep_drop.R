###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- suppressMessages(dummy_data(10))

###############################################################################
# Keep
###############################################################################

test_that("Different way of passing variables in keep", {
    keep_df1 <- test_df |> keep(year)
    keep_df2 <- test_df |> keep("year")

    expect_identical(keep_df1, keep_df2)
})


test_that("Keep only one variable", {
    keep_df <- test_df |> keep(year)

    expect_equal(ncol(keep_df), 1)

    expect_true("year" %in% names(keep_df))

    expect_identical(keep_df[["year"]], test_df[["year"]])
})


test_that("Keep more than one variable", {
    keep_df <- test_df |> keep(year, age, sex)

    expect_equal(ncol(keep_df), 3)

    expect_true(all(c("year", "sex", "age") %in% names(keep_df)))

    expect_identical(keep_df[["year"]], test_df[["year"]])
    expect_identical(keep_df[["age"]], test_df[["age"]])
    expect_identical(keep_df[["sex"]], test_df[["sex"]])
})


test_that("Variables to keep contain a variable name that isn't part of the data frame", {
    expect_message(keep_df1 <- test_df |> keep(year, age, sex, cats, dogs), " ! WARNING: The provided variable to keep")
    keep_df2 <- test_df |> keep(year, age, sex)

    expect_equal(ncol(keep_df1), 3)
    expect_equal(ncol(keep_df2), 3)

    expect_identical(keep_df1, keep_df2)

    expect_true(all(c("year", "sex", "age") %in% names(keep_df1)))
    expect_true(!all(c("cats", "dogs") %in% names(keep_df1)))
})


test_that("Keep only variables that are not part of the data frame", {
    expect_message(keep_df <- test_df |> keep(cats, dogs), " ! WARNING: The provided variable to keep")

    expect_true(nrow(keep_df) == 0)
    expect_true(ncol(keep_df) == 0)

    expect_true(!all(c("cats", "dogs") %in% names(keep_df)))
})


test_that("Keep without any variables provided", {
    keep_df <- test_df |> keep()

    expect_identical(keep_df, test_df)
})


test_that("Keep with sorted variables", {
    keep_df <- test_df |> keep(weight, income, age, order_vars = TRUE)

    expect_equal(names(keep_df)[1], "weight")
    expect_equal(names(keep_df)[2], "income")
    expect_equal(names(keep_df)[3], "age")
})

###############################################################################
# Drop
###############################################################################

test_that("Different way of passing variables in drop", {
    drop_df1 <- test_df |> dropp(year)
    drop_df2 <- test_df |> dropp("year")

    expect_identical(drop_df1, drop_df2)
})


test_that("Drop only one variable", {
    drop_df <- test_df |> dropp(year)

    expect_equal(ncol(drop_df), ncol(test_df) - 1)

    expect_true(!"year" %in% names(drop_df))
})


test_that("Drop more than one variable", {
    drop_df <- test_df |> dropp(year, age, sex)

    expect_equal(ncol(drop_df), ncol(test_df) - 3)

    expect_true(!all(c("year", "sex", "age") %in% names(drop_df)))
})


test_that("Variables to drop contain a variable name that isn't part of the data frame", {
    expect_message(drop_df1 <- test_df |> dropp(year, age, sex, cats, dogs), " ! WARNING: The provided variable to drop")
    drop_df2 <- test_df |> dropp(year, age, sex)

    expect_equal(ncol(drop_df1), ncol(test_df) - 3)
    expect_equal(ncol(drop_df2), ncol(test_df) - 3)

    expect_identical(drop_df1, drop_df2)

    expect_true(!all(c("year", "sex", "age") %in% names(drop_df1)))
    expect_true(!all(c("cats", "dogs") %in% names(test_df)))
})


test_that("Drop only variables that are not part of the data frame", {
    expect_message(drop_df <- test_df |> dropp(cats, dogs), " ! WARNING: The provided variable to drop")

    expect_identical(drop_df, test_df)
})


test_that("Drop without any variables provided", {
    drop_df <- test_df |> dropp()

    expect_identical(drop_df, test_df)
})
