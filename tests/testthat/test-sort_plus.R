###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

small_df <- data.frame(var1 = c(3, 2, 1, 2),
                       var2 = c("c", "a", "a", "b"))


test_that("Simple numeric sort", {
    sort_df <- suppressMessages(small_df |> sort_plus(by = var1))

    expect_equal(sort_df[["var1"]], c(1, 2, 2, 3))
})


test_that("Simple character sort", {
    sort_df <- suppressMessages(small_df |> sort_plus(by = var2))

    expect_equal(sort_df[["var2"]], c("a", "a", "b", "c"))
})


test_that("Simple numeric sort, descending", {
    sort_df <- suppressMessages(small_df |> sort_plus(by = var1, order = "descending"))

    expect_equal(sort_df[["var1"]], c(3, 2, 2, 1))
})


test_that("Sort with preserve", {
    sort_df <- suppressMessages(small_df |> sort_plus(by = var1, preserve = var2))

    expect_equal(sort_df[["var1"]], c(3, 1, 2, 2))
    expect_equal(sort_df[["var2"]], c("c", "a", "a", "b"))
})


test_that("Sort with format", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    sort_df <- suppressMessages(small_df |>
        sort_plus(by = var2,
                  formats = list(var2 = suppressMessages(
                      discrete_format("1" = "a",
                                      "2" = "c",
                                      "3" = "b")))))

    expect_equal(sort_df[["var2"]], c("a", "a", "c", "b"))
})

###############################################################################
# Warning checks
###############################################################################

test_that("Sort warning with invalid by variable", {
    expect_message(sort_df <- small_df |> sort_plus(by = var3), " ! WARNING: The provided <by> variable")
})


test_that("Sort warning when by variable is also part of preserve, sort normal", {
    expect_message(sort_df <- small_df |> sort_plus(by = var1, preserve = var1), " ! WARNING: The provided <by> variable")
})


test_that("Sort warning when wrong order is specified, sorting in ascending order instead", {
    expect_message(sort_df <- small_df |> sort_plus(by = var1, order = "test"), " ! WARNING: <Order> other than 'ascending'/'a' or 'descending'/'d' specified, which is")

    expect_equal(sort_df[["var1"]], c(1, 2, 2, 3))
})


test_that("Sort warning when trying to apply a multilabel, sort normal", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(sort_df <- small_df |>
                       sort_plus(by = var1,
                                 formats = list(age = suppressMessages(
                                                discrete_format("Total"          = 1:3,
                                                                "Number 1 and 2" = 1:2,
                                                                "Number 3"       = 3)))),
                   " ! WARNING: Applying a multilabel to variable")

    expect_equal(sort_df[["var1"]], c(1, 2, 2, 3))
})


test_that("Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(sort_df <- small_df |>
                       sort_plus(by = var1,
                                 formats = list(var3 = suppressMessages(
                                     discrete_format("under 18"     = 0:17,
                                                     "18 and older" = 18:100)))),
                   " ! WARNING: The variable 'var3' is not part of the data frame.")

    expect_equal(sort_df[["var1"]], c(1, 2, 2, 3))
})


test_that("Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal", {
    # NOTE: The user doesn't pass the formats in like this but it's the only way
    #       to test the functionality because test_that otherwise has no access
    #       to the formats if declared outside the function to test.
    expect_message(sort_df <- small_df |>
                       sort_plus(by = var1,
                                 formats = list(var2 = suppressMessages(
                                     discrete_format("under 18"     = 0:17,
                                                     "18 and older" = 18:100)))),
                   " ~ NOTE: Format for variable")

    expect_equal(sort_df[["var1"]], c(1, 2, 2, 3))
})


###############################################################################
# Abort checks
###############################################################################

test_that("Sort aborts without by variables", {
    expect_message(sort_df <- small_df |> sort_plus(), " X ERROR: No <by> variables provided. Sorting will be aborted.")
})
