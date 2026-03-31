###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

small_df <- data.frame(var1 = c(3, 2, 1, 2),
                       var2 = c("c", "a", "a", "b"))


# Simple numeric sort
sort_df <- suppressMessages(small_df |> sort_plus(by = var1))

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Simple numeric sort")


# Simple character sort
sort_df <- suppressMessages(small_df |> sort_plus(by = var2))

expect_equal(sort_df[["var2"]], c("a", "a", "b", "c"), info = "Simple character sort")


# Simple numeric sort, descending
sort_df <- suppressMessages(small_df |> sort_plus(by = var1, order = "descending"))

expect_equal(sort_df[["var1"]], c(3, 2, 2, 1), info = "Simple numeric sort, descending")


# Sort with preserve
sort_df <- suppressMessages(small_df |> sort_plus(by = var1, preserve = var2))

expect_equal(sort_df[["var1"]], c(3, 1, 2, 2), info = "Sort with preserve")
expect_equal(sort_df[["var2"]], c("c", "a", "a", "b"), info = "Sort with preserve")


# Sort with format
sort_df <- suppressMessages(small_df |>
    sort_plus(by = var2,
              formats = list(var2 = suppressMessages(
                  discrete_format("1" = "a",
                                  "2" = "c",
                                  "3" = "b")))))

expect_equal(sort_df[["var2"]], c("a", "a", "c", "b"), info = "Sort with format")

###############################################################################
# Warning checks
###############################################################################

# Sort warning with invalid by variable
expect_message(sort_df <- small_df |> sort_plus(by = var3), " ! WARNING: The provided <by> variable", info = "Sort warning with invalid by variable")


# Sort warning when by variable is also part of preserve, sort normal
expect_message(sort_df <- small_df |> sort_plus(by = var1, preserve = var1), " ! WARNING: The provided <by> variable", info = "Sort warning when by variable is also part of preserve, sort normal")


# Sort warning when wrong order is specified, sorting in ascending order instead
expect_message(sort_df <- small_df |> sort_plus(by = var1, order = "test"), " ! WARNING: <Order> other than 'ascending'/'a' or 'descending'/'d' specified, which is", info = "Sort warning when wrong order is specified, sorting in ascending order instead")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when wrong order is specified, sorting in ascending order instead")


# Sort warning when trying to apply a multilabel, sort normal
expect_message(sort_df <- small_df |>
                   sort_plus(by = var1,
                             formats = list(age = suppressMessages(
                                            discrete_format("Total"          = 1:3,
                                                            "Number 1 and 2" = 1:2,
                                                            "Number 3"       = 3)))),
               " ! WARNING: Applying a multilabel to variable", info = "Sort warning when trying to apply a multilabel, sort normal")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when trying to apply a multilabel, sort normal")


# Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal
expect_message(sort_df <- small_df |>
                   sort_plus(by = var1,
                             formats = list(var3 = suppressMessages(
                                 discrete_format("under 18"     = 0:17,
                                                 "18 and older" = 18:100)))),
               " ! WARNING: The variable 'var3' is not part of the data frame.", info = "Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal")


# Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal
expect_message(sort_df <- small_df |>
                   sort_plus(by = var1,
                             formats = list(var2 = suppressMessages(
                                 discrete_format("under 18"     = 0:17,
                                                 "18 and older" = 18:100)))),
               " ~ NOTE: Format for variable", info = "Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal")


###############################################################################
# Abort checks
###############################################################################

# Sort aborts without by variables
expect_message(sort_df <- small_df |> sort_plus(), " X ERROR: No <by> variables provided. Sorting will be aborted.", info = "Sort aborts without by variables")
