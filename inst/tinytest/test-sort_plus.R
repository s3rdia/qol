set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
# Printing the output is always set to FALSE, so the overall code can be tested
# but without drawing the whole outputs on screen.
###############################################################################

small_df <- data.frame(var1 = c(3, 2, 1, 2),
                       var2 = c("c", "a", "a", "b"))


# Simple numeric sort
sort_df <- small_df |> sort_plus(by = var1)

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Simple numeric sort")


# Simple character sort
sort_df <- small_df |> sort_plus(by = var2)

expect_equal(sort_df[["var2"]], c("a", "a", "b", "c"), info = "Simple character sort")


# Simple numeric sort, descending
sort_df <- small_df |> sort_plus(by = var1, order = "descending")

expect_equal(sort_df[["var1"]], c(3, 2, 2, 1), info = "Simple numeric sort, descending")


# Sort with preserve
sort_df <- small_df |> sort_plus(by = var1, preserve = var2)

expect_equal(sort_df[["var1"]], c(3, 1, 2, 2), info = "Sort with preserve")
expect_equal(sort_df[["var2"]], c("c", "a", "a", "b"), info = "Sort with preserve")


# Sort with format
sort_df <- small_df |>
    sort_plus(by = var2,
              formats = list(var2 =
                  discrete_format("1" = "a",
                                  "2" = "c",
                                  "3" = "b")))

expect_equal(sort_df[["var2"]], c("a", "a", "c", "b"), info = "Sort with format")

###############################################################################
# Warning checks
###############################################################################

# Sort warning with invalid by variable
sort_df <- small_df |> sort_plus(by = var3)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable", info = "Sort warning with invalid by variable")


# Sort warning when by variable is also part of preserve, sort normal
sort_df <- small_df |> sort_plus(by = var1, preserve = var1)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable", info = "Sort warning when by variable is also part of preserve, sort normal")


# Sort warning when wrong order is specified, sorting in ascending order instead
sort_df <- small_df |> sort_plus(by = var1, order = "test")

expect_warning(print_stack_as_messages("WARNING"), "<Order> other than 'ascending'/'a' or 'descending'/'d' specified, which is", info = "Sort warning when wrong order is specified, sorting in ascending order instead")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when wrong order is specified, sorting in ascending order instead")


# Sort warning when trying to apply a multilabel, sort normal
sort_df <- small_df |>
	   sort_plus(by = var1,
				 formats = list(age =
								discrete_format("Total"          = 1:3,
												"Number 1 and 2" = 1:2,
												"Number 3"       = 3)))

expect_warning(print_stack_as_messages("WARNING"), "Applying a multilabel to variable", info = "Sort warning when trying to apply a multilabel, sort normal")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when trying to apply a multilabel, sort normal")


# Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal
sort_df <- small_df |>
	   sort_plus(by = var1,
				 formats = list(var3 =
					 discrete_format("under 18"     = 0:17,
									 "18 and older" = 18:100)))

expect_warning(print_stack_as_messages("WARNING"), "The variable 'var3' is not part of the data frame.", info = "Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when trying to apply a format to a variable which is not in the data frame, sort normal")


# Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal
sort_df <- small_df |>
	   sort_plus(by = var1,
				 formats = list(var2 =
					 discrete_format("under 18"     = 0:17,
									 "18 and older" = 18:100)))

expect_message(print_stack_as_messages("NOTE"), "Format for variable", info = "Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal")

expect_equal(sort_df[["var1"]], c(1, 2, 2, 3), info = "Sort warning when trying to apply a format to a variable which is not part of the by variables, sort normal")


###############################################################################
# Abort checks
###############################################################################

# Sort aborts without by variables
sort_df <- small_df |> sort_plus()

expect_error(print_stack_as_messages("ERROR"), "No <by> variables provided. Sorting will be aborted.", info = "Sort aborts without by variables")


set_no_print()
