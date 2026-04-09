set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- dummy_data(1000)


# Different way of passing variables in
split_list1 <- dummy_df |> split_by(sex)
split_list2 <- dummy_df |> split_by("sex")

expect_identical(split_list1, split_list2, info = "Different way of passing variables in")


# Split data frame by variable
split_list <- dummy_df |> split_by(sex)

expect_inherits(split_list, "list", info = "Split data frame by variable")
expect_equal(length(split_list), 3, info = "Split data frame by variable")

expect_true(!2 %in% split_list[["1"]][["sex"]], info = "Split data frame by variable")
expect_true(!1 %in% split_list[["2"]][["sex"]], info = "Split data frame by variable")
expect_true(!all(c(1, 2) %in% split_list[["NA_sex"]][["sex"]]), info = "Split data frame by variable")


# Split data frame by condition
split_list <- dummy_df |> split_by(sex == 1)

expect_inherits(split_list, "list", info = "Split data frame by condition")
expect_equal(length(split_list), 1, info = "Split data frame by condition")
expect_true(!2 %in% split_list[["sex == 1"]][["sex"]], info = "Split data frame by condition")


# Split data frame by multiple conditions
split_list <- dummy_df |>
       split_by(sex == 1 & income > 2000,
                age < 18)

expect_inherits(split_list, "list", info = "Split data frame by multiple conditions")
expect_equal(length(split_list), 2, info = "Split data frame by multiple conditions")
expect_true(min(split_list[[1]][["income"]]) > 2000, info = "Split data frame by multiple conditions")
expect_true(max(split_list[[2]][["age"]]) < 18, info = "Split data frame by multiple conditions")


# Split with inverse group
split_list <- dummy_df |>
       split_by(sex == 1, inverse = TRUE)

expect_inherits(split_list, "list", info = "Split with inverse group")
expect_equal(length(split_list), 1, info = "Split with inverse group")
expect_true(!1 %in% split_list[["not (sex == 1)"]][["sex"]], info = "Split with inverse group")


# Split data frame by variable and condition
split_list <- dummy_df |>
       split_by(sex,
                age < 18)

expect_inherits(split_list, "list", info = "Split data frame by variable and condition")
expect_equal(length(split_list), 4, info = "Split data frame by variable and condition")

expect_true(!2 %in% split_list[["1"]][["sex"]], info = "Split data frame by variable and condition")
expect_true(!1 %in% split_list[["2"]][["sex"]], info = "Split data frame by variable and condition")
expect_true(!all(c(1, 2) %in% split_list[["NA_sex"]][["sex"]]), info = "Split data frame by variable and condition")
expect_true(max(split_list[["age < 18"]][["age"]]) < 18, info = "Split data frame by variable and condition")


# Split data frame by variable using formats
split_list <- dummy_df |>
       split_by(sex,
                formats = list(sex = discrete_format(
                    "Total"  = 1:2,
                    "Male"   = 1,
                    "Female" = 2)))

expect_inherits(split_list, "list", info = "Split data frame by variable using formats")
expect_equal(length(split_list), 4, info = "Split data frame by variable using formats")

expect_equal(names(split_list), c("Total", "Male", "Female", "NA_sex"), info = "Split data frame by variable using formats")

###############################################################################
# Warning checks
###############################################################################


# Split data frame throws a warning, if variable is not part of the data frame
split_list <- dummy_df |> split_by("test")

expect_warning(print_stack_as_messages("WARNING"), "The provided <conditions> variable 'test' is not part of", info = "Split data frame throws a warning, if variable is not part of the data frame")


###############################################################################
# Abort checks
###############################################################################


# Split data frame aborts, if duplicate list entry names are produced
split_list <- dummy_df |> split_by("state", "age")

expect_error(print_stack_as_messages("ERROR"), "Variable 'age' caused duplicate list entry names.", info = "Split data frame aborts, if duplicate list entry names are produced")


# Split data frame aborts, if no valid split element is passed
split_list <- dummy_df |> split_by(1)

expect_error(print_stack_as_messages("ERROR"), "Only single variables or conditions allowed. Splitting will be aborted.", info = "Split data frame aborts, if no valid split element is passed")


set_no_print()
