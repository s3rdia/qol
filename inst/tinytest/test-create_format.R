set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

# Create single discrete label
sex. <- discrete_format(
        "Male"   = 1,
        "Female" = 2)

expect_true(all(c("value", "label") %in% names(sex.)), info = "Create single discrete label")
expect_equal(nrow(sex.), 2, info = "Create single discrete label")
expect_equal(ncol(sex.), 2, info = "Create single discrete label")


# Create discrete multilabel
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

expect_true(all(c("value", "label") %in% names(sex.)), info = "Create discrete multilabel")
expect_equal(nrow(sex.), 4, info = "Create discrete multilabel")
expect_equal(ncol(sex.), 2, info = "Create discrete multilabel")


# Create single interval label
income. <- interval_format(
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

expect_true(all(c("from", "to", "label") %in% names(income.)), info = "Create single interval label")
expect_equal(nrow(income.), 4, info = "Create single interval label")
expect_equal(ncol(income.), 3, info = "Create single interval label")


# Create interval multilabel
income. <- interval_format(
    "Total"              = 0:99999,
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

expect_true(all(c("from", "to", "label") %in% names(income.)), info = "Create interval multilabel")
expect_equal(nrow(income.), 5, info = "Create interval multilabel")
expect_equal(ncol(income.), 3, info = "Create interval multilabel")


# Create interval format with low and high keywords
income. <- interval_format(
    "Total"              = c("low", "high"),
    "below 500"          = c("low", 499),
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = c(2000, "high"))

expect_true(all(c("from", "to", "label") %in% names(income.)), info = "Create interval format with low and high keywords")
expect_equal(income.[["from"]][1], -Inf, info = "Create interval format with low and high keywords")
expect_equal(income.[["to"]][1],    Inf, info = "Create interval format with low and high keywords")


# Expand formats
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

income. <- interval_format(
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

expand_df <- expand_formats(sex., income.,
                            names = c("sex", "income"))

expect_true(all(c("sex", "income") %in% names(expand_df)), info = "Expand formats")
expect_equal(collapse::fncol(expand_df), 2, info = "Expand formats")
expect_equal(collapse::fnrow(expand_df), 12, info = "Expand formats")


# Expand formats provided as list
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

income. <- interval_format(
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

expand_df <- expand_formats(list(sex., income.), names = c("sex", "income"))

expect_true(all(c("sex", "income") %in% names(expand_df)), info = "Expand formats provided as list")
expect_equal(collapse::fncol(expand_df), 2, info = "Expand formats provided as list")
expect_equal(collapse::fnrow(expand_df), 12, info = "Expand formats provided as list")


# Expand formats returns unique label values if only one format is provided
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

expand_df <- expand_formats(sex.)

expect_equal(expand_df[[1]], c("Total", "Male", "Female"), info = "Expand formats returns unique label values if only one format is provided")

###############################################################################
# Abort checks
###############################################################################

# Abort format expansion, if a data frame has no label column
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

sex. <- sex. |> dropp("label")

income. <- interval_format(
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

expand_df <- expand_formats(sex., income., names = c("sex", "income"))

expect_error(print_stack_as_messages("ERROR"), "A data frame is missing the 'label' column. This function is especially for expanding formats created", info = "Abort format expansion, if a data frame has no label column")


# Create single discrete label aborts, if elements not provided in the correct way
discrete_format(test == 1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single discrete label aborts, if elements not provided in the correct way")


# Create single discrete label aborts, if list element is missing a name
discrete_format(1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single discrete label aborts, if list element is missing a name")


# Create single interval label aborts, if elements not provided in the correct way
interval_format(test == 1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single interval label aborts, if elements not provided in the correct way")


# Create single interval label aborts, if list element is missing a name
discrete_format(1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single interval label aborts, if list element is missing a name")


set_no_print()
