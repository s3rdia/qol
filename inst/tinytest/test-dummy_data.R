set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

# Create dummy data with 1 observations
test_df <- dummy_data(1)
expect_equal(nrow(test_df), 1, info = "Create dummy data with 1 observations")


# Create dummy data with 10 observations
test_df <- dummy_data(10)
expect_equal(nrow(test_df), 10, info = "Create dummy data with 10 observations")


# Create dummy data with 100 observations
test_df <- dummy_data(100)
expect_equal(nrow(test_df), 100, info = "Create dummy data with 100 observations")


# Create dummy data with 100 observations
test_df <- dummy_data(1000)
expect_equal(nrow(test_df), 1000, info = "Create dummy data with 100 observations")


# Dummy data first_person has always person_id 1
first <- test_df |> collapse::fsubset(first_person == 1)
expect_equal(collapse::funique(first[["person_id"]]), 1, info = "Dummy data first_person has always person_id 1")


# Dummy data wide format has one row per household
test_df_wide <- dummy_data(100, wide = TRUE)

expect_equal(collapse::fnrow(test_df_wide), collapse::fnrow(unique(test_df_wide[, c("year", "state", "household_id")])),
             info = "Dummy data wide format has one row per household")


# Dummy data wide format has multiple variables with same name correpsonding to different persons
test_df_long <- dummy_data(100)
test_df_wide <- dummy_data(100, wide = TRUE)

long_age_cols  <- grep("^age", names(test_df_long), value = TRUE)
wide_age_cols  <- grep("^age", names(test_df_wide), value = TRUE)

expect_equal(length(long_age_cols), 1, info = "Dummy data wide format has multiple variables with same name correpsonding to different persons")
expect_true(length(wide_age_cols) > 1, info = "Dummy data wide format has multiple variables with same name correpsonding to different persons")


set_no_print()
