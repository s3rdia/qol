###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(1000))


# Get inverse variable names from data frame works correct
var_names     <- c("state", "age", "sex", "education")
inverse_names <- dummy_df |> inverse(var_names)

var_count <- length(names(dummy_df))

expect_equal(length(var_names) + length((inverse_names)), var_count, info = "Get inverse variable names from data frame works correct")
expect_true(!all(var_names %in% inverse_names), info = "Get inverse variable names from data frame works correct")


# Get variable names between two variables
var_names <- dummy_df |> vars_between(state, person_id)

expect_equal(length(var_names), 5, info = "Get variable names between two variables")


# Get variable names from variable to the end
var_names <- dummy_df |> vars_between(body_weight)

expect_equal(length(var_names), 8, info = "Get variable names from variable to the end")


# Get variable names from beginning to position
var_names <- dummy_df |> vars_between(to = person_id)

expect_equal(length(var_names), 6, info = "Get variable names from beginning to position")

###############################################################################
# Warning checks
###############################################################################

# Providing not existing from-variable throws a warning
expect_message(var_names <- dummy_df |> vars_between(test, person_id), " ! WARNING: 'from' variable '", info = "Providing not existing from-variable throws a warning")

expect_equal(length(var_names), 6, info = "Providing not existing from-variable throws a warning")


# Providing not existing to-variable throws a warning
expect_message(var_names <- dummy_df |> vars_between(state, test), " ! WARNING: 'to' variable '", info = "Providing not existing to-variable throws a warning")

expect_equal(length(var_names), 19, info = "Providing not existing to-variable throws a warning")


# Providing more than one from or to variable throws a note
expect_message(var_names <- dummy_df |> vars_between(c(state, age), income),
               " ~ NOTE: Only single variable names allowed for 'from' and 'to'. The respective\n", info = "Providing more than one from or to variable throws a note")

expect_equal(length(var_names), 14, info = "Providing more than one from or to variable throws a note")
