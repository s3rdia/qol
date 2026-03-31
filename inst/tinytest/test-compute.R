###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(var1 = c("a", "a", "a", "b", "b"),
                      var2 = c(1, 2, 3, 4, 5),
                      var3 = c(10, 20, 30, 40, 50))


# Compute evaluates simple expressions
result_df <- test_df |> compute(sum = var2 + var3)

expected <- test_df[["var2"]] + test_df[["var3"]]

expect_equal(result_df[["sum"]], expected, info = "Compute evaluates simple expressions")


# Compute supports evaluation of functions
result_df <- test_df |> compute(col_sum = collapse::fsum(var2))

expected <- collapse::fsum(test_df[["var2"]])

expect_equal(collapse::funique(result_df[["col_sum"]]), expected, info = "Compute supports evaluation of functions")


# Compute supports qol functions
result_df <- test_df |> compute(row_sum = row_calculation("sum", var2, var3))

expected <- test_df[["var2"]] + test_df[["var3"]]

expect_equal(result_df[["row_sum"]], expected, info = "Compute supports qol functions")


# Compute supports value assignment
result_df <- test_df |> compute(var4 = 1, var5 = "Hello")

expect_equal(collapse::funique(result_df[["var4"]]), 1, info = "Compute supports value assignment")
expect_equal(collapse::funique(result_df[["var5"]]), "Hello", info = "Compute supports value assignment")


# Compute respects do_if filtering
result_df <- test_df |>
    do_if(var1 == "a") |>
        compute(sum = var2 + var3) |>
    end_do()

expected <- test_df[["var2"]] + test_df[["var3"]]
expected[test_df[["var1"]] != "a"] <- NA

expect_equal(result_df[["sum"]], expected, info = "Compute respects do_if filtering")


# Compute overwrites existing variable
result_df <- test_df |> compute(var2 = var3)

expect_true(!identical(result_df[["var2"]], test_df[["var2"]]), info = "Compute overwrites existing variable")
expect_equal(result_df[["var2"]], result_df[["var3"]], info = "Compute overwrites existing variable")


# Compute as do over loop
variables  <- c("NEW_VAR1", "NEW_VAR2")
values     <- c(1, 2)

do_over_df <- test_df |> compute(variables = values)

expect_true(all(c("NEW_VAR1", "NEW_VAR2") %in% names(do_over_df)), info = "Compute as do over loop")
expect_true(collapse::funique(do_over_df[["NEW_VAR1"]]) == 1, info = "Compute as do over loop")
expect_true(collapse::funique(do_over_df[["NEW_VAR2"]]) == 2, info = "Compute as do over loop")


# Compute handles multiple assignments
variables  <- c("NEW_VAR1", "NEW_VAR2")
values     <- c(1, 2)

result_df <- test_df |> compute(sum       = var2 + var3,
                                col_sum   = collapse::fsum(var2),
                                row_sum   = row_calculation("sum", var2, var3),
                                var4      = 1,
                                var5      = "Hello",
                                variables = values)

expect_true(all(c("sum", "col_sum", "row_sum", "var4", "var5", "NEW_VAR1", "NEW_VAR2") %in% names(result_df)), info = "Compute handles multiple assignments")

###############################################################################
# Warning checks
###############################################################################

# Type conversion in if. block on type mismatch
expect_message(result_df <- test_df |> compute(var1 = var2), " ! WARNING: Type mismatch", info = "Type conversion in if. block on type mismatch")

###############################################################################
# Abort checks
###############################################################################

# Compute aborts with no assignment
expect_message(result_df <- test_df |> compute(),
               " X ERROR: No assignments. Evaluation will be aborted.", info = "Compute aborts with no assignment")


# Compute aborts with no assignment
variables  <- c("NEW_VAR1", "NEW_VAR2", "NEW_VAR3")
values     <- c(1, 2)

expect_message(do_over_df <- test_df |> compute(variables = values),
               " X ERROR: Passed vectors are of unequal lengths.", info = "Compute aborts with no assignment")
