set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- dummy_data(1000)
dummy_df[["age"]] <- as.character(dummy_df[["age"]])


# Convert to numeric only if possible
test_df1 <- dummy_df |> convert_numeric("age")
test_df2 <- dummy_df |> convert_numeric("education")

expect_true(is.numeric(test_df1[["age"]]), info = "Convert to numeric only if possible")
expect_true(is.character(test_df2[["age"]]), info = "Convert to numeric only if possible")
expect_true(is.character(test_df2[["education"]]), info = "Convert to numeric only if possible")


# Convert arguments to character vector
return_param <- function(parameter){
    args_to_char(substitute(parameter))
}

expect_equal(return_param(age), "age", info = "Convert arguments to character vector")
expect_equal(return_param("age"), "age", info = "Convert arguments to character vector")
expect_equal(return_param(c(age, sex, income, weight)), c("age", "sex", "income", "weight"), info = "Convert arguments to character vector")
expect_equal(return_param(c("age", "sex", "income", "weight")), c("age", "sex", "income", "weight"), info = "Convert arguments to character vector")


# Convert arguments to character vector in nested function
nested_function <- function(parameter){
    get_origin_as_char(parameter, substitute(parameter))
}

return_param <- function(parameter){
    nested_function(parameter)
}

expect_equal(return_param("age"), "age", info = "Convert arguments to character vector in nested function")
expect_equal(return_param(c("age", "sex", "income", "weight")), c("age", "sex", "income", "weight"), info = "Convert arguments to character vector in nested function")


# Convert ellipsis to character vector
return_param <- function(...){
    dots_to_char(...)
}

expect_equal(return_param(age), "age", info = "Convert ellipsis to character vector")
expect_equal(return_param("age"), "age", info = "Convert ellipsis to character vector")
expect_equal(return_param(c(age, sex, income, weight)), c("age", "sex", "income", "weight"), info = "Convert ellipsis to character vector")
expect_equal(return_param(c("age", "sex", "income", "weight")), c("age", "sex", "income", "weight"), info = "Convert ellipsis to character vector")


set_no_print()
