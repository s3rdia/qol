set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

###############################################################################
# Setup test data frames
###############################################################################

# One key
df1 <- data.frame(key = c(1, 1, 1, 2, 2, 2),
                  a   = c("a", "a", "a", "a", "a", "a"))

df2 <- data.frame(key = c(2, 3),
                  b   = c("b", "b"))

df3 <- data.frame(key = c(1, 2),
                  c   = c("c", "c"))

df4 <- data.frame(key  = c(1, 2),
                  key1 = c(1, 2),
                  a    = c("d", "d"))

# Multiple same keys
df1b <- data.frame(key1 = c(1, 1, 1, 2, 2, 2),
                   key2 = c("a", "a", "a", "a", "a", "a"),
                   a    = c("a", "a", "a", "a", "a", "a"))

df2b <- data.frame(key1 = c(2, 3),
                   key2 = c("a", "a"),
                   b    = c("b", "b"))

# Multiple different keys
df1c <- data.frame(key1 = c(1, 1, 1, 2, 2, 2),
                   key2 = c("a", "a", "a", "a", "a", "a"),
                   a    = c("a", "a", "a", "a", "a", "a"))

df2c <- data.frame(var1 = c(2, 3),
                   var2 = c("a", "a"),
                   b    = c("b", "b"))

df3c <- data.frame(any  = c(1, 2),
                   name = c("a", "a"),
                   c    = c("c", "c"))

###############################################################################
# Successful joins
###############################################################################

# Test all basic joins with one key
join_methods <- c("left", "right", "inner", "full", "outer", "left_inner", "right_inner")
joined_data  <- list()

for (method in seq_along(join_methods)){
    joined_data[[method]] <-
        multi_join(list(df1, df2),
                   on  = "key",
                   how = join_methods[[method]],
                   keep_indicators = TRUE)
}

expect_equal(length(joined_data), 7, info = "Test all basic joins with one key")


# Join on multiple keys
left_joined <- multi_join(list(df1b, df2b), on = c("key1", "key2"))

expect_true("b" %in% names(left_joined), info = "Join on multiple keys")

left_joined <- multi_join(list(df1b, df2b), on = list(key1, key2))

expect_true("b" %in% names(left_joined), info = "Join on multiple keys")


# Join more than two data frames
multiple_joined <- multi_join(list(df1, df2, df3), on = "key")

expect_true(all(c("b", "c") %in% names(multiple_joined)), info = "Join more than two data frames")


# Join multiple data frames on different variable names
multiple_joined <-
    multi_join(list(df1c, df2c, df3c),
               on = list(df1c = c("key1", "key2"),
                         df2c = c("var1", "var2"),
                         df3c = c("any", "name")))

expect_true(all(c("b", "c") %in% names(multiple_joined)), info = "Join multiple data frames on different variable names")

multiple_joined2 <-
    multi_join(list(df1c, df2c, df3c),
               on = list(df1c = c(key1, key2),
                         df2c = c(var1, var2),
                         df3c = c(any, name)))

expect_equal(multiple_joined, multiple_joined2, info = "Join multiple data frames on different variable names")


# Join the first data frame on different variables with each following data frame
df1d <- data.frame(key1 = c(1, 1, 2),
                   key2 = c("a", "a", "b"),
                   key3 = c(10, 20, 20),
                   a    = "a")

df2d <- data.frame(var1 = c(1, 2),
                   var2 = c("a", "b"),
                   b    = "b")

df3d <- data.frame(any  = c("a", "a", "b"),
                   name = c(10, 20, 20),
                   c    = "c")

different_keys_join <-
    multi_join(list(df1d, df2d, df3d),
               on = list(df1d = list(c("key1", "key2"), c("key3", "key2")),
                         df2d = c("var1", "var2"),
                         df3d = c("name", "any")))

expect_equal(collapse::fnrow(different_keys_join), 3, info = "Join the first data frame on different variables with each following data frame")
expect_true(all(different_keys_join[["b"]] == "b"), info = "Join the first data frame on different variables with each following data frame")
expect_true(all(different_keys_join[["c"]] == "c"), info = "Join the first data frame on different variables with each following data frame")


# Unquoted join variables give the same result as quoted ones
different_keys_join2 <-
    multi_join(list(df1d, df2d, df3d),
               on = list(df1d = list(c(key1, key2), c(key3, key2)),
                         df2d = c(var1, var2),
                         df3d = c(name, any)))

expect_equal(different_keys_join, different_keys_join2, info = "Unquoted join variables give the same result as quoted ones")


# The last variable combination is repeated if too few are provided
repeated_joined <-
    multi_join(list(df1c, df2c, df3c),
               on = list(df1c = list(c("key1", "key2")),
                         df2c = c("var1", "var2"),
                         df3c = c("any", "name")))

expect_true(all(c("b", "c") %in% names(repeated_joined)), info = "The last variable combination is repeated if too few are provided")


# multi_join can handle many to one joins
base_df <- data.frame(key = c(1, 1, 2),
                      a   = c("a", "a", "a"))

join_df  <- data.frame(key = c(2, 3),
                       b   = c("b", "b"))

keys_expected <- c(left        = 3,
                   right       = 2,
                   inner       = 1,
                   full        = 4,
                   outer       = 3,
                   left_inner  = 2,
                   right_inner = 1)

for (method in names(keys_expected)){
    joined <- multi_join(list(base_df, join_df),
                         on      = "key",
                         how     = method,
                         monitor = FALSE)

    expect_equal(nrow(joined), unname(keys_expected[[method]]),
                 info = paste("multi_join can handle many to one joins: ", method))
}


# Multi-frame joins where a widening join (right, full, outer) is followed by another join
df1_3 <- data.frame(key = c(1, 2, 3), a = "a")
df2_3 <- data.frame(key = c(2, 3, 4), b = "b")
df3_3 <- data.frame(key = c(3, 4, 5), c = "c")

wide_left <- multi_join(list(df1_3, df2_3, df3_3), on = "key", how = c("right", "left"), monitor = FALSE)

expect_equal(sort(wide_left[["key"]]), 2:4, info = "Multi-frame joins where a widening join (right, full, outer) is followed by another join")

full_left <- multi_join(list(df1_3, df2_3, df3_3), on = "key", how = c("full", "left"), monitor = FALSE)

expect_equal(sort(full_left[["key"]]), 1:4, info = "Multi-frame joins where a widening join (right, full, outer) is followed by another join")

outer_outer <- multi_join(list(df1_3, df2_3, df3_3), on = "key", how = c("outer", "outer"), monitor = FALSE)

expect_equal(sort(outer_outer[["key"]]), c(1, 3, 5), info = "Multi-frame joins where a widening join (right, full, outer) is followed by another join")

right_inner_right_inner <- multi_join(list(df1_3, df2_3, df3_3), on = "key", how = c("right_inner", "right_inner"), monitor = FALSE)

expect_equal(sort(right_inner_right_inner[["key"]]), c(3, 5), info = "Multi-frame joins where a widening join (right, full, outer) is followed by another join")

###############################################################################
# Warning checks
###############################################################################

# Warning on invalid join method
left_joined <- multi_join(list(df1, df2), on = "key", how = "test")

expect_warning(print_stack_as_messages("WARNING"), "No valid join method provided, 'left' will be used.", info = "Warning on invalid join method")
expect_true("b" %in% names(left_joined), info = "Warning on invalid join method")


# Note on too many join methods provided
left_joined <- multi_join(list(df1, df2),
                                         on  = "key",
                                         how = c("left", "right"))

expect_message(print_stack_as_messages("NOTE"), "Too many join methods given in <how>. Excess methods will remain unused.",
               info = "Note on too many join methods provided")
expect_true("b" %in% names(left_joined), info = "Note on too many join methods provided")


# Warning duplicate variables on join
left_joined <- multi_join(list(df1, df4), on = "key")

expect_warning(print_stack_as_messages("WARNING"), "Duplicate variable names found", info = "Warning duplicate variables on join")
expect_true(collapse::fncol(left_joined) == 3, info = "Warning duplicate variables on join")

left_joined <- multi_join(list(df1, df4), on = list(df1 = "key",
                                                    df4 = "key1"))

expect_warning(print_stack_as_messages("WARNING"), "Duplicate variable names found", info = "Warning duplicate variables on join")
expect_true(collapse::fncol(left_joined) == 3, info = "Warning duplicate variables on join")

###############################################################################
# Abort checks
###############################################################################

# Abort join, if data frames aren't provided as list
left_joined <- multi_join(c(df1, df2), on = "key")

expect_error(print_stack_as_messages("ERROR"), "Data frames must be provided as a list. Join will be aborted.",
             info = "Abort join, if data frames aren't provided as list")


# Abort join, if only one data frames provided
left_joined <- multi_join(list(df1), on = "key")

expect_error(print_stack_as_messages("ERROR"), "At least two data frames are required. Join will be aborted.",
             info = "Abort join, if only one data frames provided")


# Abort join, if <on> variables are provided as incomplete unnamed list
left_joined <- multi_join(list(df1b, df2b), on = list(a = "key1", "key2"))

expect_error(print_stack_as_messages("ERROR"), "If all data frames have the same variable names for the <on> variables",
             info = "Abort join, if <on> variables are provided as incomplete unnamed list")


# Abort join, if <on> variables are not provided for every data frame
multiple_joined <- multi_join(list(df1c, df2c, df3c),
                              on = list(df1c = c("key1", "key2"),
                                        df2c = c("var1", "var2")))

expect_error(print_stack_as_messages("ERROR"), "Length of <on> doesn't match the number of provided data frames. Join will be aborted.",
             info = "Abort join, if <on> variables are not provided for every data frame")


# Abort join, if <on> variables are missing in data frame (equal names)
left_joined <- multi_join(list(df1, df2), on = "var")

expect_error(print_stack_as_messages("ERROR"), "Not all <on> variables", info = "Abort join, if <on> variables are missing in data frame (equal names)")


# Abort join, if <on> variables are missing in data frame (unequal names)
multiple_joined <- multi_join(list(df1c, df2c, df3c),
                              on = list(df1c = c("var1", "key2"),
                                        df2c = c("var1", "var2"),
                                        df3c = c("any", "name")))

expect_error(print_stack_as_messages("ERROR"), "Not all <on> variables", info = "Abort join, if <on> variables are missing in data frame (unequal names)")


# Abort join, if second of following data frame doesn't consist of only unique values (equal names)
left_joined <- multi_join(list(df1, df1), on = "key")

expect_error(print_stack_as_messages("ERROR"), "The second and all following data frames need to have unique combinations",
             info = "Abort join, if second of following data frame doesn't consist of only unique values (equal names)")


# Abort join, if second of following data frame doesn't consist of only unique values  (unequal names)
multiple_joined <- multi_join(list(df1c, df1c, df3c),
                              on = list(df1c = c("key1", "key2"),
                                        df1c = c("key1", "key2"),
                                        df3c = c("any", "name")))

expect_error(print_stack_as_messages("ERROR"), "The second and all following data frames need to have unique combinations",
             info = "Abort join, if second of following data frame doesn't consist of only unique values  (unequal names)")


# Abort join, if <on> variables have unequal length
multiple_joined <- multi_join(list(df1c, df2c, df3c),
                              on = list(df1c = c("key1", "key2"),
                                        df2c = c("var1"),
                                        df3c = c("any", "name")))

expect_error(print_stack_as_messages("ERROR"), "Unequal number of <on> variables provided", info = "Abort join, if <on> variables have unequal length")


set_no_print()
