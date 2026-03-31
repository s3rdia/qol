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
    joined_data[[method]] <- suppressMessages(
        multi_join(list(df1, df2),
                   on  = "key",
                   how = join_methods[[method]],
                   keep_indicators = TRUE))
}

expect_equal(length(joined_data), 7, info = "Test all basic joins with one key")


# Join on multiple keys
left_joined <- suppressMessages(multi_join(list(df1b, df2b), on = c("key1", "key2")))

expect_true("b" %in% names(left_joined), info = "Join on multiple keys")


# Join more than two data frames
multiple_joined <- suppressMessages(multi_join(list(df1, df2, df3), on = "key"))

expect_true(all(c("b", "c") %in% names(multiple_joined)), info = "Join more than two data frames")


# Join multiple data frames on different variable names
multiple_joined <- suppressMessages(
    multi_join(list(df1c, df2c, df3c),
               on = list(df1c = c("key1", "key2"),
                         df2c = c("var1", "var2"),
                         df3c = c("any", "name"))))

expect_true(all(c("b", "c") %in% names(multiple_joined)), info = "Join multiple data frames on different variable names")


# Warning on invalid join method
expect_message(left_joined <- multi_join(list(df1, df2), on = "key", how = "test"), " ! WARNING: No valid join method provided, 'left' will be used.", info = "Warning on invalid join method")

expect_true("b" %in% names(left_joined), info = "Warning on invalid join method")


# Note on too many join methods provided
expect_message(left_joined <- multi_join(list(df1, df2),
                                         on  = "key",
                                         how = c("left", "right")), " ~ NOTE: Too many join methods given in <how>. Excess methods will remain unused.", info = "Note on too many join methods provided")

expect_true("b" %in% names(left_joined), info = "Note on too many join methods provided")

###############################################################################
# Abort checks
###############################################################################

# Abort join, if data frames aren't provided as list
expect_message(left_joined <- multi_join(c(df1, df2), on = "key"), " X ERROR: Data frames must be provided as a list. Join will be aborted.", info = "Abort join, if data frames aren't provided as list")


# Abort join, if only one data frames provided
expect_message(left_joined <- multi_join(list(df1), on = "key"), " X ERROR: At least two data frames are required. Join will be aborted.", info = "Abort join, if only one data frames provided")


# Abort join, if <on> variables are provided as unnamed list
expect_message(left_joined <- multi_join(list(df1, df2), on = list("key")), " X ERROR: If all data frames have the same variable names for the <on> variables", info = "Abort join, if <on> variables are provided as unnamed list")


# Abort join, if <on> variables are not provided for every data frame
expect_message(multiple_joined <-
                   multi_join(list(df1c, df2c, df3c),
                              on = list(df1c = c("key1", "key2"),
                                        df2c = c("var1", "var2"))), " X ERROR: Length of <on> doesn't match the number of provided data frames. Join will be aborted.", info = "Abort join, if <on> variables are not provided for every data frame")


# Abort join, if <on> variables are missing in data frame (equal names)
expect_message(left_joined <- multi_join(list(df1, df2), on = "var"), " X ERROR: Not all <on> variables", info = "Abort join, if <on> variables are missing in data frame (equal names)")


# Abort join, if <on> variables are missing in data frame (unequal names)
expect_message(multiple_joined <-
                   multi_join(list(df1c, df2c, df3c),
                              on = list(df1c = c("var1", "key2"),
                                        df2c = c("var1", "var2"),
                                        df3c = c("any", "name"))), " X ERROR: Not all <on> variables", info = "Abort join, if <on> variables are missing in data frame (unequal names)")


# Abort join, if second of following data frame doesn't consist of only unique values (equal names)
expect_message(left_joined <- multi_join(list(df1, df1), on = "key"), " X ERROR: The second and all following data frames need to have unique combinations", info = "Abort join, if second of following data frame doesn't consist of only unique values (equal names)")


# Abort join, if second of following data frame doesn't consist of only unique values  (unequal names)
expect_message(multiple_joined <-
                   multi_join(list(df1c, df1c, df3c),
                              on = list(df1c = c("key1", "key2"),
                                        df1c = c("key1", "key2"),
                                        df3c = c("any", "name"))), " X ERROR: The second and all following data frames need to have unique combinations", info = "Abort join, if second of following data frame doesn't consist of only unique values  (unequal names)")


# Abort join, if <on> variables have unequal length
expect_message(multiple_joined <- multi_join(list(df1c, df2c, df3c),
                              on = list(df1c = c("key1", "key2"),
                                        df2c = c("var1"),
                                        df3c = c("any", "name"))), " X ERROR: Unequal number of <on> variables provided", info = "Abort join, if <on> variables have unequal length")
