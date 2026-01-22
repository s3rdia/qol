###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(
    var1 = c(1, 10, 100),
    var2 = c(1, NA, 3),
    var3 = c("abc", "ab", "a"))

text_df <- data.frame(
    var1 = c("This is a text", "Hello World", "this is a Text"))


test_that("Concatenating multiple variables as provided", {
    concat_vec <- test_df |> concat(var1, var2, var3)

    expect_equal(concat_vec, c("11abc", "10NAab", "1003a"))
})


test_that("Concatenating multiple variables with automatic padding", {
    concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_char = "0")

    expect_equal(concat_vec, c("0011abc", "01000ab", "100300a"))
})


test_that("Concatenating multiple variables with individual padding length", {
    concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_char   = "0",
                                    padding_length = c(5, 6, 7))

    expect_equal(concat_vec, c("000010000010000abc", "0001000000000000ab", "00100000003000000a"))
})


test_that("Concatenating multiple variables with individual padding length and no padding character provided", {
    concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_length = c(5, 6, 7))

    expect_equal(concat_vec, c("    1     1    abc", "   10           ab", "  100     3      a"))
})


test_that("Concatenating multiple variables with individual padding length and too small values removes NA values", {
    concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_length = c(1, 1, 1))

    expect_equal(concat_vec, c("11abc", "10 ab", "1003a"))
})


test_that("Pad single variable", {
    concat_vec <- test_df |> concat(var1, padding_char = "0")

    expect_equal(concat_vec, c("001", "010", "100"))
})


test_that("Pad single variable with individual length", {
    concat_vec <- test_df |> concat(var1, padding_char   = "_",
                                          padding_length = 5)

    expect_equal(concat_vec, c("____1", "___10", "__100"))
})


test_that("Pad single variable on the right side", {
    concat_vec <- test_df |> concat(var1, padding_char = "0", padding_right = TRUE)

    expect_equal(concat_vec, c("100", "100", "100"))
})


test_that("Concatenating multiple variables with automatic padding on the right side", {
    concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_char = "0", padding_right = TRUE)

    expect_equal(concat_vec, c("1001abc", "1000ab0", "1003a00"))
})


test_that("Substring from the left with numeric value", {
    sub_vec <- text_df |> sub_string(var1, to = 3)

    expect_equal(sub_vec, c("Thi", "Hel", "thi"))
})


test_that("Substring from the right with numeric value", {
    sub_vec <- text_df |> sub_string(var1, from = 5)

    expect_equal(sub_vec, c(" is a text", "o World", " is a Text"))
})


test_that("Substring in the middle with numeric values", {
    sub_vec <- text_df |> sub_string(var1, from = 5, to = 10)

    expect_equal(sub_vec, c(" is a ", "o Worl", " is a "))
})


test_that("Substring from the left with character value", {
    sub_vec <- text_df |> sub_string(var1, to = "s")

    expect_equal(sub_vec, c("This", "Hello World", "this"))
})


test_that("Substring from the right with character value", {
    sub_vec <- text_df |> sub_string(var1, from = "s")

    expect_equal(sub_vec, c("s is a text", "Hello World", "s is a Text"))
})


test_that("Substring in the middle with character values", {
    sub_vec <- text_df |> sub_string(var1, from = "t", to = "s")

    expect_equal(sub_vec, c("", "Hello World", "this"))
})


test_that("Substring in the middle with character values and no case sensitivity", {
    sub_vec <- text_df |> sub_string(var1, from = "t", to = "s", case_sensitive = FALSE)

    expect_equal(sub_vec, c("This", "Hello World", "this"))
})

###############################################################################
# Warning checks
###############################################################################

test_that("Concatenating only possible with single length padding_char", {
    expect_message(concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_char = "00"),
                   " ! WARNING: <Padding chararacter> must be a single character. Concat will be done without <padding character>.")

    expect_equal(concat_vec, c("11abc", "10NAab", "1003a"))

    expect_message(concat_vec <- test_df |> concat(var1, var2, var3,
                                                   padding_char = ""),
                   " ! WARNING: <Padding chararacter> must be a single character. Concat will be done without <padding character>.")

    expect_equal(concat_vec, c("11abc", "10NAab", "1003a"))
})


test_that("Concatenating with too short individual padding length", {
    expect_message(concat_vec <- test_df |> concat(var1, var2, var3,
                                                   padding_char   = "0",
                                                   padding_length = c(5, 6)),
                   " ! WARNING: <Padding length> is shorter than the number of variables.")

    expect_equal(concat_vec, c("00001000001abc", "000100000000ab", "0010000000300a"))
})


test_that("Concatenating with too long individual padding length", {
    expect_message(concat_vec <- test_df |> concat(var1, var2, var3,
                                    padding_char   = "0",
                                    padding_length = c(5, 6, 7, 8)),
                   " ! WARNING: <Padding length> is longer than the number of variables.")

    expect_equal(concat_vec, c("000010000010000abc", "0001000000000000ab", "00100000003000000a"))
})


test_that("Substring throws a warning when variable is provided as a vector", {
    expect_message(sub_vec <- text_df |> sub_string(c(var1, var1), to = 3),
                   " ! WARNING: <Variable> may only be of length one. The first Element will be used.")
})


test_that("Substring throws a warning when to is provided as a vector", {
    expect_message(sub_vec <- text_df |> sub_string(var1, to = c(3, 5)),
                   " ! WARNING: <To> may only be of length one. The first Element will be used.")
})


test_that("Substring throws a warning when from is provided as a vector", {
    expect_message(sub_vec <- text_df |> sub_string(var1, from = c(3, 5)),
                   " ! WARNING: <From> may only be of length one. The first Element will be used.")
})

###############################################################################
# Abort checks
###############################################################################

test_that("Substring aborts if variable is not part of the data frame", {
    expect_message(sub_vec <- text_df |> sub_string(var2, to = 3),
                   " X ERROR: The provided <variable> '")
})


test_that("Substring aborts if variable is not character", {
    expect_message(sub_vec <- test_df |> sub_string(var1, to = 3),
                   " X ERROR: <Variable> type must be character. Substring will be aborted.")
})


test_that("Substring aborts if neither from nor to are provided", {
    expect_message(sub_vec <- text_df |> sub_string(var1),
                   " X ERROR: Neither <from> nor <to> is provided. Substring will be aborted.")
})
