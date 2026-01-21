###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(
    var1 = c(1, 10, 100),
    var2 = c(1, NA, 3),
    var3 = c("abc", "ab", "a"))


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
