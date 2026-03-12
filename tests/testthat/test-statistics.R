test_df <- data.frame(var1 = 1:5,
                      var2 = c(6, 7, 8, NA, 10),
                      var3 = 11:15)

test_that("Row wise calculation", {
    result <- test_df |> row_calculation("sum",  var1, var2, var3)

    expect_equal(result, c(18, 21, 24, 18, 30))
})
