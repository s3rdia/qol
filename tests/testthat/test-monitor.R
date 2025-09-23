###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- suppressMessages(dummy_data(100))


test_that("Starting monitoring creates new data table and variables", {
    monitor_df <- NULL |> monitor_start("Unit test")

    expect_equal(nrow(monitor_df), 1)
    expect_equal(ncol(monitor_df), 3)
    expect_true("Unit test" %in% monitor_df[["section"]])
})


test_that("Ending monitoring stores system time in current observation and calculates delta", {
    monitor_df <- NULL |> monitor_start("Unit test")
    monitor_df <- monitor_df |> monitor_end()

    expect_equal(nrow(monitor_df), 1)
    expect_equal(ncol(monitor_df), 5)
    expect_true("delta" %in% names(monitor_df))
    expect_true("Unit test" %in% monitor_df[["section"]])
})


test_that("Next monitoring ends the current one and directly starts a new one", {
    monitor_df <- NULL |> monitor_start("Unit test")
    monitor_df <- monitor_df |> monitor_next("Next")
    monitor_df <- monitor_df |> monitor_end()

    expect_equal(nrow(monitor_df), 2)
    expect_equal(ncol(monitor_df), 5)
    expect_true("Unit test" %in% monitor_df[["section"]])
    expect_true("Next" %in% monitor_df[["section"]])
})


test_that("Plotting monitoring resets par to the values before", {
    old_par <- graphics::par()

    monitor_df <- NULL |> monitor_start("Unit test")
    monitor_df <- monitor_df |> monitor_next("Next")
    monitor_df <- monitor_df |> monitor_end()

    monitor_df |> monitor_plot()
    monitor_df |> monitor_plot(by = "group")

    expect_identical(old_par, graphics::par())
})
