set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

dummy_df <- dummy_data(100)


# Starting monitoring creates new data table and variables
monitor_df <- NULL |> monitor_start("Unit test")

expect_equal(nrow(monitor_df), 1, info = "Starting monitoring creates new data table and variables")
expect_equal(ncol(monitor_df), 3, info = "Starting monitoring creates new data table and variables")
expect_true("Unit test" %in% monitor_df[["section"]], info = "Starting monitoring creates new data table and variables")


# Ending monitoring stores system time in current observation and calculates delta
monitor_df <- NULL |> monitor_start("Unit test")
monitor_df <- monitor_df |> monitor_end()

expect_equal(nrow(monitor_df), 1, info = "Ending monitoring stores system time in current observation and calculates delta")
expect_equal(ncol(monitor_df), 5, info = "Ending monitoring stores system time in current observation and calculates delta")
expect_true("delta" %in% names(monitor_df), info = "Ending monitoring stores system time in current observation and calculates delta")
expect_true("Unit test" %in% monitor_df[["section"]], info = "Ending monitoring stores system time in current observation and calculates delta")


# Next monitoring ends the current one and directly starts a new one
monitor_df <- NULL |> monitor_start("Unit test")
monitor_df <- monitor_df |> monitor_next("Next")
monitor_df <- monitor_df |> monitor_end()

expect_equal(nrow(monitor_df), 2, info = "Next monitoring ends the current one and directly starts a new one")
expect_equal(ncol(monitor_df), 5, info = "Next monitoring ends the current one and directly starts a new one")
expect_true("Unit test" %in% monitor_df[["section"]], info = "Next monitoring ends the current one and directly starts a new one")
expect_true("Next" %in% monitor_df[["section"]], info = "Next monitoring ends the current one and directly starts a new one")


# Plotting monitoring resets par to the values before
old_par <- graphics::par()

monitor_df <- NULL |> monitor_start("Unit test")
monitor_df <- monitor_df |> monitor_next("Next")
monitor_df <- monitor_df |> monitor_end()

monitor_df |> monitor_plot()
monitor_df |> monitor_plot(by = "group")

expect_identical(old_par, graphics::par(), info = "Plotting monitoring resets par to the values before")


set_no_print()
