set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(
    group  = c("A", "A", "A", "A", "B", "B", "B", "B", "B"),
    value  = c(0, 1, 2, 2, 3, 4, 4, 100, NA),
    weight = c(7, 8, 6, 5, 3, 7, 9, 2, NA))

dummy_df <- dummy_data(1000)

dummy_df[["binary"]] <- replicate(nrow(dummy_df), {
    paste0(sample(0:1, 2, replace = TRUE), collapse = "")
})

###############################################################################
# Do statistics produce correct output (unweighted)
###############################################################################

# Unweighted sum is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "sum") |>
    remove_stat_extension("sum")

expect_equal(result_df[["value"]][1], collapse::fsum(c(0, 1, 2, 2)),       info = "Unweighted sum is correct")
expect_equal(result_df[["value"]][2], collapse::fsum(c(3, 4, 4, 100, NA)), info = "Unweighted sum is correct")


# Unweighted sum of weights is correct (every observation gets weight of 1)
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "sum_wgt")

expect_equal(result_df[["sum_wgt"]][1], 4, info = "Unweighted sum of weights is correct (every observation gets weight of 1)")
expect_equal(result_df[["sum_wgt"]][2], 5, info = "Unweighted sum of weights is correct (every observation gets weight of 1)")


# Unweighted frequency is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "freq") |>
    remove_stat_extension("freq")

expect_equal(result_df[["value"]][1], 4, info = "Unweighted frequency is correct")
expect_equal(result_df[["value"]][2], 4, info = "Unweighted frequency is correct")


# Unweighted frequency greater zero is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "freq_g0") |>
    remove_stat_extension("freq_g0")

expect_equal(result_df[["value"]][1], 3, info = "Unweighted frequency greater zero is correct")
expect_equal(result_df[["value"]][2], 4, info = "Unweighted frequency greater zero is correct")


# Unweighted missing is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "missing") |>
    remove_stat_extension("missing")

expect_equal(result_df[["value"]][1], 0, info = "Unweighted missing is correct")
expect_equal(result_df[["value"]][2], 1, info = "Unweighted missing is correct")


# Unweighted mean is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "mean") |>
    remove_stat_extension("mean")

expect_equal(result_df[["value"]][1], collapse::fmean(c(0, 1, 2, 2)), info = "Unweighted mean is correct")
expect_equal(result_df[["value"]][2], collapse::fmean(c(3, 4, 4, 100, NA)), info = "Unweighted mean is correct")


# Unweighted median is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "median") |>
    remove_stat_extension("median")

expect_equal(result_df[["value"]][1], collapse::fmedian(c(0, 1, 2, 2)), info = "Unweighted median is correct")
expect_equal(result_df[["value"]][2], collapse::fmedian(c(3, 4, 4, 100, NA)), info = "Unweighted median is correct")


# Unweighted mode is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "mode") |>
    remove_stat_extension("mode")

expect_equal(result_df[["value"]][1], 2, info = "Unweighted mode is correct")
expect_equal(result_df[["value"]][2], 4, info = "Unweighted mode is correct")


# Unweighted min and max are correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("min", "max"))

expect_equal(result_df[["value_min"]][1], collapse::fmin(c(0, 1, 2, 2)), info = "Unweighted min and max are correct")
expect_equal(result_df[["value_max"]][1], collapse::fmax(c(0, 1, 2, 2)), info = "Unweighted min and max are correct")
expect_equal(result_df[["value_min"]][2], collapse::fmin(c(3, 4, 4, 100, NA)), info = "Unweighted min and max are correct")
expect_equal(result_df[["value_max"]][2], collapse::fmax(c(3, 4, 4, 100, NA)), info = "Unweighted min and max are correct")


# Unweighted first and last are correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("first", "last"))

expect_equal(result_df[["value_first"]][1], 0, info = "Unweighted first and last are correct")
expect_equal(result_df[["value_last"]][1], 2, info = "Unweighted first and last are correct")
expect_equal(result_df[["value_first"]][2], 3, info = "Unweighted first and last are correct")
expect_equal(result_df[["value_last"]][2], 100, info = "Unweighted first and last are correct")


# Unweighted percentiles are correct
result_df <- test_df |>
    collapse::fsubset(!is.na(value)) |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("p1", "p99"))

expect_equal(result_df[["value_p1"]][1],  collapse::fquantile(c(0, 1, 2, 2),   probs = 0.01, names = FALSE), info = "Unweighted percentiles are correct")
expect_equal(result_df[["value_p99"]][1], collapse::fquantile(c(0, 1, 2, 2),   probs = 0.99, names = FALSE), info = "Unweighted percentiles are correct")
expect_equal(result_df[["value_p1"]][2],  collapse::fquantile(c(3, 4, 4, 100), probs = 0.01, names = FALSE), info = "Unweighted percentiles are correct")
expect_equal(result_df[["value_p99"]][2], collapse::fquantile(c(3, 4, 4, 100), probs = 0.99, names = FALSE), info = "Unweighted percentiles are correct")


# Unweighted sd and variance are correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("sd", "variance"))

expect_equal(result_df[["value_sd"]][1],       collapse::fsd(c(0, 1, 2, 2)), info = "Unweighted sd and variance are correct")
expect_equal(result_df[["value_variance"]][1], collapse::fvar(c(0, 1, 2, 2)), info = "Unweighted sd and variance are correct")
expect_equal(result_df[["value_sd"]][2],       collapse::fsd(c(3, 4, 4, 100, NA)), info = "Unweighted sd and variance are correct")
expect_equal(result_df[["value_variance"]][2], collapse::fvar(c(3, 4, 4, 100, NA)), info = "Unweighted sd and variance are correct")


# Unweighted percentages are correct
result_df <- dummy_df |>
    summarise_plus(class      = c(education, sex),
                   values     = income,
                   statistics = c("pct_group", "pct_total"),
                   na.rm      = TRUE)

sum_pct_group    <- collapse::fsum(result_df[["income_pct_group"]])
expect_pct_group <- length(unique(result_df[["education"]])) * 100

sum_pct_total    <- collapse::fsum(result_df[["income_pct_total"]])

expect_equal(sum_pct_group, expect_pct_group, info = "Unweighted percentages are correct")
expect_equal(sum_pct_total, 100, info = "Unweighted percentages are correct")

###############################################################################
# Do statistics produce correct output (weighted)
###############################################################################

# Weighted sum is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "sum",
                   weight     = weight) |>
        remove_stat_extension("sum")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted sum is correct")

expect_equal(result_df[["value"]][1], collapse::fsum(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)), info = "Weighted sum is correct")
expect_equal(result_df[["value"]][2], collapse::fsum(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)), info = "Weighted sum is correct")


# sum of weights is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "sum_wgt",
                   weight     = weight)

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "sum of weights is correct")

expect_equal(result_df[["sum_wgt"]][1], collapse::fsum(c(7, 8, 6, 5)), info = "sum of weights is correct")
expect_equal(result_df[["sum_wgt"]][2], collapse::fsum(c(3, 7, 9, 2, NA)), info = "sum of weights is correct")


# Weighted frequency is correct (unweighted count)
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "freq",
                   weight     = weight) |>
        remove_stat_extension("freq")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted frequency is correct (unweighted count)")

expect_equal(result_df[["value"]][1], 4, info = "Weighted frequency is correct (unweighted count)")
expect_equal(result_df[["value"]][2], 4, info = "Weighted frequency is correct (unweighted count)")


# Weighted frequency greater zero is correct (unweighted count > 0)
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "freq_g0",
                   weight     = weight) |>
        remove_stat_extension("freq_g0")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted frequency greater zero is correct (unweighted count > 0)")

expect_equal(result_df[["value"]][1], 3, info = "Weighted frequency greater zero is correct (unweighted count > 0)")
expect_equal(result_df[["value"]][2], 4, info = "Weighted frequency greater zero is correct (unweighted count > 0)")


# Weighted missing is correct (unweighted missing count)
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "missing",
                   weight     = weight) |>
        remove_stat_extension("missing")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted missing is correct (unweighted missing count)")

expect_equal(result_df[["value"]][1], 0, info = "Weighted missing is correct (unweighted missing count)")
expect_equal(result_df[["value"]][2], 1, info = "Weighted missing is correct (unweighted missing count)")


# Weighted mean is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "mean",
                   weight     = weight) |>
        remove_stat_extension("mean")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted mean is correct")

expect_equal(result_df[["value"]][1], collapse::fmean(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)), info = "Weighted mean is correct")
expect_equal(result_df[["value"]][2], collapse::fmean(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)), info = "Weighted mean is correct")


# Weighted median is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "median",
                   weight     = weight) |>
        remove_stat_extension("median")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted median is correct")

expect_equal(result_df[["value"]][1], collapse::fmedian(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)), info = "Weighted median is correct")
expect_equal(result_df[["value"]][2], collapse::fmedian(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)), info = "Weighted median is correct")


# Weighted mode is correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = "mode",
                   weight     = weight) |>
        remove_stat_extension("mode")

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted mode is correct")

expect_equal(result_df[["value"]][1], 2, info = "Weighted mode is correct")
expect_equal(result_df[["value"]][2], 4, info = "Weighted mode is correct")


# Weighted min and max are correct (same as unweighted)
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("min", "max"),
                   weight     = weight)

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted min and max are correct (same as unweighted)")

expect_equal(result_df[["value_min"]][1], collapse::fmin(c(0, 1, 2, 2)), info = "Weighted min and max are correct (same as unweighted)")
expect_equal(result_df[["value_max"]][1], collapse::fmax(c(0, 1, 2, 2)), info = "Weighted min and max are correct (same as unweighted)")
expect_equal(result_df[["value_min"]][2], collapse::fmin(c(3, 4, 4, 100, NA)), info = "Weighted min and max are correct (same as unweighted)")
expect_equal(result_df[["value_max"]][2], collapse::fmax(c(3, 4, 4, 100, NA)), info = "Weighted min and max are correct (same as unweighted)")


# Weighted first and last are correct (same as unweighted)
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("first", "last"),
                   weight     = weight)

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted first and last are correct (same as unweighted)")

expect_equal(result_df[["value_first"]][1], 0, info = "Weighted first and last are correct (same as unweighted)")
expect_equal(result_df[["value_last"]][1], 2, info = "Weighted first and last are correct (same as unweighted)")
expect_equal(result_df[["value_first"]][2], 3, info = "Weighted first and last are correct (same as unweighted)")
expect_equal(result_df[["value_last"]][2], 100, info = "Weighted first and last are correct (same as unweighted)")


# Weighted percentiles are correct
result_df <- test_df |>
    collapse::fsubset(!is.na(value)) |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("p1", "p99"),
                   weight     = weight)

expect_equal(result_df[["value_p1"]][1],  collapse::fquantile(c(0, 1, 2, 2),   probs = 0.01, w = c(7, 8, 6, 5), names = FALSE), info = "Weighted percentiles are correct")
expect_equal(result_df[["value_p99"]][1], collapse::fquantile(c(0, 1, 2, 2),   probs = 0.99, w = c(7, 8, 6, 5), names = FALSE), info = "Weighted percentiles are correct")
expect_equal(result_df[["value_p1"]][2],  collapse::fquantile(c(3, 4, 4, 100), probs = 0.01, w = c(3, 7, 9, 2), names = FALSE), info = "Weighted percentiles are correct")
expect_equal(result_df[["value_p99"]][2], collapse::fquantile(c(3, 4, 4, 100), probs = 0.99, w = c(3, 7, 9, 2), names = FALSE), info = "Weighted percentiles are correct")


# Weighted sd and variance are correct
result_df <- test_df |>
    summarise_plus(class      = group,
                   values     = value,
                   statistics = c("sd", "variance"),
                   weight     = weight)

expect_message(print_stack_as_messages("NOTE"), "Missing values in weight variable 'weight' will be converted to 0", info = "Weighted sd and variance are correct")

expect_equal(result_df[["value_sd"]][1],       collapse::fsd(c(0, 1, 2, 2),        w = c(7, 8, 6, 5)), info = "Weighted sd and variance are correct")
expect_equal(result_df[["value_variance"]][1], collapse::fvar(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)), info = "Weighted sd and variance are correct")
expect_equal(result_df[["value_sd"]][2],       collapse::fsd(c(3, 4, 4, 100, NA),  w = c(3, 7, 9, 2, NA)), info = "Weighted sd and variance are correct")
expect_equal(result_df[["value_variance"]][2], collapse::fvar(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)), info = "Weighted sd and variance are correct")


# Weighted percentages are correct
result_df <- dummy_df |>
    summarise_plus(class      = c(education, sex),
                   values     = income,
                   statistics = c("pct_group", "pct_total"),
                   weight     = weight,
                   na.rm      = TRUE)

sum_pct_group    <- collapse::fsum(result_df[["income_pct_group"]])
expect_pct_group <- length(unique(result_df[["education"]])) * 100

sum_pct_total    <- collapse::fsum(result_df[["income_pct_total"]])

expect_equal(sum_pct_group, expect_pct_group, info = "Weighted percentages are correct")
expect_equal(sum_pct_total, 100, info = "Weighted percentages are correct")

###############################################################################
# Other checks
###############################################################################

# Different way of passing variables in (single variables)
result_df1 <- dummy_df |>
    summarise_plus(class  = year,
                   values = income,
                   weight = weight)

result_df2 <- dummy_df |>
    summarise_plus(class  = "year",
                   values = "income",
                   weight = "weight")

expect_identical(result_df1, result_df2, info = "Different way of passing variables in (single variables)")


# Different way of passing variables in (multiple variables)
result_df1 <- dummy_df |>
    summarise_plus(class  = c(year, sex),
                   values = c(income, weight),
                   weight = weight)

result_df2 <- dummy_df |>
    summarise_plus(class  = c("year", "sex"),
                   values = c("income", "weight"),
                   weight = "weight")

result_df3 <- dummy_df |>
    summarise_plus(class  = list(year, sex),
                   values = list(income, weight),
                   weight = weight)

result_df4 <- dummy_df |>
    summarise_plus(class  = list("year", "sex"),
                   values = list("income", "weight"),
                   weight = "weight")

result_df5 <- dummy_df |>
    summarise_plus(class  = year,
                   values = income,
                   weight = weight)

result_df6 <- dummy_df |>
    summarise_plus(class  = "year",
                   values = "income",
                   weight = "weight")

expect_identical(result_df1, result_df2, info = "Different way of passing variables in (multiple variables)")
expect_identical(result_df1, result_df3, info = "Different way of passing variables in (multiple variables)")
expect_identical(result_df1, result_df4, info = "Different way of passing variables in (multiple variables)")


# Simplest form without specifying statistics leads to sum and freq output
result_df <- dummy_df |>
    summarise_plus(class  = year,
                   values = income)

expect_equal(collapse::fncol(result_df), 6, info = "Simplest form without specifying statistics leads to sum and freq output")
expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df)), info = "Simplest form without specifying statistics leads to sum and freq output")


# Weighted vs. unweighted output
result_df1 <- dummy_df |>
    summarise_plus(class  = year,
                   values = income)

result_df2 <- dummy_df |>
    summarise_plus(class  = year,
                   values = income,
                   weight = weight)

expect_equal(collapse::fncol(result_df1), 6, info = "Weighted vs. unweighted output")
expect_equal(collapse::fncol(result_df2), 6, info = "Weighted vs. unweighted output")

expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df1)), info = "Weighted vs. unweighted output")
expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df2)), info = "Weighted vs. unweighted output")

expect_identical(result_df1[["year"]], result_df2[["year"]], info = "Weighted vs. unweighted output")
expect_identical(result_df1[["income_freq"]], result_df2[["income_freq"]], info = "Weighted vs. unweighted output")
expect_false(identical(result_df1[["income_sum"]], result_df2[["income_sum"]]), info = "Weighted vs. unweighted output")


# Specifying many statistics doesn't break function
result_df <- dummy_df |>
    collapse::fsubset(!is.na(income) & !is.na(probability)) |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income, probability),
                   statistics = c("sum", "freq", "freq_g0", "mean", "median", "mode", "min", "max",
                                  "first", "last", "pct_group", "pct_total", "sum_wgt", "p1", "p99",
                                  "sd", "variance", "missing"),
                   weight     = weight)

variable_count <- 2 + (2 * 17) + 1 + 3
expect_equal(collapse::fncol(result_df), variable_count, info = "Specifying many statistics doesn't break function")


# Specifying only one statistic puts out variable names without extension
result_df1 <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = weight,
                   statistics = "sum")

result_df2 <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = weight,
                   statistics = c("mean", "sum_wgt"))

expect_equal(names(result_df1), c("year", "sex", "TYPE", "TYPE_NR", "DEPTH", "weight_sum"), info = "Specifying only one statistic puts out variable names without extension")
expect_equal(names(result_df2), c("year", "sex", "TYPE", "TYPE_NR", "DEPTH", "weight_mean", "sum_wgt"), info = "Specifying only one statistic puts out variable names without extension")


# None existent statistics will be omitted
result_df <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = income,
                   statistics = c("sum", "test"),
                   weight     = weight)

expect_equal(collapse::fncol(result_df), 6, info = "None existent statistics will be omitted")


# Merging variables back to original data frame creates new column
result_df <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income),
                   statistics = "sum",
                   weight     = weight,
                   merge_back = TRUE)

expect_equal(collapse::fncol(result_df), collapse::fncol(dummy_df) + 1, info = "Merging variables back to original data frame creates new column")
expect_equal(collapse::fnrow(result_df), collapse::fnrow(dummy_df), info = "Merging variables back to original data frame creates new column")


# Don't nest class variables but compute them separately and fuse variables into one super variable
result_df <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income),
                   statistics = "sum",
                   weight     = weight,
                   nesting    = "single")

expect_equal(collapse::fncol(result_df), 5, info = "Don't nest class variables but compute them separately and fuse variables into one super variable")

expect_true(all(c("TYPE", "TYPE_NR", "DEPTH") %in% names(result_df)), info = "Don't nest class variables but compute them separately and fuse variables into one super variable")

expect_equal(max(result_df[["DEPTH"]]), 1, info = "Don't nest class variables but compute them separately and fuse variables into one super variable")


# Drop auto generated variables after summarise
result_df <- dummy_df |>
    summarise_plus(class      = c(year, sex),
                   values     = c(income),
                   statistics = "sum",
                   weight     = weight,
                   nesting    = "single") |>
    drop_type_vars()

expect_equal(collapse::fncol(result_df), 2, info = "Drop auto generated variables after summarise")

expect_true(!all(c("TYPE", "TYPE_NR", "DEPTH") %in% names(result_df)), info = "Drop auto generated variables after summarise")


# Generate all possible combinations of class variables
result_df <- dummy_df |>
    summarise_plus(class      = c(year, sex, age, education),
                   values     = c(income),
                   statistics = c("sum", "mean", "pct_group", "pct_total"),
                   weight     = weight,
                   nesting    = "all")

expect_equal(collapse::fncol(result_df), 11, info = "Generate all possible combinations of class variables")

expect_true(all(c("TYPE", "TYPE_NR", "DEPTH") %in% names(result_df)), info = "Generate all possible combinations of class variables")

expect_equal(min(result_df[["DEPTH"]]), 0, info = "Generate all possible combinations of class variables")
expect_equal(max(result_df[["DEPTH"]]), 4, info = "Generate all possible combinations of class variables")

expect_true(is.na(result_df[["year"]][1]) & is.na(result_df[["sex"]][1]) & is.na(result_df[["age"]][1]) & is.na(result_df[["education"]][1]), info = "Generate all possible combinations of class variables")


# Generate only chosen combinations of class variables
result_df <- dummy_df |>
    summarise_plus(class      = c(year, sex, age),
                   values     = c(income),
                   statistics = c("sum", "mean", "pct_group", "pct_total"),
                   weight     = weight,
                   type       = c("total", "year + sex", "sex + age", "age"),
                   nesting    = "all")

expect_equal(min(result_df[["DEPTH"]]), 0, info = "Generate only chosen combinations of class variables")
expect_equal(max(result_df[["DEPTH"]]), 2, info = "Generate only chosen combinations of class variables")

expect_equal(length(unique(result_df[["TYPE"]])), 4, info = "Generate only chosen combinations of class variables")
expect_true("year+sex" %in% result_df[["TYPE"]], info = "Generate only chosen combinations of class variables")
expect_true("sex+age" %in% result_df[["TYPE"]], info = "Generate only chosen combinations of class variables")
expect_true("age" %in% result_df[["TYPE"]], info = "Generate only chosen combinations of class variables")
expect_true("total" %in% result_df[["TYPE"]], info = "Generate only chosen combinations of class variables")


# Summarise possible with empty class vector
result_df <- dummy_df |>
    summarise_plus(class  = c(),
                   values = income,
                   statistics = "sum")

expect_equal(collapse::fncol(result_df), 4, info = "Summarise possible with empty class vector")
expect_equal(collapse::fnrow(result_df), 1, info = "Summarise possible with empty class vector")


# Summarise possible with no class variables provided
result_df <- dummy_df |>
    summarise_plus(values = income,
                   statistics = "sum")

expect_equal(collapse::fncol(result_df), 4, info = "Summarise possible with no class variables provided")
expect_equal(collapse::fnrow(result_df), 1, info = "Summarise possible with no class variables provided")


# Summarise uses temporary variable, if no analysis variable is provided
result_df <- dummy_df |> summarise_plus()

expect_equal(as.character(result_df[1, ]), c("pseudo_class", "1", "1", "1000"), info = "Summarise uses temporary variable, if no analysis variable is provided")

###############################################################################
# Format checks
###############################################################################

# Apply single discrete labels
sex. <- discrete_format(
    "Male"   = 1,
    "Female" = 2)
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

format_df <- dummy_df |>
    summarise_plus(class      = c(sex, age),
                   values     = income,
                   statistics = "sum",
                   formats    = list(sex = sex., age = age.),
                   weight     = weight,
                   nesting    = "deepest")

no_format_df <- dummy_df |>
    summarise_plus(class      = c(sex, age),
                   values     = income,
                   statistics = "sum",
                   weight     = weight,
                   nesting    = "deepest")

expect_equal(collapse::fsum(format_df[["income"]]),
             collapse::fsum(no_format_df[["income"]]), info = "Apply single discrete labels")
expect_true(all(c("Male", "Female") %in% format_df[["sex"]]), info = "Apply single discrete labels")
expect_true(all(c("under 18",
                  "18 to under 25",
                  "25 to under 55",
                  "55 to under 65",
                  "65 and older") %in% format_df[["age"]]), info = "Apply single discrete labels")


# Apply discrete multilabels
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)
age. <- discrete_format(
    "Total"          = 0:100,
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

format_df <- dummy_df |>
    summarise_plus(class      = c(sex, age),
                   values     = weight,
                   statistics = "sum",
                   formats    = list(sex = sex., age = age.),
                   nesting    = "deepest",
                   na.rm      = TRUE) |>
    remove_stat_extension("sum")

no_format_df <- dummy_df |>
    summarise_plus(class      = c(sex, age),
                   values     = weight,
                   statistics = "sum",
                   nesting    = "deepest",
                   na.rm      = TRUE) |>
    remove_stat_extension("sum")

expect_equal(collapse::fsum(format_df[["weight"]]),
             collapse::fsum(no_format_df[["weight"]]) * 4, info = "Apply discrete multilabels")
expect_true(all(c("Total", "Male", "Female") %in% format_df[["sex"]]), info = "Apply discrete multilabels")
expect_true(all(c("Total",
                  "under 18",
                  "18 to under 25",
                  "25 to under 55",
                  "55 to under 65",
                  "65 and older") %in% format_df[["age"]]), info = "Apply discrete multilabels")


# Apply single interval label
income. <- interval_format(
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

format_df <- dummy_df |>
    summarise_plus(class      = c(income),
                   values     = weight,
                   statistics = "sum",
                   formats    = list(income = income.),
                   nesting    = "deepest") |>
    remove_stat_extension("sum")

expect_equal(collapse::fsum(format_df[["weight"]]),
             collapse::fsum(dummy_df[["weight"]]), info = "Apply single interval label")
expect_true(all(c("below 500",
                  "500 to under 1000",
                  "1000 to under 2000",
                  "2000 and more") %in% format_df[["income"]]), info = "Apply single interval label")


# Apply interval multilabel
income. <- interval_format(
    "Total"              = 0:99999,
    "below 500"          = 0:499,
    "500 to under 1000"  = 500:999,
    "1000 to under 2000" = 1000:1999,
    "2000 and more"      = 2000:99999)

format_df <- dummy_df |>
    summarise_plus(class      = c(income),
                   values     = weight,
                   statistics = "sum",
                   formats    = list(income = income.),
                   nesting    = "deepest",
                   na.rm      = TRUE) |>
    remove_stat_extension("sum")

no_format_df <- dummy_df |>
    collapse::fsubset(!is.na(income))

expect_equal(collapse::fsum(format_df[["weight"]]),
             collapse::fsum(no_format_df[["weight"]]) * 2, info = "Apply interval multilabel")
expect_true(all(c("Total",
                  "below 500",
                  "500 to under 1000",
                  "1000 to under 2000",
                  "2000 and more") %in% format_df[["income"]]), info = "Apply interval multilabel")

# Output missing categories in summarise_plus with deepest nesting
sex. <- discrete_format(
    "Male"   = 1,
    "Female" = 2)
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

format_df <- dummy_df |>
    if.(sex == 1 & (age < 25 | age >= 55)) |>
    summarise_plus(class   = c(year, sex, age),
                   values  = weight,
                   formats = list(sex = sex., age = age.),
                   print_miss = TRUE)

expect_true("Female" %in% format_df[["sex"]], info = "Output missing categories in summarise_plus with deepest nesting")
expect_true("25 to under 55" %in% format_df[["age"]], info = "Output missing categories in summarise_plus with deepest nesting")


# Output missing categories in summarise_plus with all combnations
sex. <- discrete_format(
    "Male"   = 1,
    "Female" = 2)
age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 25" = 18:24,
    "25 to under 55" = 25:54,
    "55 to under 65" = 55:64,
    "65 and older"   = 65:100)

format_df <- dummy_df |>
    if.(sex == 1 & (age < 25 | age >= 55)) |>
    summarise_plus(class   = c(year, sex, age),
                   values  = weight,
                   formats = list(sex = sex., age = age.),
                   nesting = "all",
                   print_miss = TRUE)

expect_true("Female" %in% format_df[["sex"]], info = "Output missing categories in summarise_plus with all combnations")
expect_true("25 to under 55" %in% format_df[["age"]], info = "Output missing categories in summarise_plus with all combnations")


# Use the 'other' format keyword with summarise_plus
age. <- discrete_format(
    "under 18"     = 0:17,
    "18 and older" = "other")

format_df <- dummy_df |>
    summarise_plus(class   = age,
                   values  = weight,
                   formats = list(age = age.))

unique_values <- as.character(format_df[["age"]] |> collapse::funique())

expect_equal(unique_values,
             c("under 18", "18 and older", NA), info = "Use the 'other' format keyword with summarise_plus")


# summarise_plus doesn't convert numeric values stored as character (short route)
result_df <- dummy_df |>
        summarise_plus(class  = binary,
                       values = weight)

expect_equal(result_df[["binary"]], c("00", "01", "10", "11"), info = "summarise_plus doesn't convert numeric values stored as character (short route)")


# summarise_plus doesn't convert numeric values stored as character (long route)
result_df <- dummy_df |>
      summarise_plus(class  = binary,
                     values = weight,
                     statistics = "mean")

expect_equal(result_df[["binary"]], c("00", "01", "10", "11"), info = "summarise_plus doesn't convert numeric values stored as character (long route)")


# summarise_plus converts numeric values back to numeric (short route)
result_df <- dummy_df |>
      summarise_plus(class  = age,
                     values = weight)

expect_equal(result_df[["age"]][1:5], c(0, 1, 2, 3, 4), info = "summarise_plus converts numeric values back to numeric (short route)")


# summarise_plus converts numeric values back to numeric (long route)
result_df <- dummy_df |>
      summarise_plus(class  = age,
                     values = weight,
                     statistics = "mean")

expect_equal(result_df[["age"]][1:5], c(0, 1, 2, 3, 4), info = "summarise_plus converts numeric values back to numeric (long route)")

###############################################################################
# Warning checks
###############################################################################

# Entering none existing variable as weight leads to unweighted results
result_df1 <- dummy_df |> summarise_plus(class  = year,
                                         values = income)

result_df2 <- dummy_df |>
           summarise_plus(class  = year,
                          values = income,
                          weight = abc)

expect_warning(print_stack_as_messages("WARNING"), "Provided weight variable is not part of the data frame",
               info = "Entering none existing variable as weight leads to unweighted results")

expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df1)),
            info = "Entering none existing variable as weight leads to unweighted results")
expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df2)),
            info = "Entering none existing variable as weight leads to unweighted results")

expect_identical(result_df1, result_df2, info = "Entering none existing variable as weight leads to unweighted results")


# Entering none numeric variable as weight leads to unweighted results
result_df1 <- dummy_df |> summarise_plus(class  = year,
                                         values = income)

result_df2 <- dummy_df |>
           summarise_plus(class  = year,
                          values = income,
                          weight = education)

expect_warning(print_stack_as_messages("WARNING"), "Provided weight variable is not numeric",
               info = "Entering none numeric variable as weight leads to unweighted results")

expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df1)),
            info = "Entering none numeric variable as weight leads to unweighted results")
expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df2)),
            info = "Entering none numeric variable as weight leads to unweighted results")

expect_identical(result_df1, result_df2, info = "Entering none numeric variable as weight leads to unweighted results")


# Percentiles won't be calculated if value variable has NA values
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex),
                          values     = c(income, probability),
                          statistics = c("p1", "p99"),
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "To calculate percentiles there may be no NAs in the value variables",
               info = "Percentiles won't be calculated if value variable has NA values")

expect_equal(collapse::fncol(result_df), 5, info = "Percentiles won't be calculated if value variable has NA values")


# Percentiles above 100 not allowed
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex),
                          values     = c(income, probability),
                          statistics = c("p101"),
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "Percentiles are only possible from p0 to p100",
               info = "Percentiles above 100 not allowed")

expect_equal(collapse::fncol(result_df), 7, info = "Percentiles above 100 not allowed")


# None existent class variables will be omitted
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex, test),
                          values     = c(income, probability),
                          statistics = "sum",
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "This variable will be omitted during computation",
               info = "None existent class variables will be omitted")

expect_equal(collapse::fncol(result_df), 7, info = "None existent class variables will be omitted")


# None existent analysis variable will be omitted
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex),
                          values     = c(income, probability, test),
                          statistics = "sum",
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "This variable will be omitted during computation",
               info = "None existent analysis variable will be omitted")

expect_equal(collapse::fncol(result_df), 7, info = "None existent analysis variable will be omitted")


# Double class variables will be omitted
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex, sex),
                          values     = c(income, probability),
                          statistics = "sum",
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "Some <class> variables are provided more than once",
               info = "Double class variables will be omitted")

expect_equal(collapse::fncol(result_df), 7, info = "Double class variables will be omitted")


# Double analysis variables will be omitted
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex),
                          values     = c(income, probability, income),
                          statistics = "sum",
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "Some <values> variables are provided more than once",
               info = "Double analysis variables will be omitted")

expect_equal(collapse::fncol(result_df), 7, info = "Double analysis variables will be omitted")


# Analysis variable will be omitted if also passed as class variable
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex, age),
                          values     = c(age, probability),
                          statistics = "sum",
                          weight     = weight)

expect_warning(print_stack_as_messages("WARNING"), "This variable will be omitted as <values> variable during computation",
               info = "Analysis variable will be omitted if also passed as class variable")

expect_equal(collapse::fncol(result_df), 7, info = "Analysis variable will be omitted if also passed as class variable")


# Merging variables back works if wrong nesting option ist provided
result_df <- dummy_df |>
           summarise_plus(class      = c(year, sex),
                          values     = c(income),
                          statistics = "sum",
                          weight     = weight,
                          nesting    = "all",
                          merge_back = TRUE)

expect_warning(print_stack_as_messages("WARNING"), "Merging variables back only works with nesting",
               info = "Merging variables back works if wrong nesting option ist provided")

expect_equal(collapse::fncol(result_df), collapse::fncol(dummy_df) + 1, info = "Merging variables back works if wrong nesting option ist provided")
expect_equal(collapse::fnrow(result_df), collapse::fnrow(dummy_df), info = "Merging variables back works if wrong nesting option ist provided")


# Drop auto generated variables will be omitted if not in data frame
result_df <- dummy_df |> drop_type_vars()

expect_warning(print_stack_as_messages("WARNING"), "The provided variable to drop", info = "Drop auto generated variables will be omitted if not in data frame")


# Invalid statistic will be omitted
result_df <- dummy_df |>
           summarise_plus(class      = year,
                          values     = income,
                          statistics = c("test", "sum"))

expect_warning(print_stack_as_messages("WARNING"), "<Statistic> 'test' is invalid and will be omitted.", info = "Invalid statistic will be omitted")

expect_equal(collapse::fncol(result_df), 5, info = "Invalid statistic will be omitted")


# Invalid statistic will be omitted 'sum' will be chosen as statistic if no valid one is provided
result_df <- dummy_df |>
           summarise_plus(class      = year,
                          values     = income,
                          statistics = "test")

expect_warning(print_stack_as_messages("WARNING"), "No valid <statistic> selected. 'sum' will be used.",
           info = "'Invalid statistic will be omitted 'sum' will be chosen as statistic if no valid one is provided")

expect_equal(collapse::fncol(result_df), 5, info = "'Invalid statistic will be omitted 'sum' will be chosen as statistic if no valid one is provided")


# summarise_plus throws a warning, if invalid format is used
result_list <- dummy_df |>
    summarise_plus(class   = "sex",
                   formats = list(sex = test))

expect_warning(print_stack_as_messages("WARNING"), "Format for variable 'sex' does not exist and can't be applied.",
               info = "summarise_plus throws a warning, if invalid format is used")


set_no_print()
