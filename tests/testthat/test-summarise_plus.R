###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

test_df <- data.frame(
    group  = c("A", "A", "A", "A", "B", "B", "B", "B", "B"),
    value  = c(0, 1, 2, 2, 3, 4, 4, 100, NA),
    weight = c(7, 8, 6, 5, 3, 7, 9, 2, NA))

dummy_df <- suppressMessages(dummy_data(1000))

###############################################################################
# Do statistics produce correct output (unweighted)
###############################################################################
test_that("Unweighted sum is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "sum") |>
        remove_stat_extension("sum")

    expect_equal(result_df[1, "value"], collapse::fsum(c(0, 1, 2, 2)))
    expect_equal(result_df[2, "value"], collapse::fsum(c(3, 4, 4, 100, NA)))
})


test_that("Unweighted sum of weights is correct (every observation gets weight of 1)", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "sum_wgt")

    expect_equal(result_df[1, "sum_wgt"], 4)
    expect_equal(result_df[2, "sum_wgt"], 5)
})


test_that("Unweighted frequency is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "freq") |>
        remove_stat_extension("freq")

    expect_equal(result_df[1, "value"], 4)
    expect_equal(result_df[2, "value"], 4)
})


test_that("Unweighted frequency greater zero is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "freq_g0") |>
        remove_stat_extension("freq_g0")

    expect_equal(result_df[1, "value"], 3)
    expect_equal(result_df[2, "value"], 4)
})


test_that("Unweighted missing is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "missing") |>
        remove_stat_extension("missing")

    expect_equal(result_df[1, "value"], 0)
    expect_equal(result_df[2, "value"], 1)
})


test_that("Unweighted mean is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "mean") |>
        remove_stat_extension("mean")

    expect_equal(result_df[1, "value"], collapse::fmean(c(0, 1, 2, 2)))
    expect_equal(result_df[2, "value"], collapse::fmean(c(3, 4, 4, 100, NA)))
})


test_that("Unweighted median is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "median") |>
        remove_stat_extension("median")

    expect_equal(result_df[1, "value"], collapse::fmedian(c(0, 1, 2, 2)))
    expect_equal(result_df[2, "value"], collapse::fmedian(c(3, 4, 4, 100, NA)))
})


test_that("Unweighted mode is correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "mode") |>
        remove_stat_extension("mode")

    expect_equal(result_df[1, "value"], 2)
    expect_equal(result_df[2, "value"], 4)
})


test_that("Unweighted min and max are correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("min", "max"))

    expect_equal(result_df[1, "value_min"], collapse::fmin(c(0, 1, 2, 2)))
    expect_equal(result_df[1, "value_max"], collapse::fmax(c(0, 1, 2, 2)))
    expect_equal(result_df[2, "value_min"], collapse::fmin(c(3, 4, 4, 100, NA)))
    expect_equal(result_df[2, "value_max"], collapse::fmax(c(3, 4, 4, 100, NA)))
})


test_that("Unweighted first and last are correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("first", "last"))

    expect_equal(result_df[1, "value_first"], 0)
    expect_equal(result_df[1, "value_last"], 2)
    expect_equal(result_df[2, "value_first"], 3)
    expect_equal(result_df[2, "value_last"], 100)
})


test_that("Unweighted percentiles are correct", {
    result_df <- test_df |>
        collapse::fsubset(!is.na(value)) |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("p1", "p99"))

    expect_equal(result_df[1, "value_p1"],  collapse::fquantile(c(0, 1, 2, 2),   probs = 0.01, names = FALSE))
    expect_equal(result_df[1, "value_p99"], collapse::fquantile(c(0, 1, 2, 2),   probs = 0.99, names = FALSE))
    expect_equal(result_df[2, "value_p1"],  collapse::fquantile(c(3, 4, 4, 100), probs = 0.01, names = FALSE))
    expect_equal(result_df[2, "value_p99"], collapse::fquantile(c(3, 4, 4, 100), probs = 0.99, names = FALSE))
})


test_that("Unweighted sd and variance are correct", {
    result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("sd", "variance"))

    expect_equal(result_df[1, "value_sd"],       collapse::fsd(c(0, 1, 2, 2)))
    expect_equal(result_df[1, "value_variance"], collapse::fvar(c(0, 1, 2, 2)))
    expect_equal(result_df[2, "value_sd"],       collapse::fsd(c(3, 4, 4, 100, NA)))
    expect_equal(result_df[2, "value_variance"], collapse::fvar(c(3, 4, 4, 100, NA)))
})


test_that("Unweighted percentages are correct", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(education, sex),
                       values     = income,
                       statistics = c("pct_group", "pct_total"),
                       na.rm      = TRUE)

    sum_pct_group    <- collapse::fsum(result_df[["income_pct_group"]])
    expect_pct_group <- length(unique(result_df[["education"]])) * 100

    sum_pct_total    <- collapse::fsum(result_df[["income_pct_total"]])

    expect_equal(sum_pct_group, expect_pct_group)
    expect_equal(sum_pct_total, 100)
})

###############################################################################
# Do statistics produce correct output (weighted)
###############################################################################
test_that("Weighted sum is correct", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "sum",
                       weight     = weight) |>
            remove_stat_extension("sum"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], collapse::fsum(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)))
    expect_equal(result_df[2, "value"], collapse::fsum(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)))
})


test_that("sum of weights is correct", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "sum_wgt",
                       weight     = weight), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "sum_wgt"], collapse::fsum(c(7, 8, 6, 5)))
    expect_equal(result_df[2, "sum_wgt"], collapse::fsum(c(3, 7, 9, 2, NA)))
})


test_that("Weighted frequency is correct (unweighted count)", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "freq",
                       weight     = weight) |>
            remove_stat_extension("freq"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], 4)
    expect_equal(result_df[2, "value"], 4)
})


test_that("Weighted frequency greater zero is correct (unweighted count > 0)", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "freq_g0",
                       weight     = weight) |>
            remove_stat_extension("freq_g0"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], 3)
    expect_equal(result_df[2, "value"], 4)
})


test_that("Weighted missing is correct (unweighted missing count)", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "missing",
                       weight     = weight) |>
            remove_stat_extension("missing"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], 0)
    expect_equal(result_df[2, "value"], 1)
})


test_that("Weighted mean is correct", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "mean",
                       weight     = weight) |>
            remove_stat_extension("mean"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], collapse::fmean(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)))
    expect_equal(result_df[2, "value"], collapse::fmean(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)))
})


test_that("Weighted median is correct", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "median",
                       weight     = weight) |>
            remove_stat_extension("median"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], collapse::fmedian(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)))
    expect_equal(result_df[2, "value"], collapse::fmedian(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)))
})


test_that("Weighted mode is correct", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = "mode",
                       weight     = weight) |>
            remove_stat_extension("mode"), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value"], 2)
    expect_equal(result_df[2, "value"], 4)
})


test_that("Weighted min and max are correct (same as unweighted)", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("min", "max"),
                       weight     = weight), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value_min"], collapse::fmin(c(0, 1, 2, 2)))
    expect_equal(result_df[1, "value_max"], collapse::fmax(c(0, 1, 2, 2)))
    expect_equal(result_df[2, "value_min"], collapse::fmin(c(3, 4, 4, 100, NA)))
    expect_equal(result_df[2, "value_max"], collapse::fmax(c(3, 4, 4, 100, NA)))
})


test_that("Weighted first and last are correct (same as unweighted)", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("first", "last"),
                       weight     = weight), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value_first"], 0)
    expect_equal(result_df[1, "value_last"], 2)
    expect_equal(result_df[2, "value_first"], 3)
    expect_equal(result_df[2, "value_last"], 100)
})


test_that("Weighted percentiles are correct", {
    result_df <- test_df |>
        collapse::fsubset(!is.na(value)) |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("p1", "p99"),
                       weight     = weight)

    expect_equal(result_df[1, "value_p1"],  collapse::fquantile(c(0, 1, 2, 2),   probs = 0.01, w = c(7, 8, 6, 5), names = FALSE))
    expect_equal(result_df[1, "value_p99"], collapse::fquantile(c(0, 1, 2, 2),   probs = 0.99, w = c(7, 8, 6, 5), names = FALSE))
    expect_equal(result_df[2, "value_p1"],  collapse::fquantile(c(3, 4, 4, 100), probs = 0.01, w = c(3, 7, 9, 2), names = FALSE))
    expect_equal(result_df[2, "value_p99"], collapse::fquantile(c(3, 4, 4, 100), probs = 0.99, w = c(3, 7, 9, 2), names = FALSE))
})


test_that("Weighted sd and variance are correct", {
    expect_message(result_df <- test_df |>
        summarise_plus(class      = group,
                       values     = value,
                       statistics = c("sd", "variance"),
                       weight     = weight), "~ NOTE: Missing values in weight variable 'weight' will be converted to 0")

    expect_equal(result_df[1, "value_sd"],       collapse::fsd(c(0, 1, 2, 2),        w = c(7, 8, 6, 5)))
    expect_equal(result_df[1, "value_variance"], collapse::fvar(c(0, 1, 2, 2),       w = c(7, 8, 6, 5)))
    expect_equal(result_df[2, "value_sd"],       collapse::fsd(c(3, 4, 4, 100, NA),  w = c(3, 7, 9, 2, NA)))
    expect_equal(result_df[2, "value_variance"], collapse::fvar(c(3, 4, 4, 100, NA), w = c(3, 7, 9, 2, NA)))
})


test_that("Weighted percentages are correct", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(education, sex),
                       values     = income,
                       statistics = c("pct_group", "pct_total"),
                       weight     = weight,
                       na.rm      = TRUE)

    sum_pct_group    <- collapse::fsum(result_df[["income_pct_group"]])
    expect_pct_group <- length(unique(result_df[["education"]])) * 100

    sum_pct_total    <- collapse::fsum(result_df[["income_pct_total"]])

    expect_equal(sum_pct_group, expect_pct_group)
    expect_equal(sum_pct_total, 100)
})

###############################################################################
# Other checks
###############################################################################

test_that("Different way of passing variables in (single variables)", {
    result_df1 <- suppressMessages(dummy_df |>
        summarise_plus(class  = year,
                       values = income,
                       weight = weight))

    result_df2 <- suppressMessages(dummy_df |>
        summarise_plus(class  = "year",
                       values = "income",
                       weight = "weight"))

    expect_identical(result_df1, result_df2)
})


test_that("Different way of passing variables in (multiple variables)", {
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

    expect_identical(result_df1, result_df2)
    expect_identical(result_df1, result_df3)
    expect_identical(result_df1, result_df4)
})


test_that("Simplest form without specifying statistics leads to sum and freq output", {
    result_df <- suppressMessages(dummy_df |>
        summarise_plus(class  = year,
                       values = income))

    expect_equal(ncol(result_df), 6)
    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df)))
})


test_that("Weighted vs. unweighted output", {
    result_df1 <- suppressMessages(dummy_df |>
        summarise_plus(class  = year,
                       values = income))

    result_df2 <- suppressMessages(dummy_df |>
        summarise_plus(class  = year,
                       values = income,
                       weight = weight))

    expect_equal(ncol(result_df1), 6)
    expect_equal(ncol(result_df2), 6)

    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df1)))
    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df2)))

    expect_identical(result_df1[["year"]], result_df2[["year"]])
    expect_identical(result_df1[["income_freq"]], result_df2[["income_freq"]])
    expect_false(identical(result_df1[["income_sum"]], result_df2[["income_sum"]]))
})


test_that("Specifying many statistics doesn't break function", {
    result_df <- dummy_df |>
        collapse::fsubset(!is.na(income) & !is.na(probability)) |>
        summarise_plus(class      = c(year, sex),
                       values     = c(income, probability),
                       statistics = c("sum", "freq", "freq_g0", "mean", "median", "mode", "min", "max",
                                      "first", "last", "pct_group", "pct_total", "sum_wgt", "p1", "p99",
                                      "sd", "variance", "missing"),
                       weight     = weight)

    # 2 class vars + 17 statistics for both variables + sum_wgt
    variable_count <- 2 + (2 * 17) + 1 + 3
    expect_equal(ncol(result_df), variable_count)
})


test_that("Specifying only one statistic puts out variable names without extension", {
    result_df1 <- dummy_df |>
        summarise_plus(class      = c(year, sex),
                       values     = weight,
                       statistics = "sum")

    # sum_wgt doesn't count because it only creates one variable detached from the input variables.
    result_df2 <- dummy_df |>
        summarise_plus(class      = c(year, sex),
                       values     = weight,
                       statistics = c("mean", "sum_wgt"))

    expect_equal(names(result_df1), c("year", "sex", "TYPE", "TYPE_NR", "DEPTH", "weight_sum"))
    expect_equal(names(result_df2), c("year", "sex", "TYPE", "TYPE_NR", "DEPTH", "weight_mean", "sum_wgt"))
})


test_that("None existent statistics will be omitted", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(year, sex),
                       values     = income,
                       statistics = c("sum", "test"),
                       weight     = weight)

    expect_equal(ncol(result_df), 6)
})


test_that("Merging variables back to original data frame creates new column", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(year, sex),
                       values     = c(income),
                       statistics = "sum",
                       weight     = weight,
                       merge_back = TRUE)

    expect_equal(ncol(result_df), ncol(dummy_df) + 1)
    expect_equal(nrow(result_df), nrow(dummy_df))
})


test_that("Don't nest class variables but compute them separately and fuse variables into one super variable", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(year, sex),
                       values     = c(income),
                       statistics = "sum",
                       weight     = weight,
                       nesting    = "single")

    expect_equal(ncol(result_df), 5)

    expect_true(all(c("TYPE", "TYPE_NR", "DEPTH") %in% names(result_df)))

    expect_equal(max(result_df[["DEPTH"]]), 1)
})


test_that("Drop auto generated variables after summarise", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(year, sex),
                       values     = c(income),
                       statistics = "sum",
                       weight     = weight,
                       nesting    = "single") |>
        drop_type_vars()

    expect_equal(ncol(result_df), 2)

    expect_true(!all(c("TYPE", "TYPE_NR", "DEPTH") %in% names(result_df)))
})


test_that("Generate all possible combinations of class variables", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(year, sex, age, education),
                       values     = c(income),
                       statistics = c("sum", "mean", "pct_group", "pct_total"),
                       weight     = weight,
                       nesting    = "all")

    expect_equal(ncol(result_df), 11)

    expect_true(all(c("TYPE", "TYPE_NR", "DEPTH") %in% names(result_df)))

    expect_equal(min(result_df[["DEPTH"]]), 0)
    expect_equal(max(result_df[["DEPTH"]]), 4)

    expect_true(is.na(result_df[1, "year"]) & is.na(result_df[1, "sex"]) & is.na(result_df[1, "age"]) & is.na(result_df[1, "education"]))
})


test_that("Generate only chosen combinations of class variables", {
    result_df <- dummy_df |>
        summarise_plus(class      = c(year, sex, age),
                       values     = c(income),
                       statistics = c("sum", "mean", "pct_group", "pct_total"),
                       weight     = weight,
                       type       = c("total", "year + sex", "sex + age", "age"),
                       nesting    = "all")

    expect_equal(min(result_df[["DEPTH"]]), 0)
    expect_equal(max(result_df[["DEPTH"]]), 2)

    expect_equal(length(unique(result_df[["TYPE"]])), 4)
    expect_true("year+sex" %in% result_df[["TYPE"]])
    expect_true("sex+age" %in% result_df[["TYPE"]])
    expect_true("age" %in% result_df[["TYPE"]])
    expect_true("total" %in% result_df[["TYPE"]])
})


test_that("Summarise possible with empty class vector", {
    result_df <- dummy_df |>
        summarise_plus(class  = c(),
                       values = income,
                       statistics = "sum")

    expect_equal(ncol(result_df), 4)
    expect_equal(nrow(result_df), 1)
})


test_that("Summarise possible with no class variables provided", {
    result_df <- dummy_df |>
        summarise_plus(values = income,
                       statistics = "sum")

    expect_equal(ncol(result_df), 4)
    expect_equal(nrow(result_df), 1)
})

###############################################################################
# Format checks
###############################################################################

test_that("Apply single discrete labels", {
    sex. <- suppressMessages(discrete_format(
        "Male"   = 1,
        "Female" = 2))
    age. <- suppressMessages(discrete_format(
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

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
                 collapse::fsum(no_format_df[["income"]]))
    expect_true(all(c("Male", "Female") %in% format_df[["sex"]]))
    expect_true(all(c("under 18",
                      "18 to under 25",
                      "25 to under 55",
                      "55 to under 65",
                      "65 and older") %in% format_df[["age"]]))
})


test_that("Apply discrete multilabels", {
    sex. <- suppressMessages(discrete_format(
        "Total"  = 1:2,
        "Male"   = 1,
        "Female" = 2))
    age. <- suppressMessages(discrete_format(
        "Total"          = 0:100,
        "under 18"       = 0:17,
        "18 to under 25" = 18:24,
        "25 to under 55" = 25:54,
        "55 to under 65" = 55:64,
        "65 and older"   = 65:100))

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
                 collapse::fsum(no_format_df[["weight"]]) * 4)
    expect_true(all(c("Total", "Male", "Female") %in% format_df[["sex"]]))
    expect_true(all(c("Total",
                      "under 18",
                      "18 to under 25",
                      "25 to under 55",
                      "55 to under 65",
                      "65 and older") %in% format_df[["age"]]))
})


test_that("Apply single interval label", {
    income. <- suppressMessages(interval_format(
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

    format_df <- dummy_df |>
        summarise_plus(class      = c(income),
                       values     = weight,
                       statistics = "sum",
                       formats    = list(income = income.),
                       nesting    = "deepest") |>
        remove_stat_extension("sum")

    expect_equal(collapse::fsum(format_df[["weight"]]),
                 collapse::fsum(dummy_df[["weight"]]))
    expect_true(all(c("below 500",
                      "500 to under 1000",
                      "1000 to under 2000",
                      "2000 and more") %in% format_df[["income"]]))
})


test_that("Apply interval multilabel", {
    income. <- suppressMessages(interval_format(
        "Total"              = 0:99999,
        "below 500"          = 0:499,
        "500 to under 1000"  = 500:999,
        "1000 to under 2000" = 1000:1999,
        "2000 and more"      = 2000:99999))

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
                 collapse::fsum(no_format_df[["weight"]]) * 2)
    expect_true(all(c("Total",
                      "below 500",
                      "500 to under 1000",
                      "1000 to under 2000",
                      "2000 and more") %in% format_df[["income"]]))
})

###############################################################################
# Warning checks
###############################################################################


test_that("Entering none existing variable as weight leads to unweighted results", {
    result_df1 <- suppressMessages(dummy_df |>
                               summarise_plus(class  = year,
                                              values = income))

    expect_message(result_df2 <- dummy_df |>
               summarise_plus(class  = year,
                              values = income,
                              weight = abc), " ! WARNING: Provided weight variable is not part of the data frame")

    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df1)))
    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df2)))

    expect_identical(result_df1, result_df2)
})


test_that("Entering none numeric variable as weight leads to unweighted results", {
    result_df1 <- suppressMessages(dummy_df |>
                               summarise_plus(class  = year,
                                              values = income))

    expect_message(result_df2 <- dummy_df |>
               summarise_plus(class  = year,
                              values = income,
                              weight = education), " ! WARNING: Provided weight variable is not numeric")

    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df1)))
    expect_true(all(c("year", "income_sum", "income_freq") %in% names(result_df2)))

    expect_identical(result_df1, result_df2)
})


test_that("Percentiles won't be calculated if value variable has NA values", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex),
                              values     = c(income, probability),
                              statistics = c("p1", "p99"),
                              weight     = weight), " ! WARNING: To calculate percentiles there may be no NAs in the value variables")

    expect_equal(ncol(result_df), 5)
})


test_that("Percentiles above 100 not allowed", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex),
                              values     = c(income, probability),
                              statistics = c("p101"),
                              weight     = weight), " ! WARNING: Percentiles are only possible from p0 to p100")

    expect_equal(ncol(result_df), 7)
})


test_that("None existent class variables will be omitted", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex, test),
                              values     = c(income, probability),
                              statistics = "sum",
                              weight     = weight), "This variable will be omitted during computation")

    expect_equal(ncol(result_df), 7)
})


test_that("None existent analysis variable will be omitted", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex),
                              values     = c(income, probability, test),
                              statistics = "sum",
                              weight     = weight), "This variable will be omitted during computation")

    expect_equal(ncol(result_df), 7)
})


test_that("Double class variables will be omitted", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex, sex),
                              values     = c(income, probability),
                              statistics = "sum",
                              weight     = weight), " ! WARNING: Some <class> variables are provided more than once")

    expect_equal(ncol(result_df), 7)
})


test_that("Double analysis variables will be omitted", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex),
                              values     = c(income, probability, income),
                              statistics = "sum",
                              weight     = weight), " ! WARNING: Some <values> variables are provided more than once")

    expect_equal(ncol(result_df), 7)
})


test_that("Analysis variable will be omitted if also passed as class variable", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex, age),
                              values     = c(age, probability),
                              statistics = "sum",
                              weight     = weight), "This variable will be omitted as <values> variable during computation")

    expect_equal(ncol(result_df), 7)
})


test_that("Merging variables back works if wrong nesting option ist provided", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = c(year, sex),
                              values     = c(income),
                              statistics = "sum",
                              weight     = weight,
                              nesting    = "all",
                              merge_back = TRUE), " ! WARNING: Merging variables back only works with nesting = 'deepest'")

    expect_equal(ncol(result_df), ncol(dummy_df) + 1)
    expect_equal(nrow(result_df), nrow(dummy_df))
})


test_that("Drop auto generated variables will be omitted if not in data frame", {
    expect_message(result_df <- dummy_df |>
               drop_type_vars(), " ! WARNING: The provided variable to drop")
})


test_that("Invalid statistic will be omitted", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = year,
                              values     = income,
                              statistics = c("test", "sum")), " ! WARNING: <Statistic> 'test' is invalid and will be omitted.")

    expect_equal(ncol(result_df), 5)
})


test_that("'Invalid statistic will be omitted 'sum' will be chosen as statistic if no valid one is provided", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(class      = year,
                              values     = income,
                              statistics = "test"), " ! WARNING: No valid <statistic> selected. 'sum' will be used.")

    expect_equal(ncol(result_df), 5)
})

###############################################################################
# Abort checks
###############################################################################


test_that("Summarise errors when no analysis variable is provided", {
    expect_message(result_df <- dummy_df |>
               summarise_plus(statistics = "sum"), " X ERROR: No <values> provided")

    expect_equal(result_df, NULL)
})
