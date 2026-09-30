set_no_print(TRUE)

###############################################################################
# Suppressing some functions messages because they only output the information
# on how much time they took.
###############################################################################

edge_case_df <- data.frame(x = c(1, 1999, 2000, 2001, 3000),
                           w = c(1, 1999, 2000, 2001, 3000))

# Create single discrete label
sex. <- discrete_format(
        "Male"   = 1,
        "Female" = 2)

expect_true(all(c("value", "label") %in% names(sex.)), info = "Create single discrete label")
expect_equal(nrow(sex.), 2, info = "Create single discrete label")
expect_equal(ncol(sex.), 2, info = "Create single discrete label")


# Create discrete multilabel
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

expect_true(all(c("value", "label") %in% names(sex.)), info = "Create discrete multilabel")
expect_equal(nrow(sex.), 4, info = "Create discrete multilabel")
expect_equal(ncol(sex.), 2, info = "Create discrete multilabel")


# Create single interval label
income. <- interval_format(
    "below 500"          =    0:500,
    "500 to under 1000"  =  500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

expect_true(all(c("from", "to", "label") %in% names(income.)), info = "Create single interval label")
expect_equal(nrow(income.), 4, info = "Create single interval label")
expect_equal(ncol(income.), 3, info = "Create single interval label")


# Create interval multilabel
income. <- interval_format(
    "Total"              =    0:100000,
    "below 500"          =    0:500,
    "500 to under 1000"  =  500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

expect_true(all(c("from", "to", "label") %in% names(income.)), info = "Create interval multilabel")
expect_equal(nrow(income.), 5, info = "Create interval multilabel")
expect_equal(ncol(income.), 3, info = "Create interval multilabel")


# Create interval format with low and high keywords
income. <- interval_format(
    "Total"              = c("low", "high"),
    "below 500"          = c("low", 500),
    "500 to under 1000"  = 500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = c(2000, "high"))

expect_true(all(c("from", "to", "label") %in% names(income.)), info = "Create interval format with low and high keywords")
expect_equal(income.[["from"]][1], -Inf, info = "Create interval format with low and high keywords")
expect_equal(income.[["to"]][1],    Inf, info = "Create interval format with low and high keywords")


# Expand formats
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

income. <- interval_format(
    "below 500"          =    0:500,
    "500 to under 1000"  =  500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

expand_df <- expand_formats(sex., income.,
                            names = c("sex", "income"))

expect_true(all(c("sex", "income") %in% names(expand_df)), info = "Expand formats")
expect_equal(collapse::fncol(expand_df), 2, info = "Expand formats")
expect_equal(collapse::fnrow(expand_df), 12, info = "Expand formats")


# Expand formats provided as list
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

income. <- interval_format(
    "below 500"          =    0:500,
    "500 to under 1000"  =  500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

expand_df <- expand_formats(list(sex., income.), names = c("sex", "income"))

expect_true(all(c("sex", "income") %in% names(expand_df)), info = "Expand formats provided as list")
expect_equal(collapse::fncol(expand_df), 2, info = "Expand formats provided as list")
expect_equal(collapse::fnrow(expand_df), 12, info = "Expand formats provided as list")


# Expand formats returns unique label values if only one format is provided
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

expand_df <- expand_formats(sex.)

expect_equal(expand_df[[1]], c("Total", "Male", "Female"), info = "Expand formats returns unique label values if only one format is provided")


# Interval formats including lower bounds and excluding upper bounds
age. <- interval_format(
    "under 65"    = c(0,  65),
    "65 und mehr" = c(65, 100))

expect_true(age.[1, "from"] ==  0 && age.[1, "to"] <  65, info = "Interval formats including lower bounds and excluding upper bounds")
expect_true(age.[2, "from"] == 65 && age.[2, "to"] < 100, info = "Interval formats including lower bounds and excluding upper bounds")


# Interval formats including upper bounds and excluding lower bounds
age. <- interval_format(
    "under 65"    = c(0,  65),
    "65 und mehr" = c(65, 100),
    include_lower = FALSE,
    include_upper = TRUE)

expect_true(age.[1, "from"] >  0 && age.[1, "to"] ==  65, info = "Interval formats including upper bounds and excluding lower bounds")
expect_true(age.[2, "from"] > 65 && age.[2, "to"] == 100, info = "Interval formats including upper bounds and excluding lower bounds")


# Edge cases in appplied interval formats fall into the correct category
include. <- interval_format(
    "A" = 0001:2000,
    "B" = 2000:3000,
    include_lower = TRUE,
    include_upper = FALSE)

edge_case_result <- edge_case_df |>
    summarise_plus(class      = x,
                   values     = w,
                   statistics = "sum",
                   formats    = list(x = include.),
                   nesting    = "deepest",
                   na.rm      = TRUE)

edge_case_result[["x"]] <- as.character(edge_case_result[["x"]])

expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "A"], 2000, info = "Edge cases in appplied interval formats fall into the correct category")
expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "B"], 4001, info = "Edge cases in appplied interval formats fall into the correct category")

include. <- interval_format(
    "A" = 0001:2000,
    "B" = 2000:3000,
    include_lower = TRUE,
    include_upper = TRUE)

edge_case_result <- edge_case_df |>
    summarise_plus(class      = x,
                   values     = w,
                   statistics = "sum",
                   formats    = list(x = include.),
                   nesting    = "deepest",
                   na.rm      = TRUE)

edge_case_result[["x"]] <- as.character(edge_case_result[["x"]])

expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "A"], 4000, info = "Edge cases in appplied interval formats fall into the correct category")
expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "B"], 7001, info = "Edge cases in appplied interval formats fall into the correct category")

include. <- interval_format(
    "A" = 0001:2000,
    "B" = 2000:3000,
    include_lower = FALSE,
    include_upper = FALSE)

edge_case_result <- edge_case_df |>
    summarise_plus(class      = x,
                   values     = w,
                   statistics = "sum",
                   formats    = list(x = include.),
                   nesting    = "deepest",
                   na.rm      = TRUE)

edge_case_result[["x"]] <- as.character(edge_case_result[["x"]])

expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "A"], 1999, info = "Edge cases in appplied interval formats fall into the correct category")
expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "B"], 2001, info = "Edge cases in appplied interval formats fall into the correct category")

include. <- interval_format(
    "A" = 0001:2000,
    "B" = 2000:3000,
    include_lower = FALSE,
    include_upper = TRUE)

edge_case_result <- edge_case_df |>
    summarise_plus(class      = x,
                   values     = w,
                   statistics = "sum",
                   formats    = list(x = include.),
                   nesting    = "deepest",
                   na.rm      = TRUE)

edge_case_result[["x"]] <- as.character(edge_case_result[["x"]])

expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "A"], 3999, info = "Edge cases in appplied interval formats fall into the correct category")
expect_equal(edge_case_result[["w_sum"]][edge_case_result[["x"]] == "B"], 5001, info = "Edge cases in appplied interval formats fall into the correct category")


# Formats convert numeric labels to numeric by default
leading_zeros <- discrete_format("00110" = 1, "00120" = 2)

expect_equal(leading_zeros[["label"]], c(110, 120), info = "Formats convert numeric labels to numeric by default")
expect_equal(interval_format("00110" = 1, "00120" = 2:3)[["label"]], c(110, 120),
             info = "Interval formats convert numeric labels to numeric by default")


# Formats keep labels as character, if as_character is TRUE
leading_zeros_char <- discrete_format("00110" = 1, "00120" = 2, as_character = TRUE)

expect_equal(leading_zeros_char[["label"]], c("00110", "00120"), info = "Discrete formats keep labels as character with as_character")
expect_true(is.character(leading_zeros_char[["label"]]), info = "Discrete formats keep labels as character with as_character")

interval_zeros_char <- interval_format("00110" = 1, "00120" = 2:3, as_character = TRUE)

expect_equal(interval_zeros_char[["label"]], c("00110", "00120"), info = "Interval formats keep labels as character with as_character")
expect_true(is.character(interval_zeros_char[["label"]]), info = "Interval formats keep labels as character with as_character")


# Keywords still work when labels are kept as character
expect_equal(discrete_format("00110" = 1, "99999" = "other", as_character = TRUE)[["label"]],
             c("00110", "99999"), info = "Other keyword works with as_character")
expect_true(interval_format("00110" = c("low", 1), "99999" = c(2, "high"), as_character = TRUE)[["label"]][1] == "00110",
            info = "Low and high keywords work with as_character")


# Leading zeros are kept when formats are applied with recode_multi
zeros_df <- data.frame(codes = c(1, 2, 3, 1))

recode_result <- zeros_df |> recode_multi(codes = leading_zeros_char)
recode_default <- zeros_df |> recode_multi(codes = leading_zeros)

expect_equal(as.character(recode_result[["codes"]]), c("00110", "00120", "3", "00110"),
             info = "Leading zeros are kept when applying formats with recode_multi")
expect_equal(as.character(recode_default[["codes"]])[1], "110",
             info = "Leading zeros are lost by default when applying formats with recode_multi")


# Leading zeros are kept when formats are applied with summarise_plus
summarise_result <- zeros_df |>
    summarise_plus(class      = codes,
                   values     = codes,
                   statistics = "freq",
                   formats    = list(codes = leading_zeros_char),
                   nesting    = "deepest")

expect_true("00110" %in% as.character(summarise_result[["codes"]]),
            info = "Leading zeros are kept when applying formats with summarise_plus")
expect_false("110" %in% as.character(summarise_result[["codes"]]),
             info = "Leading zeros are kept when applying formats with summarise_plus")

###############################################################################
# Abort checks
###############################################################################

# Abort format expansion, if a data frame has no label column
sex. <- discrete_format(
    "Total"  = 1:2,
    "Male"   = 1,
    "Female" = 2)

sex. <- sex. |> dropp("label")

income. <- interval_format(
    "below 500"          =    0:500,
    "500 to under 1000"  =  500:1000,
    "1000 to under 2000" = 1000:2000,
    "2000 and more"      = 2000:100000)

expand_df <- expand_formats(sex., income., names = c("sex", "income"))

expect_error(print_stack_as_messages("ERROR"), "A data frame is missing the 'label' column. This function is especially for expanding formats created", info = "Abort format expansion, if a data frame has no label column")


# Create single discrete label aborts, if elements not provided in the correct way
discrete_format(test == 1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single discrete label aborts, if elements not provided in the correct way")


# Create single discrete label aborts, if list element is missing a name
discrete_format(1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single discrete label aborts, if list element is missing a name")


# Create single interval label aborts, if elements not provided in the correct way
interval_format(test == 1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single interval label aborts, if elements not provided in the correct way")


# Create single interval label aborts, if list element is missing a name
discrete_format(1)

expect_error(print_stack_as_messages("ERROR"), "Formats must be provided in the form.", info = "Create single interval label aborts, if list element is missing a name")


set_no_print()
