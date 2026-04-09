#' Drop automatically generated Variables
#'
#' @description
#' If [summarise_plus()] is used with the nested options "all" or "single", three
#' variables are automatically generated: TYPE, TYPE_NR and DEPTH. With this functions
#' these variables are dropped.
#'
#' @param data_frame The data frame with automatically generated variables.
#'
#' @return
#' Returns a data frame without the variables TYPE, TYPE_NR and DEPTH.
#'
#' @examples
#' # Example format
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' all_possible <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(income, probability),
#'                    statistics = c("sum", "mean", "freq"),
#'                    formats    = list(sex = "sex."),
#'                    weight     = weight,
#'                    nesting    = "all",
#'                    na.rm      = TRUE) |>
#'     drop_type_vars()
#'
#' @export
drop_type_vars <- function(data_frame){
    data_frame |> dropp("TYPE", "TYPE_NR", "DEPTH")
}


#' Fuse Multiple Variables
#'
#' @description
#' When you have a situation where you have multiple variables with different NA
#' values that happen to be in different places (where one variable has a value the
#' other is NA and vice versa) you can fuse these together to a single variable.
#'
#' @param data_frame A data frame with variables to fuse.
#' @param new_variable_name The name of the new fused variable.
#' @param variables_to_fuse A vector with the variables that should be fused together.
#' @param drop_original_vars Whether to drop or keep the original values. TRUE by default.
#'
#' @return
#' Returns a data frame without the variables TYPE, TYPE_NR and DEPTH.
#'
#' @examples
#' # Example format
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' all_possible <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(income, probability),
#'                    statistics = c("sum", "mean", "freq"),
#'                    formats    = list(sex = "sex."),
#'                    weight     = weight,
#'                    nesting    = "all",
#'                    na.rm      = TRUE)
#'
#' all_possible <- all_possible[DEPTH <= 1] |>
#'     fuse_variables("fusion", c("year", "sex"))
#'
#' # NOTE: You can generally use this function to fuse variables. What is done in
#' #       multiple steps above can be achieved by just using nested = "single" in
#' #       summarise_plus.
#' single <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(income, probability),
#'                    statistics = c("sum", "mean", "freq"),
#'                    formats    = list(sex = "sex."),
#'                    weight     = weight,
#'                    nesting    = "single",
#'                    na.rm      = TRUE)
#'
#' @export
fuse_variables <- function(data_frame,
                           new_variable_name,
                           variables_to_fuse,
                           drop_original_vars = TRUE){
    # Convert to character
    new_variable_name <- gsub("\"", "", deparse(substitute(new_variable_name)))

    if (length(new_variable_name) > 1){
        print_message("ERROR", "No vector allowed. Only one new variable can be generated.")
        return(invisible(data_frame))
    }

    # Convert to character vectors
    variables_to_fuse <- get_origin_as_char(variables_to_fuse, substitute(variables_to_fuse))

    # Make sure that the variables provided are part of the data frame.
    variables_to_fuse <- data_frame |> part_of_df(variables_to_fuse)

    # Convert the group columns to character so coalesce works properly
    selected_data   <- data_frame[variables_to_fuse]
    selected_data[] <- lapply(selected_data, as.character)

    # Create a new variable with the first non-NA value from the group columns
    data_frame[[new_variable_name]] <- do.call(data.table::fcoalesce, selected_data)

    if (drop_original_vars){
        data_frame <- data_frame |> dropp(variables_to_fuse)
    }

    data_frame |> data.table::setcolorder(new_variable_name)
}


#' Order Columns by Variable Name Patterns
#'
#' @description
#' Order variables in a data frame based on a pattern rather than whole variable
#' names. E.g. grab every variable that contains "sum" in it's name and order
#' them together so that they appear next to each other.
#'
#' @param data_frame The data frame to be ordered.
#' @param pattern The pattern which is used for ordering the data frame columns.
#'
#' @return
#' Returns a reordered data frame with the ordered variables at the end.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Summarise data
#' all_nested <- my_data |>
#'     summarise_plus(class      = c(year, sex),
#'                    values     = c(weight, income),
#'                    statistics = c("sum", "pct_group", "pct_total", "sum_wgt", "freq"),
#'                    weight     = weight,
#'                    nesting    = "deepest",
#'                    na.rm      = TRUE)
#'
#' # Set a different column order
#' new_order <- all_nested |> setcolorder_by_pattern(c("pct", "freq", "sum"))
#'
#' @export
setcolorder_by_pattern <- function(data_frame, pattern){
    # Copy data frame or else the original input data frame will be altered too
    data_frame <- data.table::copy(data_frame)

    # Select all variables in blocks of the requested pattern
    ordered_cols <- unlist(lapply(pattern, function(single_pattern){
        # Returns all variable names that match the provided pattern
        grep(single_pattern, names(data_frame),
             value = TRUE, ignore.case = TRUE)
    }))

    ordered_cols <- collapse::funique(ordered_cols)

    # Put the ordered columns at the end of the data frame
    data_frame |> data.table::setcolorder(ordered_cols, after = ncol(data_frame))
}


#' Add Empty Variables In A Given Range
#'
#' @description
#' Add empty variables to a data frame in the provided range. Basically does in a
#' data frame, what paste0("age", 1:10) does for a vector.
#'
#' @param data_frame A data frame to add variables to.
#' @param var_range A range of variables to add, provided in the form: var_name1:var_name10.
#'
#' @return
#' Returns a data frame with added variables.
#'
#' @examples
#' # Example data frames
#' my_data <- dummy_data(100)
#'
#' # Add variable range
#' my_data <- my_data |> add_variable_range(status1:status12)
#'
#' @export
add_variable_range <- function(data_frame, var_range){
    var_range <- get_origin_as_char(var_range, substitute(var_range))

    # Using regex to capture prefix, start number, and end number
    # Meaning: (prefix)(start):(prefix)(end)
    pattern <- "^([A-Za-z_.]+)([0-9]+):\\1([0-9]+)$"

    if (!grepl(pattern, var_range)){
        print_message("ERROR", c("Variable range has to be provided in the form 'var_name1:var_name10'.",
								 "Variable names must match. No variables will be added."))
        return(invisible(data_frame))
    }

    # Put together variable names
    prefix <- gsub(pattern, "\\1", var_range)
    start  <- as.numeric(gsub(pattern, "\\2", var_range))
    end    <- as.numeric(gsub(pattern, "\\3", var_range))

    var_names <- paste0(prefix, start:end)

    # If variable names are already part of the data frame, abort
    invalid_variables <- var_names[var_names %in% names(data_frame)]

    if (length(invalid_variables) > 0){
        print_message("ERROR", c("Some variables are already part of the data frame: [invalid].",
								 "No variables will be added."), invalid = invalid_variables)
        return(invisible(data_frame))
    }

    # Create new variables in their own data frame first
    new_columns <- collapse::qDF(matrix(NA_integer_,
                                        nrow = collapse::fnrow(data_frame),
                                        ncol = end))

    names(new_columns) <- paste0(prefix, start:end)

    # Add new empty variables to data frame
    collapse::add_vars(data_frame, new_columns)
}


#' Get Integer Length
#'
#' @description
#' Get the number of digits of an integer variable.
#'
#' @param variable The integer variable from which to get the length.
#'
#' @return
#' Returns a vector with the number of digits places.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' my_data[["age_length"]] <- get_integer_length(my_data[["age"]])
#'
#' @export
get_integer_length <- function(variable){
    if (!is.numeric(variable)){
        print_message("ERROR", "Only numeric values allowed.")

        return(invisible(NA_integer_))
    }

    if (!is.integer(variable)){
        print_message("WARNING", "Variable is not an integer and will be floored. Decimal places won't count.")

        variable <- floor(variable)
    }

    nchar(abs(variable))
}


#' Check For Duplicate Variable Names
#'
#' @name duplicates
#'
#' @description
#' Checks for duplicate variable names in a data frame, e.g. AGE, age and Age.
#'
#' @param data_frame The data frame which variable names to check for duplicates.
#'
#' @return
#' [get_duplicate_var_names()]: Returns a vector of duplicate variable names.
#'
#' @examples
#' # Example data frame
#' my_data <- data.frame(age    = 1,
#'                       AGE    = 2,
#'                       Age    = 3,
#'                       sex    = 1)
#'
#' dup_var_names <- my_data |> get_duplicate_var_names()
#'
#' @rdname duplicates
#'
#' @export
get_duplicate_var_names <- function(data_frame){
    var_names <- names(data_frame)

    duplicate_names <- duplicated(tolower(var_names)) |
        duplicated(tolower(var_names), fromLast = TRUE)

    var_names[duplicate_names]
}


#' @description
#' Counts the number of duplicated variables in a data frame. If a variable
#' appears three times, e.g. AGE, age and Age, the variable count will be one.
#'
#' @return
#' [get_duplicate_var_count()]: Returns the count of duplicated variables as a
#' numeric value.
#'
#' @examples
#' dup_var_count <- my_data |> get_duplicate_var_count()
#'
#' @rdname duplicates
#'
#' @export
get_duplicate_var_count <- function(data_frame){
    duplicate_names <- data_frame |> get_duplicate_var_names()

    unique_names <- collapse::funique(tolower(duplicate_names))

    length(unique_names)
}


#' Round Values With Half Rounded Up And Multiples Of X
#'
#' @name round_values
#'
#' @description
#' This function rounds values according to DIN 1333 (round half up) or as an
#' alternative in multiples of a given value.
#'
#' @param values Numeric values to round.
#' @param digits The number of decimal places the values should be rounded to.
#' @param multiple The multiple to round the values to.
#'
#' @return
#' Returns rounded values.
#'
#' @examples
#' # With vectors
#' round_numbers1 <- round_values(c(-0.5, -0.4, 0.1, 0.49, 0.5, 1.5, 2.5, 3.2))
#' round_numbers2 <- round_values(c(-0.5, -0.49, 0.17, 0.499, 0.51, 1.549, 2.51, 3.25),
#'                                digits = 1)
#' round_numbers3 <- round_values(c(-0.3, -0.24, 1.17, 2.749, 0.25, 1.549, 2.75, 3.25),
#'                                multiple = 0.5)
#'
#' # With a data frame
#' my_data <- dummy_data(100)
#'
#' my_data[["income_round1"]] <- my_data[["income"]] |>  round_values()
#' my_data[["income_round2"]] <- my_data[["income"]] |>  round_values(multiple = 100)
#'
#'
#' @rdname round_values
#'
#' @export
round_values <- function(values,
                         digits   = 0,
                         multiple = NULL){
    if (!is.numeric(values)){
        print_message("ERROR", "Only numeric values allowed. Rounding will be aborted.")

        return(invisible(values))
    }

    eps <- .Machine[["double.eps"]]

    if (!is.null(multiple)){
        # Round in multiples of x
        values <- trunc(abs(values) / multiple + (0.5 + sqrt(eps))) * multiple * sign(values)
    }
    else{
        # Pre calculate the multiplier
        p <- 10 ^ digits

        # Using vectorized math to reduce the number of temporary objects created otherwise
        values <- trunc(abs(values) * p + (0.5 + sqrt(eps))) / p * sign(values)
    }

    values
}


#' @description
#' [round_multi()]: Rounds multiple variables at once inside a data frame.
#'
#' @param data_frame The data frame in which the variables to be rounded are found.
#' @param variables The variable names of which to round the values.
#' @param new_names The new names of the rounded variables. If provided adds variables,
#' if not overwrites the existing variables with rounded values.
#'
#' @return
#' Returns a data frame with rounded values.
#'
#' @examples
#' # Round multiple variables in a data frame
#' my_data <- my_data |>  round_multi(variables = c(income,  expenses,  balance),
#'                                    new_names = c(incomeR, expensesR, balanceR),
#'                                    digits = 1)
#'
#' @rdname round_values
#'
#' @export
round_multi <- function(data_frame,
                        variables,
                        new_names = NULL,
                        digits    = 0,
                        multiple  = NULL){
    variables <- get_origin_as_char(variables, substitute(variables))
    new_names <- get_origin_as_char(new_names, substitute(new_names))

    # Make sure that the variables provided are part of the data frame.
    variables <- data_frame |> part_of_df(variables)

    # If no variables are provided return data frame without rounding
    if (length(variables) == 0){
        return(invisible(data_frame))
    }

    # If no variables are provided return data frame without rounding
    if (!is.null(new_names) && length(variables) != length(new_names)){
        print_message("ERROR", "<Variables> and <new_names> are of unequal length. Rounding will be aborted.")
        return(invisible(data_frame))
    }

    # Expand digits and multiples to variable count, if fewer values than variables
    # are given.
    var_count    <- length(variables)
    digits_count <- length(digits)

    digits <- digits[seq_len(min(digits_count, var_count))]

    if (!is.null(digits) && digits_count < var_count){
        digits <- c(digits, rep(digits[digits_count], var_count - digits_count))
    }

    multiple_count <- length(multiple)

    multiple <- multiple[seq_len(min(multiple_count, var_count))]

    if (!is.null(multiple) && multiple_count < var_count){
        multiple <- c(multiple, rep(multiple[multiple_count], var_count - multiple_count))
    }

    # Loop through all provided variables and check whether they are numeric.
    # Non numeric variables will be excluded.
    invalid_vars <- c()

    for (i in seq_along(variables)){
        variable <- variables[[i]]
        new_name <- new_names[[i]]
        digit    <- digits[[i]]
        multi    <- multiple[[i]]

        # Exclude non numeric variables
        if (!is.numeric(data_frame[[variable]])){
            invalid_vars <- c(invalid_vars, variable)
            variables    <- variables[!variable %in% variables]
        }
        # Actual rounding
        else{
            # Overwrite existing variables if no new names are provided
            if (is.null(new_name)){
                new_name <- variable
            }

            data_frame[[new_name]] <- data_frame[[variable]] |>
                round_values(digits = digit, multiple = multi)
        }
    }

    if (length(invalid_vars)){
        print_message("WARNING", c("Only numeric values allowed. [invalid] [?is/are] not numeric, and will",
                                   "be excluded from rounding."), invalid = invalid_vars)
    }

    data_frame
}
