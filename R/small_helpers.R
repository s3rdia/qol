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
        message(" X ERROR: No vector allowed. Only one new variable can be generated.")
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
        message(" X ERROR: Variable range has to be provided in the form 'var_name1:var_name10'.\n",
                "          Variable names must match. No variables will be added.")
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
        message(" X ERROR: Some variables are already part of the data frame: ", paste(invalid_variables, collapse = ", "), "\n",
                "          No variables will be added.")
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
        message(" X ERROR: Only numeric values allowed.")

        return(invisible(NA_integer_))
    }

    if (!is.integer(variable)){
        message(" ! WARNING: Variable is not an integer and will be floored. Decimal places won't count.")

        variable <- floor(variable)
    }

    nchar(abs(variable))
}
