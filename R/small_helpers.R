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
        return(data_frame)
    }

    # Convert to character vectors
    fuse_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(variables_to_fuse))))

    if (fuse_temp == "group_vars"){
    }
    else if (!is_error(fuse_temp)){
        # Do nothing. In this case values already contains the substituted variable names
        # while values_temp is evaluated to the symbol passed into the function.
    }
    else if (substr(fuse_temp, 1, 2) == "c("){
        variables_to_fuse  <- as.character(substitute(variables_to_fuse))[-1]
    }
    else{
        variables_to_fuse <- fuse_temp
    }

    # Make sure that the variables provided are part of the data frame.
    provided_fuse     <- variables_to_fuse
    invalid_fuse      <- variables_to_fuse[!variables_to_fuse %in% names(data_frame)]
    variables_to_fuse <- variables_to_fuse[variables_to_fuse %in% names(data_frame)]

    if (length(invalid_fuse) > 0){
        message(" ! WARNING: The provided variable to fuse '", paste(invalid_fuse, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted during computation.")
    }

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

#' Get Variable Names which are not Part of the Given Vector
#'
#' @description
#' If you have stored variable names inside a character vector, this function gives you
#' the inverse variable name vector.
#'
#' @param data_frame The data frame from which to take the variable names.
#' @param var_names A character vector of variable names.
#'
#' @return
#' Returns the inverse vector of variable names compared to the given vector.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Get variable names
#' var_names <- c("year", "age", "sex")
#' other_names <- my_data |> inverse(var_names)
#'
#' # Can also be used to just get all variable names
#' all_names <- my_data |> inverse(NULL)
#' all_names <- my_data |> inverse(character(0))
#'
#'
#' @export
inverse <- function(data_frame, var_names){
    names(data_frame)[!names(data_frame) %in% var_names]
}


#' Compute Running Numbers
#'
#' @description
#' Compute running numbers in a data frame. Without specifying a by variable
#' results in the row number. With by variable computes the running number within
#' each group of expressions.
#'
#' @param data_frame The data frame in which to compute the running number.
#' @param var_name The variable name of the running number.
#' @param by By group in which to compute the running number per expression.
#'
#' @return
#' Returns the data frame with a new variable containing a running number.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Get row numbers
#' my_data <- my_data |> running_number()
#' my_data <- my_data |> running_number("row_number")
#'
#' # Running number per variable expression
#' my_data <- my_data |> running_number(by = year)
#'
#' @export
running_number <- function(data_frame,
                           var_name = "run_nr",
                           by       = NULL){
    # Convert to character vectors
    by_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(by))))

    if (substr(by_temp, 1, 2) == "c("){
        by <- as.character(substitute(by))
    }
    else if (!is_error(by)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while variables_temp is evaluated to the symbol passed into the function.
    }
    else{
        by <- by_temp
    }

    # Remove extra first character created with substitution
    by <- by[by != "c"]

    # In case of a by variable
    if (length(by) == 1){
        data_frame[[var_name]] <- stats::ave(seq_len(nrow(data_frame)),
                                             data.table::rleid(data_frame[[by]]),
                                             FUN = seq_along)
    }
    # In case of no by variable
    else{
        data_frame[[var_name]] <- seq_len(nrow(data_frame))
    }

    data_frame
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

    ordered_cols <- unique(ordered_cols)

    # Put the ordered columns at the end of the data frame
    data_frame |> data.table::setcolorder(ordered_cols, after = ncol(data_frame))
}


#' Replace Patterns Inside Variable Names
#'
#' @description
#' Replace a certain pattern inside a variable name with a new one. This can be
#' used if there are multiple different variable names which have a pattern in
#' common (e.g. all end in "_sum" but start different), so that there don't have
#' to be multiple rename variable calls.
#'
#' @param data_frame The data frame in which there are variables to be renamed.
#' @param old_pattern The pattern which should be replaced in the variable names.
#' @param new_pattern The pattern which should be set in place for the old one.
#'
#' @return
#' Returns a data frame with renamed variables.
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
#' # Rename variables by repacing patterns
#' new_names <- all_nested |>
#'     rename_pattern("pct", "percent") |>
#'     rename_pattern("_sum", "")
#'
#' @export
rename_pattern <- function(data_frame, old_pattern, new_pattern){
    if (length(old_pattern) > 1 | length(new_pattern) > 1){
        message(" X ERROR: Only single pattern allowed. Rename pattern will be aborted.")
        return(data_frame)
    }

    # Replace old_pattern with new_pattern in all column names
    new_names <- gsub(old_pattern, new_pattern, names(data_frame))
    names(data_frame) <- new_names

    data_frame
}


#' Replace Statistic From Variable Names
#'
#' @description
#' Remove the statistic name from variable names, so that they get back their old
#' names without extension.
#'
#' @param data_frame The data frame in which there are variables to be renamed.
#' @param statistics Statistic extensions that should be removed from the variable names.
#'
#' @return
#' Returns a data frame with renamed variables.
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
#' # Remove statistic extension
#' new_names <- all_nested |> remove_stat_extension("sum")
#'
#' @export
remove_stat_extension <- function(data_frame, statistics){
    var_names <- names(data_frame)

    # Remove statistic extensions
    for (stat in statistics){
        new_names <- sub(paste0("_", stat, "$"), "", var_names)
    }

    # Check if unique new names still have the same length as the original names.
    # Only if this is true the new names can be applied.
    if (length(var_names) == length(unique(new_names))){
        names(data_frame) <- new_names
    }
    # If there are duplicate names abort
    else{
        message(" ! WARNING: New variable names are not unique. Statistic extensions won't be removed.")
    }

    data_frame
}


#' Add Extensions to Variable Names
#'
#' @description
#' Renames variables in a data frame by adding the desired extensions to the original names.
#' This can be useful if you want to use pre summarised data with [any_table()], which needs
#' the value variables to have the statistic extensions.
#'
#' @param data_frame The data frame in which variables should gain extensions to their name.
#' @param from The position of the variable inside the data frame at which to start the renaming.
#' @param extensions The extensions to add.
#' @param reuse "none" by default, meaning only the provided extensions will be set. E.g. if
#' there are two extensions provided, two variables will be renamed. If "last", the last provided
#' extension will be used for every following variable until the end of the data frame. If "repeat",
#' the provided extensions will be repeated from the first one for every following variable until
#' the end of the data frame.
#'
#' @return
#' Returns a data frame with extended variable names.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Add extensions to variable names
#' new_names1 <- my_data |> add_extension(5, c("sum", "pct"))
#' new_names2 <- my_data |> add_extension(5, c("sum", "pct"), reuse = "last")
#' new_names3 <- my_data |> add_extension(5, c("sum", "pct"), reuse = "alternate")
#'
#' @export
add_extension <- function(data_frame,
                          from,
                          extensions,
                          reuse = "none"){
    if (!is.numeric(from)){
        message(" X ERROR: From needs to be numeric. Adding extensions will be aborted.")
        return(data_frame)
    }

    if (!is.character(extensions)){
        message(" X ERROR: Extensions need to be characters. Adding extensions will be aborted.")
        return(data_frame)
    }

    if (from > ncol(data_frame)){
        message(" X ERROR: From is greater than number of columns in data frame. Adding extensions will be aborted.")
        return(data_frame)
    }

    if (!reuse %in% c("none", "last", "repeat")){
        message(" ! WARNING: Reuse must be one of 'none', 'last', 'repeat'. 'none' will be used.")
        reuse <- "none"
    }

    var_names <- names(data_frame)

    # Columns to modify
    target_columns <- from:length(var_names)
    n_target       <- length(target_columns)
    n_extensions   <- length(extensions)

    # generally shorten extensions, if there are more than columns are left in the data frame
    extensions <- extensions[seq_len(min(n_extensions, n_target))]

    # Create the extended names
    if (reuse == "last" & n_target - n_extensions > 0){
        # Repeat the last extension for the remaining columns
        extensions <- c(extensions, rep(extensions[n_extensions], n_target - n_extensions))
    }
    else if(reuse == "none"){
        # Set new last column depending on number of extensions
        last_column    <- from + (length(extensions) - 1)
        target_columns <- from:last_column
    }

    # Apply extensions to variable names
    names(data_frame)[target_columns] <- paste0(var_names[target_columns], "_", extensions)

    data_frame
}
