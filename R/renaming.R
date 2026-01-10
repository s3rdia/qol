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
    if (length(old_pattern) > 1 || length(new_pattern) > 1){
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
    if (length(var_names) == length(collapse::funique(new_names))){
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
#' my_data <- dummy_data(10)
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
    if (reuse == "last" && n_target - n_extensions > 0){
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


#' Replace Patterns While Protecting Exceptions
#'
#' @description
#' Replaces a provided pattern with another, while protecting exceptions. Exceptions can
#' contain the given pattern, but won't be changed during replacement.
#'
#' @param vector A vector containing the texts, where a pattern should be replaced.
#' @param pattern The pattern that should be replaced.
#' @param replacement The new pattern, which replaces the old one.
#' @param exceptions A character vector containing exceptions, which should not be altered.
#'
#' @return
#' Returns a vector with replaced pattern.
#'
#' @examples
#' # Vector, where underscores should be replaced
#' underscores <- c("my_variable", "var_with_underscores", "var_sum", "var_pct_total")
#'
#' # Extensions, where underscores shouldn't be replaced
#' extensions <- c("_sum", "_pct_group", "_pct_total", "_pct_value", "_pct", "_freq_g0",
#'                 "_freq", "_mean", "_median", "_mode", "_min", "_max", "_first",
#'                 "_last", "_p1", "_p2", "_p3", "_p4", "_p5", "_p6", "_p7", "_p8", "_p9",
#'                 "sum_wgt", "_sd", "_variance", "_missing")
#'
#' # Replace
#' new_vector <- underscores |> replace_except("_", ".", extensions)
#'
#' @export
replace_except <- function(vector,
                           pattern,
                           replacement,
                           exceptions = NULL){
    # Replace the pattern in the exceptions with a pseudo symbol
    except_replace <- gsub(pattern, "&%!", exceptions)

    # Protect exceptions in original vector
    for (element in seq_along(vector)){
        for (exception in seq_along(exceptions)){
            vector[[element]] <- gsub(exceptions[[exception]],
                                      except_replace[[exception]],
                                      vector[[element]])
        }
    }

    # Replace pattern safely
    vector <- gsub(pattern, replacement, vector)

    # Reestablish protected pattern
    gsub("&%!", pattern, vector)
}


#' Rename One Or More Variables
#'
#' @description
#' Can rename one or more existing variable names into the corresponding new variable
#' names in one go.
#'
#' @param data_frame The data frame which contains the variable names to be renamed.
#' @param ... Pass in variables to be renamed in the form: "old_var" = "new_var".
#'
#' @return
#' Returns a data_frame with renamed variables.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(10)
#'
#' # Rename multiple variables at once
#' new_names_df <- my_data |> rename_multi("sex"   = "var1",
#'                                         "age"   = "var2",
#'                                         "state" = "var3")
#'
#' @export
rename_multi <- function(data_frame, ...){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into a list if possible
    rename_list <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(rename_list)){
        message('X ERROR: Unknown object found. Provide variables in quotation marks, like: "old_var" = "new_var".\n",
                "         Renaming will be aborted.')
        return(data_frame)
    }

    # Get old and new names in separate vectors to rename them in one go
    old_names <- names(rename_list)
    new_names <- unlist(rename_list, use.names = FALSE)

    # Make sure that the variables provided are part of the data frame.
    old_names <- data_frame |> part_of_df(old_names, check_only = TRUE)

    if (is.list(old_names)){
        message(" X ERROR: The provided <old name> '", paste(old_names[[1]], collapse = ", "), "' is not part of\n",
                "          the data frame. Renaming will be aborted.")
        return(data_frame)
    }

    # If any of the new variable names is already part of the data frame abort
    invalid_new_names <- new_names[new_names %in% names(data_frame)]

    # If not all old names are part of the data frame abort
    if (length(invalid_new_names) > 0){
        message(" X ERROR: The provided <new name> '", paste(invalid_new_names, collapse = ", "), "' is already part of\n",
                "          the data frame. Renaming will be aborted.")
        return(data_frame)
    }

    # Rename all variables in one go
    data_frame <- data_frame |> collapse::frename(stats::setNames(old_names, new_names))

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'rename_multi' execution time: ", end_time, " seconds")

    data_frame
}


#' Set First Data Frame Row As Variable Names
#'
#' @description
#' Sets the first row of a data frame as variable names and deletes it. In case
#' of NA, numeric values or empty characters in the first row, the old names are kept.
#'
#' @param data_frame A data frame for which to set new variable names.
#'
#' @return
#' Returns a data frame with renamed variables.
#'
#' @examples
#' # Example data frame
#' my_data <- data.frame(
#'               var1 = c("id", 1, 2, 3),
#'               var2 = c(NA, "a", "b", "c"),
#'               var3 = c("value", 1, 2, 3),
#'               var4 = c("", "a", "b", "c"),
#'               var5 = c(1, 2, 3, 4))
#'
#' my_data <- my_data |> first_row_as_names()
#'
#' @export
first_row_as_names <- function(data_frame) {
    # Extract first row and current names
    new_names <- as.character(data_frame[1, ])
    old_names <- names(data_frame)

    # Set up condition on when to keep the old names
    keep_old <- is.na(new_names) |
                new_names == "" |
                !is.na(suppressWarnings(as.numeric(new_names)))

    # Rename conditionally
    names(data_frame) <- data.table::fifelse(keep_old, old_names, new_names)

    # Delete first row and return
    data_frame[-1, , drop = FALSE]
}
