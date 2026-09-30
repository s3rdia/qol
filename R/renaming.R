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
        print_message("ERROR", "Only single pattern allowed. Rename pattern will be aborted.")
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
    statistics <- get_origin_as_char(statistics, substitute(statistics))

    var_names <- names(data_frame)

    # Remove statistic extensions
    for (stat in statistics){
        var_names <- sub(paste0("_", stat, "$"), "", var_names)
    }

    # Check if unique new names still have the same length as the original names.
    # Only if this is true the new names can be applied.
    if (length(var_names) == length(collapse::funique(var_names))){
        names(data_frame) <- var_names
    }
    # If there are duplicate names abort
    else{
        print_message("WARNING", "New variable names are not unique. Statistic extensions won't be removed.")
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
        print_message("ERROR", "From needs to be numeric. Adding extensions will be aborted.")
        return(data_frame)
    }

    if (!is.character(extensions)){
        print_message("ERROR", "Extensions need to be characters. Adding extensions will be aborted.")
        return(data_frame)
    }

    if (from > collapse::fncol(data_frame)){
        print_message("ERROR", "From is greater than number of columns in data frame. Adding extensions will be aborted.")
        return(data_frame)
    }

    if (!reuse %in% c("none", "last", "repeat")){
        print_message("WARNING", "Reuse must be one of 'none', 'last', 'repeat'. 'none' will be used.")
        reuse <- "none"
    }

    var_names <- names(data_frame)

    # Columns to modify
    target_columns <- from:length(var_names)
    n_target       <- length(target_columns)
    n_extensions   <- length(extensions)

    # Generally shorten extensions, if there are more than columns are left in the data frame
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
    # If there are no exceptions just do a normal replace
    if (is.null(exceptions) || length(exceptions) == 0){
        return(gsub(pattern, replacement, vector))
    }

    # Replace pattern first in exceptions globally
    placeholder      <- "&%!"
    except_protected <- gsub(pattern, placeholder, exceptions, fixed = TRUE)

    # Protect exceptions in original vector
    for (i in seq_along(exceptions)){
        vector <- gsub(exceptions[i], except_protected[i], vector, fixed = TRUE)
    }

    # Replace pattern safely
    vector <- gsub(pattern, replacement, vector)

    # Reestablish protected pattern
    gsub(placeholder, pattern, vector, fixed = TRUE)
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
#' new_names_df <- my_data |> rename_multi(sex   = var1,
#'                                         age   = var2,
#'                                         state = var3)
#'
#' # Also works with variable names in quotation marks
#' new_names_df <- my_data |> rename_multi("sex"   = "var1",
#'                                         "age"   = "var2",
#'                                         "state" = "var3")
#'
#' # It is also possible to rename variables stored in vectors
#' old_names <- c("sex", "age", "state")
#' new_names <- c("var1", "var2", "var3")
#'
#' new_names_df <- my_data |> rename_multi(old_names = new_names)
#'
#' # Single variables and vectors can be mixed in one call
#' old_names <- c("income", "balance")
#' new_names <- c("var1", "var2")
#'
#' new_names_df <- my_data |> rename_multi(sex       = var3,
#'                                         old_names = new_names)
#'
#' @export
rename_multi <- function(data_frame, ...){
    # Measure the time
    print_start_message(suppress = TRUE)

    parent_env <- parent.frame()

    # Capture the ellipsis as it was passed in. This keeps quoted, unquoted and
    # vector based renamings apart, which is needed because a single variable and
    # a vector of variables can be passed in the same call.
    rename_list <- as.list(substitute(list(...)))[-1]

    # Collect all arguments as old and new name pairs.
    old_names <- character(0)
    new_names <- character(0)

    for (arg_name in names(rename_list)){
        # Depending on whether the variable names were passed in with or without
        # quotation marks, they have to be captured on a different way.
        new_name <- tryCatch(eval(rename_list[[arg_name]], envir = parent_env),
                             error = function(e){
                                 NULL
                             })

        # Arguments which are not a variable of the data frame, but a vector of
        # variable names, belong to a vector renaming.
        old_name <- tryCatch(get(arg_name, envir = parent_env),
                             error = function(e){
                                 NULL
                             })

        # Check if arg_name refers to an external character vector in parent_env
        # rather than a direct column name in data_frame (vector-based renaming).
        if (is.character(old_name) && !(arg_name %in% names(data_frame))){
            old_names <- c(old_names, old_name)
            new_names <- c(new_names, new_name)
        }
        # Treat arg_name itself as the target column name to be renamed
        else{
            old_names <- c(old_names, arg_name)

            # If new_name evaluated successfully to a character string, keep it as is
            if (is.character(new_name)){
                new_name <- new_name
            }
            # Otherwise, capture unquoted symbol names by converting the expression
            # to a string.
            else{
                new_name <- deparse(rename_list[[arg_name]])
            }

            new_names <- c(new_names, new_name)
        }
    }

    # Old and new names have to match up one to one
    if (length(old_names) != length(new_names)){
        print_message("ERROR", c("The provided <old names> and <new names> differ in length",
                                 "([old] vs. [new]). Renaming will be aborted."),
                      old = length(old_names), new = length(new_names))
        return(data_frame)
    }

    # Make sure that the variables provided are part of the data frame.
    old_names <- data_frame |> part_of_df(old_names, check_only = TRUE)

    if (is.list(old_names)){
        print_message("ERROR", c("The provided <old name> '[old]' is not part of",
 								 "the data frame. Pass in variables to be renamed in the form:",
 								 '"old_var" = "new_var". Renaming will be aborted.'), old = old_names[[1]])
        return(data_frame)
    }

    # If any of the new variable names is already part of the data frame abort
    invalid_new_names <- new_names[new_names %in% names(data_frame)]

    # Extract identical variable names and only rename the ones who differ
    old_names         <- old_names[!new_names %in% invalid_new_names]
    new_names         <- new_names[!new_names %in% invalid_new_names]

    # Rename all variables in one go
    if (length(new_names) > 0 && length(old_names) > 0){
        data_frame <- data_frame |> collapse::frename(stats::setNames(old_names, new_names))
    }

    print_closing()

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
first_row_as_names <- function(data_frame){
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


#' Clean Duplicate Suffixes From Variable Names
#'
#' @description
#' This function removes all automatically generated duplicate suffixes ".dup"
#' from variable names. Variable names that appear more than once after this
#' removal get a number at the end to make them unique again.
#'
#' @param names A vector of variable names.
#'
#' @return
#' Returns the variable names without duplicate suffixes. Names that are still
#' not unique get a number appended at the end.
#'
#' @noRd
remove_duplicate_suffixes <- function(names){
    # First remove all duplicate suffixes from all variable names
    clean_names <- gsub("\\.dup[0-9]+", "", names)

    # Rescan the cleaned names and identify the names that are not unique
    counts           <- table(clean_names)
    duplicated_names <- names(counts)[counts > 1]

    if (length(duplicated_names) == 0){
        return(clean_names)
    }

    # Add a number at the end of every occurrence of a duplicated name
    result <- clean_names

    for (dup_name in duplicated_names){
        number <- 1
        for (i in which(clean_names == dup_name)){
            new_name <- paste0(dup_name, ".dup", number)

            # Use the next free number if a name is already taken
            while (new_name %in% result[-i]){
                number   <- number + 1
                new_name <- paste0(dup_name, ".dup", number)
            }

            result[i] <- new_name
            number    <- number + 1
        }
    }

    result
}
