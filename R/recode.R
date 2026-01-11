#' Recode New Variables With Formats
#'
#' @name recode
#'
#' @description
#' Instead of writing multiple if-clauses to recode values into a new variable,
#' you can use formats to recode a variable into a new one.
#'
#' @param data_frame A data frame which contains the the original variables to recode.
#' @param new_var The name of the newly created and recoded variable.
#' @param ... [recode()] Pass in the original variable name that should be recoded
#' along with the corresponding format container in the form: variable = format.
#'
#' In [recode_multi()] multiple variables can be recoded in one go and multilabels
#' can be applied. This overwrites the original variables and duplicates rows if
#' multilabels are applied. In occasions were you want to use format containers to
#' afterwards perform operations with other packages, you can make use of this
#' principle with this function.
#'
#' @details
#' [recode()] is based on the 'SAS' function put(), which provides an efficient
#' and readable way, to generate new variables with the help of formats.
#'
#' When creating a format you can basically write code like you think: This new
#' category consists of these original values. And after that you just apply these
#' new categories to the original values to create a new variable. No need for multiple
#' if_else statements.
#'
#' @return
#' Returns a data frame with the newly recoded variable.
#'
#' @seealso
#' Creating formats: [discrete_format()] and [interval_format()].
#'
#' Functions that also make use of formats: [frequencies()], [crosstabs()],
#' [any_table()].
#'
#' @examples
#' # Example formats
#' age. <- discrete_format(
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = 25:54,
#'     "55 to under 65" = 55:64,
#'     "65 and older"   = 65:100)
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' my_data <- my_data |> recode("age_group1", age = age.)
#'
#' # Formats can also be passed as characters
#' my_data <- my_data |> recode("age_group2", age = "age.")
#'
#' # Multilabel recode
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' income. <- interval_format(
#'     "Total"              = 0:99999,
#'     "below 500"          = 0:499,
#'     "500 to under 1000"  = 500:999,
#'     "1000 to under 2000" = 1000:1999,
#'     "2000 and more"      = 2000:99999)
#'
#' multi_data <- my_data |> recode_multi(sex = sex., income = income.)
#'
#' @rdname recode
#'
#' @export
recode <- function(data_frame,
                   new_var,
                   ...){
    # Measure the time
    start_time <- Sys.time()

    ###########################################################################
    # Early evaluations
    ###########################################################################

    # Convert to character
    new_var <- gsub("\"", "", deparse(substitute(new_var)))

    if (new_var %in% names(data_frame)){
        message(" X ERROR: Variable '", new_var, "' already exists. Recoding aborted, variable won't be overwritten.")
        return(data_frame)
    }

    # Translate ... into separately controllable arguments
    formats <- list(...)

    # Evaluate formats early
    if (!is_list_of_dfs(formats)){
        formats <- evaluate_formats(formats)
    }

    ###########################################################################
    # Error handling
    ###########################################################################

    # Get information from ... list
    current_var <- names(formats)[1]
    format_df   <- formats[[current_var]]

    if (!current_var %in% names(data_frame)){
        message(" X ERROR: Variable '", current_var, "' not found in the input data frame. No format will be applied.")
        return(data_frame)
    }

    if (!data.table::is.data.table(format_df)){
        message(" X ERROR: The format for '", current_var, "' must be a data table. No format will be applied.")
        return(data_frame)
    }

    if (names(format_df)[1] == "value" && collapse::any_duplicated(format_df[["value"]])){
        message(" ! WARNING: The format for '", current_var, "' is a multilabel. A multilabel can't be fully applied in recode.\n",
                "            Only one of the matching categories will be applied.")

        format_df <- format_df |> unique(by = "value", fromLast = FALSE)
    }

    if (is.factor(data_frame[[current_var]])){
        message(" ~ NOTE: '", current_var, "' is a factor variable. Formats only work if the visible character values\n",
                "         are specified as input values and not the factor levels.")
    }

    ###########################################################################
    # Recode
    ###########################################################################

    # Look up variable names in format data frame to check whether it is an
    # interval or discrete format
    interval_variables <- c("from", "to")
    actual_variables   <- names(format_df)[1:2]

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # In case of interval format
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (identical(interval_variables, actual_variables)){
        # Remove NA values
        if (any(is.na(data_frame[[current_var]]))){
            message(" ! WARNING: Variable '", current_var, "' has NA values. Interval merge only works without NA values.\n",
                    "            NA values will be removed.")
        }

        data_frame <- data_frame |>
            collapse::fsubset(!is.na(data_frame[[current_var]]))

        # Get number of rows from data frame to compare after the merge to check for multilabel
        original_rows <- collapse::fnrow(data_frame)

        # Generate pseudo variables for range merging
        data_frame[["qol_from"]] <- data_frame[[as.character(current_var)]]
        data_frame[["qol_to"]]   <- data_frame[[as.character(current_var)]]

        data_frame[["qol_ID"]] <- seq_len(collapse::fnrow(data_frame))

        # Set key variables
        temp_dt   <- data.table::as.data.table(data_frame)
        format_dt <- data.table::copy(format_df)

        data.table::setkey(temp_dt, qol_from, qol_to)
        data.table::setkey(format_dt, from, to)

        # Merge data frame with format by range
        data_frame <- data.table::foverlaps(temp_dt, format_dt,
                                            by.x = c("qol_from", "qol_to"),
                                            by.y = c("from", "to")) |>
            collapse::frename(stats::setNames("label", new_var)) |>
            data.table::setorder(qol_ID) |>
            unique(by = "qol_ID") |>
            dropp("qol_ID", "qol_from", "qol_to", "from", "to")

        data_frame <- data_frame |>
            data.table::setcolorder(new_var, after = ncol(data_frame))

        if (collapse::fnrow(data_frame) > original_rows){
            message(" ! WARNING: The format for '", current_var, "' is a multilabel. For interval formats this leads to\n",
                    "            doubling observations.")
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # In case of discrete format
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    else{
        # Rename label column to be specific to the variable
        format_df <- format_df |>
            collapse::frename(stats::setNames("value", current_var))

        # Join format with data frame
        data_frame <- data_frame |>
            collapse::join(format_df,
                           on = current_var,
                           how = "left",
                           verbose = FALSE) |>
            collapse::frename(stats::setNames("label", new_var))
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'recode' execution time: ", end_time, " seconds")

    data_frame
}


#' @rdname recode
#'
#' @export
recode_multi <- function(data_frame, ...){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into a list if possible
    formats <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e) {
        # Evaluation failed
        NULL
    })

    if (is.null(formats)){
        message('X ERROR: Unknown object found. Provide recode arguments in the form: variable_name = format_df.\n",
                "         Recoding will be aborted.')
        return(data_frame)
    }

    # Evaluate formats early
    if (!is_list_of_dfs(formats)){
        formats <- evaluate_formats(formats)
    }

    # Get information from ... list
    variables <- names(formats)
    var_order <- names(data_frame)

    data_frame <- data_frame |>
        apply_format(formats, variables) |>
        data.table::setcolorder(var_order)

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'recode_multi' execution time: ", end_time, " seconds")

    data_frame
}
