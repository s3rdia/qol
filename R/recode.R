#' Recode New Variables With Formats
#'
#' @name recode
#'
#' @description
#' Instead of writing multiple if-clauses to recode values into a new variable,
#' you can use formats to recode a variable into a new one.
#'
#' @param data_frame A data frame which contains the the original variables to recode.
#' @param ... [recode.()] Pass in the original variable name that should be recoded
#' along with the corresponding format container in the form: variable = format.
#'
#' In [recode_multi()] multiple variables can be recoded in one go and multilabels
#' can be applied. This overwrites the original variables and duplicates rows if
#' multilabels are applied. In occasions were you want to use format containers to
#' afterwards perform operations with other packages, you can make use of this
#' principle with this function.
#'
#' @details
#' [recode.()] is based on the 'SAS' function put(), which provides an efficient
#' and readable way, to generate new variables with the help of formats.
#'
#' When creating a format you can basically write code like you think: This new
#' category consists of these original values. And after that you just apply these
#' new categories to the original values to create a new variable. No need for multiple
#' if_else statements.
#'
#' @return
#' [recode.()]: If one variable is provided, returns a vector with recoded values.
#' If multiple variables are provided, returns a list of recoded vectors which can
#' be assigned to multiple new variables at once.
#'
#' @seealso
#' Creating formats: [discrete_format()] and [interval_format()].
#'
#' Functions that also make use of formats: [frequencies()], [crosstabs()],
#' [any_table()], [summarise_plus()], [transpose_plus()], [sort_plus()]
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
#' my_data[["age_group1"]] <- my_data |> recode.(age = age.)
#'
#' # Formats can also be passed as characters
#' my_data[["age_group2"]] <- my_data |> recode.(age = "age.")
#'
#' # Multiple variables can be recoded at once into multiple new variables
#' income. <- interval_format(
#'     "below 500"          =    0:500,
#'     "500 to under 1000"  =  500:1000,
#'     "1000 to under 2000" = 1000:2000,
#'     "2000 and more"      = 2000:100000)
#'
#' my_data[, c("age_group", "income_group")] <- my_data |>
#'     recode.(age = age., income = income.)
#'
#' # Multilabel recode
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' income_multi. <- interval_format(
#'     "Total"              =    0:100000,
#'     "below 500"          =    0:500,
#'     "500 to under 1000"  =  500:1000,
#'     "1000 to under 2000" = 1000:2000,
#'     "2000 and more"      = 2000:100000)
#'
#' # recode_multi() can not only apply multiple recodings, but it can also
#' # apply multilabels.
#' # NOTE: Recoding will always be in place. When applying multilabels the
#' #       result data frame will have more observations than before.
#' multi_data <- my_data |> recode_multi(sex = sex., income = income_multi.)
#'
#' @rdname recode
#'
#' @export
recode. <- function(data_frame,
                    ...){
    # Measure the time
    print_start_message(suppress = TRUE)

    ###########################################################################
    # Early evaluations
    ###########################################################################

    # Translate ... into separately controllable arguments
    formats <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(formats)){
        print_message("ERROR", "Unknown object found. Recode will be aborted.")
        return(invisible(NULL))
    }

    # Evaluate formats early
    if (!is_list_of_dfs(formats)){
        formats <- evaluate_formats(formats)
    }

    ###########################################################################
    # Recode
    ###########################################################################

    recoded <- list()

    for (current_var in names(formats)){
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Error handling
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        format_df <- formats[[current_var]]

        if (!current_var %in% names(data_frame)){
            print_message("ERROR", "Variable '[var]' not found in the input data frame. No format will be applied.", var = current_var)
            return(invisible(NULL))
        }

        if (!data.table::is.data.table(format_df)){
            print_message("ERROR", "The format for '[var]' must be a data table. No format will be applied.", var = current_var)
            return(invisible(NULL))
        }

        if (names(format_df)[1] == "value" && collapse::any_duplicated(format_df[["value"]])){
            print_message("WARNING", c("The format for '[var]' is a multilabel. A multilabel can't be fully applied in recode.",
                                       "Only one of the matching categories will be applied."), var = current_var)

            format_df <- format_df |> unique(by = "value", fromLast = FALSE)
        }

        if (is.factor(data_frame[[current_var]])){
            print_message("NOTE", c("'[var]' is a factor variable. Formats only work if the visible character values",
                                    "are specified as input values and not the factor levels."), var = current_var)
        }

        # Only keep the variable to be recoded
        recode_df <- data_frame |> keep(current_var)

        # Look up variable names in format data frame to check whether it is an
        # interval or discrete format
        interval_variables <- c("from", "to")
        actual_variables   <- names(format_df)[1:2]

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # In case of interval format
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        if (identical(interval_variables, actual_variables)){
            # Reduce multilabel formats to unique ranges which don't overlap
            format_original <- format_df
            format_df       <- format_df |> collapse::fsubset(from >= data.table::shift(cummax(to), fill = -Inf))

            if (collapse::fnrow(format_original) > collapse::fnrow(format_df)){
                print_message("WARNING", c("The format for '[current_var]' is a multilabel. A multilabel can't be fully applied in recode.",
                                           "Only one of the matching categories will be applied."), current_var = current_var)
            }

            # Keep the original values to fill positions which are not covered by the format
            original_values <- recode_df[[current_var]]

            # Get number of rows from data frame to compare after the merge to check for multilabel
            original_rows <- collapse::fnrow(recode_df)

            # Separate NAs from rest of the data frame because the used join
            # can't handle them
            na_positions <- is.na(recode_df[[current_var]])
            recode_df    <- recode_df |> collapse::fsubset(!na_positions)

            # Generate pseudo variables for range merging
            recode_df[["qol_from"]] <- recode_df[[as.character(current_var)]]
            recode_df[["qol_to"]]   <- recode_df[[as.character(current_var)]]

            recode_df[["qol_ID"]] <- seq_len(collapse::fnrow(recode_df))

            # Set key variables
            temp_dt   <- data.table::as.data.table(recode_df)
            format_dt <- data.table::copy(format_df)

            data.table::setkey(format_dt, from, to)

            # Merge data frame with format by range
            temp_dt <- data.table::foverlaps(temp_dt, format_dt,
                                             by.x = c("qol_from", "qol_to"),
                                             by.y = c("from", "to")) |>
                keep("label", "qol_from")

            # If a value is not covered by the format, keep the original value instead of NA
            unmatched <- is.na(temp_dt[["label"]]) & !is.na(temp_dt[["qol_from"]])
            temp_dt[["label"]][unmatched] <- temp_dt[["qol_from"]][unmatched]

            # NA values are now inserted in the same spots as they where before to
            # ensure that there will be no missmatch with the original data frame.
            recode_df                <- rep(NA, original_rows)
            recode_df[!na_positions] <- temp_dt[["label"]]
            recode_df                <- data.table::as.data.table(recode_df)
        }

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # In case of discrete format
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        else{
            # Keep the original values to fill positions which are not covered by the format
            original_values <- recode_df[[current_var]]

            # Rename label column to be specific to the variable
            format_df <- format_df |>
                collapse::frename(stats::setNames("value", current_var))

            # Join format with data frame
            recode_df <- recode_df |>
                collapse::join(format_df,
                               on      = current_var,
                               how     = "left",
                               verbose = FALSE,
                               overid  = 2) |>
                keep("label")

            # If a value is not covered by the format, keep the original value instead of NA.
            # The "other" keyword is an exception and catches all remaining values.
            na_positions <- which(is.na(recode_df[["label"]]) & !is.na(original_values))
            if (length(na_positions) > 0){
                if (as.character(.Machine[["integer.max"]]) %in% tolower(format_df[[current_var]])){
                    recode_df[["label"]][na_positions] <- format_df[["label"]][tolower(format_df[[current_var]]) == as.character(.Machine[["integer.max"]])]
                }
                else if(.Machine[["integer.max"]] %in% format_df[[current_var]]){
                    recode_df[["label"]][na_positions] <- format_df[["label"]][format_df[[current_var]] == .Machine[["integer.max"]]]
                }
                else{
                    recode_df[["label"]][na_positions] <- original_values[na_positions]
                }
            }
        }

        recoded[[current_var]] <- as.vector(recode_df)[[1]]
    }

    print_closing()

    if (length(recoded) == 1){
        recoded[[1]]
    }
    else{
        recoded
    }
}


#' @param convert TRUE by default. Converts recoded variables to numeric or character
#' depending on the input format instead of leaving them as factors.
#'
#' @return
#' [recode_multi()]: Returns a data frame with the newly recoded variable.
#'
#' @rdname recode
#'
#' @export
recode_multi <- function(data_frame, ..., convert = TRUE){
    # Measure the time
    print_start_message(suppress = TRUE)

    # Translate ... into a list if possible
    formats <- tryCatch({
        # Force evaluation to see if it exists
        list(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(formats)){
        print_message("ERROR", c("Unknown object found. Provide recode arguments in the form: variable_name = format_name",
								 "Recoding will be aborted."))
        return(data_frame)
    }

    # First check if a list of data frames is wrapped as single entry in a list.
    # This can happen if e.g. a list of data frames is passed on.
    if (is_list_of_dfs(formats[[1]])){
        formats <- formats[[1]]
    }
    # Otherwise evaluate formats early
    else if (!is_list_of_dfs(formats)){
        formats <- evaluate_formats(formats)
    }

    # Get information from ... list
    variables <- names(formats)
    var_order <- names(data_frame)

    # Check if all variables are part of the data frame
    variables <- data_frame |> part_of_df(variables, check_only = TRUE)

    if (is.list(variables)){
        print_message("ERROR", c("The provided variable[?s] '[vars]' [?is/are] not part of",
                                 "the data frame. Recoding will be aborted."), vars = variables[[1]])
        return(data_frame)
    }

    # Apply formats
    data_frame <- data_frame |>
        apply_format(formats, variables) |>
        data.table::setcolorder(var_order)

    # Convert formatted values to intended type
    if (convert){
        for (variable in variables){
            if (is.numeric(formats[[variable]][["label"]])){
                data_frame[[variable]] <- as.numeric(as.character(data_frame[[variable]]))
            }
            else{
                data_frame[[variable]] <- as.character(data_frame[[variable]])
            }
        }
    }

    print_closing(5)

    data_frame
}
