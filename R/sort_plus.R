#' Sort Data Frame Rows With Some Additions
#'
#' @description
#' Sort data frame rows by the provided variables. [sort_plus()] is also able to
#' preserve the current order of certain variables and only sort other variables
#' within this order. As another option one can sort a variable with the help of
#' formats, which can be used to e.g. sort a character variable in another than
#' alphabetical order without creating a temporary variable just for sorting.
#'
#' @param data_frame A data frame to summarise.
#' @param by A variable vector which contains the variables to sort by.
#' @param preserve A vector containing all variables which current order should be
#' preserved.
#' @param order A vector containing the sorting order for each variable. 'ascending'/'a'
#' or 'descending'/'d' can be used. If there are less orders given than by variables
#' provided, the last given sorting order will be used for the additional by variables.
#' @param formats A list in which is specified which formats should be used to sort
#' certain variables.
#' @param na.last TRUE by default. Specifies whether NA values should come last or first.
#'
#' @details
#' [sort_plus()] is just very loosely based on the 'SAS' procedure Proc Sort. It tries
#' to keep the simplicity, but with some added features.
#'
#' @return
#' Returns a sorted data table.
#'
#' @seealso
#' Creating formats: [discrete_format()] and [interval_format()].
#'
#' Functions that also make use of formats: [frequencies()], [crosstabs()],
#' [any_table()], [recode()], [recode_multi()], [transpose_plus()].
#'
#' @examples
#' # Example formats
#' education. <- discrete_format(
#'     "1" = "low",
#'     "2" = "middle",
#'     "3" = "high")
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Simple sorting
#' sort_df1 <- my_data |> sort_plus(by = c(state, sex, age))
#' sort_df2 <- my_data |> sort_plus(by    = c(state, sex, age),
#'                                  order = c("ascending", "descending"))
#'
#' # Character variables will normally be sorted alphabetically. With the help
#' # of a format this variable can be sorted in a completely different way.
#' sort_df3 <- my_data |> sort_plus(by      = education,
#'                                  formats = list(education = education.))
#'
#' # Preserve the order of the character variable, otherwise it couldn't stay in
#' # it's current order.
#' sort_df4 <- sort_df3 |> sort_plus(by       = age,
#'                                   preserve = education)
#'
#' @export
sort_plus <- function(data_frame,
                      by,
                      preserve = NULL,
                      order    = "ascending",
                      formats  = c(),
                      na.last  = TRUE){
    # Measure the time
    start_time <- Sys.time()

    ###########################################################################
    # Early evaluations
    ###########################################################################

    # First convert data frame to data table
    if (!data.table::is.data.table(data_frame)){
        data_frame <- data.table::as.data.table(data_frame)
    }

    # Evaluate formats early
    if (!is_list_of_dfs(formats)){
        formats_list <- as.list(substitute(formats))[-1]
        formats      <- evaluate_formats(formats_list)
    }

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Preserve variables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    preserve <- get_origin_as_char(preserve, substitute(preserve))

    # Make sure that the variables provided are part of the data frame.
    preserve <- data_frame |> part_of_df(preserve)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # By variables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    by <- get_origin_as_char(by, substitute(by))

    # If no value variables are provided abort
    if (length(by) <= 1){
        if (length(by) == 0 || by == ""){
            message(" X ERROR: No <by> variables provided. Sorting will be aborted.")
            return(invisible(NULL))
        }
    }

    # Make sure there is no column variable that is also a row variable.
    by <- resolve_intersection(by, preserve)

    # Make sure that the variables provided are part of the data frame.
    by <- data_frame |> part_of_df(by)

    if (length(by) == 0){
        message(" X ERROR: No valid <by> variable provided. Sorting will be aborted.")
        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Double entries
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    preserve <- remove_doubled_values(preserve)
    by       <- remove_doubled_values(by)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Order
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # If there are invalid orders specified transform them to ascending order
    invalid_order <- any(!tolower(order) %in% c("ascending", "descending", "a", "d"))

    if (invalid_order){
        message(" ! WARNING: <Order> other than 'ascending'/'a' or 'descending'/'d' specified, which is\n",
                "            not allowed. 'ascending' will be used instead.")
    }

    # Convert character expressions into numbers, which is needed later on in the
    # data.table sort.
    order[order %in% c("descending", "d")] <- -1
    order[order != -1] <- 1

    # Fill up order vector with the last element or trim it down, if too many
    # elements are provided.
    order <- fill_or_trim(order, length(by))

    ###########################################################################
    # Sorting starts
    ###########################################################################

    # If formats are provided, they will be used to temporarily recode new variables,
    # which are used to order the data frame in format order first.
    if (!is.null(formats)){
        message("\n > Preparing formats")

        extended_by <- c()

        for (variable in names(formats)){
            if (is_multilabel(formats, variable)){
                message(" ! WARNING: Applying a multilabel to variable '", variable, "' is not allowed.\n",
                        "            Format won't be applied. Variable is skipped.")
                next
            }

            if (!variable %in% names(data_frame)){
                message(" ! WARNING: The variable '", variable, "' is not part of the data frame.\n",
                        "            Format cannot be applied. Variable is skipped.")
                next
            }

            if (!variable %in% by){
                message(" ~ NOTE: Format for variable '", variable, "' is provided, but the variable itself.\n",
                        "         is not part of the <by> variables. Format won't be applied. Variable is skipped.")
                next
            }

            # To make recode work "variable" can't be passed directly, because recode
            # would take it as "variable" instead of e.g. "age". Therefore a named list
            # with all the needed elements has to be constructed beforehand.
            new_column    <- paste0(".sort_", variable)
            argument_list <- list(data_frame, new_column)
            argument_list[[variable]] <- formats[[variable]]

            # To prevent errors when running examples
            if (length(argument_list) < 3){
                next
            }

            # Now use do.call to run e.g.: recode(data_frame, ".sort_age", age = formats[["age"]])
            data_frame <- suppressMessages(do.call(recode, argument_list))

            # Format new sorting variable as factor to sort in provided format order
            # Extract the number of labels from variable
            label_levels <- formats[[variable]] |>
                unlist(use.names = FALSE) |>
                collapse::funique() |>
                collapse::na_omit()

            # Convert variable to factor
            data_frame[[new_column]] <- factor(
                data_frame[[new_column]],
                levels  = label_levels,
                ordered = TRUE)

            extended_by <- c(extended_by, new_column)
        }

        by <- c(extended_by, by)
    }

    if (!is.null(preserve)){
        message("\n > Preserving: ", paste(preserve, collapse = ", "))

        # If there are variables to preserve, they will be converted into factor
        # variables before sorting. To restore them afterwards, to there original
        # format, grab it here.
        original_class <- lapply(data_frame[preserve], class)

        # Convert preserve variables to factor so that they keep their order during sort
        data_frame <- data_frame |> convert_factor(preserve)
        new_by     <- c(preserve, by)

        # Fill up order vector with the last element or trim it down, if too many
        # elements are provided.
        order <- fill_or_trim(order, length(new_by))

        # Actual sort
        sort_df <- data.table::copy(data_frame) |>
            data.table::setorderv(cols    = new_by,
                                  order   = order,
                                  na.last = na.last)

        # Convert preserve variables back
        for (variable in names(original_class)){
            if (original_class[[variable]] == "numeric"){
                sort_df[[variable]] <- as.numeric(sort_df[[variable]])
            }
            else if (original_class[[variable]] == "character"){
                sort_df[[variable]] <- as.character(sort_df[[variable]])
            }
        }
    }
    else{
        sort_df <- data.table::copy(data_frame) |>
            data.table::setorderv(cols    = by,
                                  order   = order,
                                  na.last = na.last)
    }

    # Clean up: Remove all temporary sort variables, if formats where used
    sort_vars <- grep("^\\.sort", names(data_frame), value = TRUE)
    sort_df   <- suppressMessages(sort_df |> dropp(sort_vars))

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'sort_plus' execution time: ", end_time, " seconds\n")

    sort_df
}
