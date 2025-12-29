#' Fast And Powerful Yet Simple To Use Transpose
#'
#' @description
#' [transpose_plus()] is able to reshape a data frame from long to wide and from
#' wide to long. In the long to wide transposition variables can be nested or
#' placed side by side. With the wide to long transposition it is also possible
#' to transpose multiple variables at once.
#'
#' Additionally [transpose_plus()] is able to weight results before transposing
#' them from long to wide.
#'
#' The function also makes use of formats, which means you don't need to create
#' variables storing the new variable names before transposition. You can just
#' use formats to name the new variables and with multilabels you can even generate
#' new variable expressions at the same time.
#'
#' @param data_frame A data frame to transpose
#' @param preserve Variables to keep and preserve in their current form.
#' @param pivot A vector that provides the expressions of single variables or od
#' variable combinations that should be transposed. To nest variables use the form:
#' "var1 + var2 + var3 + ...".
#' @param values A vector containing all value variables that should be transposed.
#' @param formats A list in which is specified which formats should be applied to which
#' variables.
#' @param weight Put in a weight variable to compute weighted results.
#' @param na.rm FALSE by default. If TRUE removes all NA values from the preserve and
#' pivot variables.
#' @param monitor FALSE by default. If TRUE, outputs two charts to visualize the
#' functions time consumption.
#'
#' @details
#' [transpose_plus()] is just very loosely based on the 'SAS' procedure Proc Transpose,
#' and the possibilities of a Data-Step transposition using loops.
#'
#' The transposition methods 'SAS' has to offer are actually fairly weak. Which is weird
#' because all tools are there to have another powerful function. So [transpose_plus()]
#' tries to create the function 'SAS' should have.
#'
#' The function is able to interpret which transposition direction the user wants by just
#' looking at what the user provided with the function parameters. For a long to wide
#' transposition it is natural to just provide variables to transpose. While it is also
#' just natural to provide new variable names when transposing from wide to long. That alone
#' reduces the number of parameters the user has to enter to perform a simple transposition.
#'
#' The real magic happens when formats come into play. With their help you can not only name
#' new variables or their expressions, but you can also generate completely new expressions
#' with no effort, just with the help of multilabels.
#'
#' @return
#' Returns a transposed data table.
#'
#' @seealso
#' Creating formats: [discrete_format()] and [interval_format()].
#'
#' Functions that also make use of formats: [frequencies()], [crosstabs()],
#' [any_table()], [recode()], [recode_multi()], [sort_plus()].
#'
#' @examples
#' # Example formats
#' age. <- discrete_format(
#'     "Total"          = 0:100,
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = 25:54,
#'     "55 to under 65" = 55:64,
#'     "65 and older"   = 65:100)
#'
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' sex2. <- discrete_format(
#'     "Total"  = c("Male", "Female"),
#'     "Male"   = "Male",
#'     "Female" = "Female")
#'
#' income. <- interval_format(
#'     "Total"              = 0:99999,
#'     "below 500"          = 0:499,
#'     "500 to under 1000"  = 500:999,
#'     "1000 to under 2000" = 1000:1999,
#'     "2000 and more"      = 2000:99999)
#'
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Transpose from long to wide and use a multilabel to generate additional categories
#' long_to_wide <- my_data |>
#'     transpose_plus(preserve = c(year, age),
#'                    pivot    = c("sex", "education"),
#'                    values   = income,
#'                    formats  = list(sex = sex., age = age.),
#'                    weight   = weight,
#'                    na.rm    = TRUE)
#'
#' # Transpose back from wide to long
#' wide_to_long <- long_to_wide |>
#'     transpose_plus(preserve = c(year, age),
#'                    pivot    = list(sex       = c("Total", "Male", "Female"),
#'                                    education = c("low", "middle", "high")))
#'
#' # Nesting variables in long to wide transposition
#' nested <- my_data |>
#'     transpose_plus(preserve = c(year, age),
#'                    pivot    = "sex + education",
#'                    values   = income,
#'                    formats  = list(sex = sex., age = age.),
#'                    weight   = weight,
#'                    na.rm    = TRUE)
#'
#' # Or both, nested and un-nested, at the same time
#' both <- my_data |>
#'     transpose_plus(preserve = c(year, age),
#'                    pivot    = c("sex + education", "sex", "education"),
#'                    values   = income,
#'                    formats  = list(sex = sex., age = age.),
#'                    weight   = weight,
#'                    na.rm    = TRUE)
#'
#' @export
transpose_plus <- function(data_frame,
                           preserve   = NULL,
                           pivot,
                           values     = NULL,
                           formats    = c(),
                           weight     = NULL,
                           na.rm      = FALSE,
                           monitor    = FALSE){
    # Measure the time
    start_time <- Sys.time()

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Error handling", "Preparation")
    #-------------------------------------------------------------------------#

    ###########################################################################
    # Early evaluations
    ###########################################################################

    # First convert data frame to data table
    if (!data.table::is.data.table(data_frame)){
        data_frame <- data.table::as.data.table(data_frame)
    }

    # Evaluate formats early, otherwise apply formats can't evaluate them in unit
    # test situation.
    formats_list <- as.list(substitute(formats))[-1]

    if (length(formats_list) > 0){
        formats <- stats::setNames(
            lapply(formats_list, function(expression){
                # Catch expression if passed as string
                if (is.character(expression)) {
                    tryCatch(get(expression, envir = parent.frame()),
                             error = function(e) NULL)
                }
                # Catch expression if passed as symbol
                else{
                    tryCatch(eval(expression, envir = parent.frame()),
                             error = function(e) NULL)
                }
            }),
            names(formats_list))
    }

    # If all pivot list/vector entries have a name, transposition will be wide to long
    long_to_wide <- TRUE

    if (!is.null(names(pivot)) && all(nzchar(names(pivot)))){
        long_to_wide <- FALSE
    }

    ###########################################################################
    # Error handling
    ###########################################################################

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Wide to long
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (!long_to_wide){
        # Crossing variables not possible when transposing into long format.
        # Loop through all variable vectors in named list. If there is any crossing
        # listed, abort.
        for (variables in pivot){
            if (any(grepl("+", variables, fixed = TRUE))){
                message(" X ERROR: Nesting pivot variables in a wide to long transposition is not possible.\n",
                        "          Transposition will be aborted.")
                return(invisible(NULL))
            }
        }

        # Values and weight has no effect in wide to long transposition
        if (!is.null(values)){
            message(" ~ NOTE: Values parameter has no effect in wide to long transposition.")
        }

        if (!is.null(weight)){
            message(" ~ NOTE: Weight parameter has no effect in wide to long transposition.")
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Weight
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (long_to_wide){
        weight_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(weight), width.cutoff = 500L)))

        # Create temporary weight column if none is provided.
        # Also get the name of the weight variable as string.
        if (weight_temp == "NULL" || substr(weight_temp, 1, 2) == "c("){
            weight_var <- ".temp_weight"
            data_frame[[".temp_weight"]] <- 1

            if (substr(weight_temp, 1, 2) == "c("){
                message(" ! WARNING: Only one variable for weight allowed. Evaluations will be unweighted.")
            }
        }
        else if (!is_error(weight)){
            # In this case weight already contains the substituted variable name
            # while weight_temp is evaluated to the symbol passed into the function.
            weight_var <- weight
        }
        else if (!weight_temp %in% names(data_frame)){
            weight_var <- ".temp_weight"
            data_frame[[".temp_weight"]] <- 1

            message(" ! WARNING: Provided weight variable is not part of the data frame. Unweighted results will be computed.")
        }
        else if (!is_numeric(data_frame[[weight_temp]])){
            weight_var <- ".temp_weight"
            data_frame[[".temp_weight"]] <- 1

            message(" ! WARNING: Provided weight variable is not numeric. Unweighted results will be computed.")
        }
        else{
            weight_var <- weight_temp

            # NA values in weight lead to errors therefor convert them to 0
            if (anyNA(data_frame[[weight_temp]])){
                message(" ~ NOTE: Missing values in weight variable '", weight_temp, "' will be converted to 0.")
            }
            data_frame[[weight_temp]] <- data.table::fifelse(is.na(data_frame[[weight_temp]]), 0, data_frame[[weight_temp]])

            # @Hack: so I don't have to check if .temp_weight exists later on
            data_frame[[".temp_weight"]] <- 1
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Preserve variables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Convert to character vectors
    preserve_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(preserve), width.cutoff = 500L)))

    if (substr(preserve_temp, 1, 2) == "c("){
        preserve <- as.character(substitute(preserve))
    }
    else if (!is_error(preserve)){
        # Do nothing. In this case preserve already contains the substituted variable names
        # while preserve_temp is evaluated to the symbol passed into the function.
    }
    else{
        preserve <- preserve_temp
    }

    # Remove extra first character created with substitution
    preserve <- preserve[preserve != "c"]

    # Make sure that the variables provided are part of the data frame.
    provided_preserve <- preserve
    invalid_preserve  <- preserve[!preserve %in% names(data_frame)]
    preserve          <- preserve[preserve %in% names(data_frame)]

    if (length(invalid_preserve) > 0){
        message(" ! WARNING: The provided preserve variable '", paste(invalid_preserve, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted during transposition")
    }

    # If no preserve variables are provided, create a pseudo preserve variable
    if (length(preserve) == 0){
        preserve <- ".pseudo_preserve"
        data_frame[[".pseudo_preserve"]] <- 1
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Pivot variables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Get pivot variables from provided combinations
    if (long_to_wide){
        pivot_vars <- collapse::funique(trimws(unlist(strsplit(pivot, "\\+"))))
    }
    else{
        pivot_vars <- collapse::funique(unlist(pivot, use.names = FALSE))
    }

    invalid_pivot <- pivot_vars[!pivot_vars %in% names(data_frame)]
    pivot_vars    <- pivot_vars[pivot_vars %in% names(data_frame)]

    if (length(invalid_pivot) > 0){
        message(" X ERROR: The provided pivot variable '", paste(invalid_pivot, collapse = ", "), "' is not part of\n",
                "          the data frame. Transposition will be aborted.")
        return(invisible(NULL))
    }

    if (length(pivot) == 0){
        message(" X ERROR: No valid pivot variables provided. Transposition will be aborted.")
        return(invisible(NULL))
    }

    if (length(pivot) == 1){
        if (pivot == ""){
            message(" X ERROR: No valid pivot variables provided. Transposition will be aborted.")
            return(invisible(NULL))
        }
    }

    # Make sure there is no pivot variable that is also a preserve variable.
    invalid_pivot <- pivot_vars[pivot_vars %in% preserve]

    if (length(invalid_pivot) > 0){
        message(" X ERROR: The provided pivot variable '", paste(invalid_pivot, collapse = ", "), "' is also part of\n",
                "          the preserve variables, which is not allowed. Transposition will be aborted.")
        return(invisible(NULL))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Value variables
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (long_to_wide){
        # Convert to character vectors
        values_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(values), width.cutoff = 500L)))

        if (substr(values_temp, 1, 2) == "c("){
            values <- as.character(substitute(values))
        }
        else if (!is_error(values)){
            # Do nothing. In this case values already contains the substituted variable names
            # while values_temp is evaluated to the symbol passed into the function.
        }
        else{
            values <- values_temp
        }

        # Remove extra first character created with substitution
        values <- values[values != "c"]

        # If no value variables are provided abort
        if (length(values) == 0){
            message(" X ERROR: No values provided. Transposition will be aborted.")
            return(NULL)
        }
        else if (length(values) == 1){
            if (values == ""){
                message(" X ERROR: No values provided. Transposition will be aborted.")
                return(NULL)
            }
        }

        # Make sure there is no preserve variable that is also a value variable.
        invalid_values <- values[values %in% preserve]
        values         <- values[!values %in% preserve]

        if (length(invalid_values) > 0){
            message(" ! WARNING: The provided value variable '", paste(invalid_values, collapse = ", "), "' is also part of\n",
                    "            the preserve variables. This variable will be omitted as value to transpose.")
        }

        # Make sure there is no pivot variable that is also a value variable.
        invalid_values <- values[values %in% pivot]

        if (length(invalid_values) > 0){
            message(" X ERROR: The provided value variable '", paste(invalid_values, collapse = ", "), "' is also part of\n",
                    "          the pivot variables, which is not allowed. Transposition will be aborted.")
        }

        provided_values <- values
        invalid_values  <- values[!values %in% names(data_frame)]
        values          <- values[values %in% names(data_frame)]

        if (length(invalid_values) > 0){
            message(" ! WARNING: The provided value to transpose '", paste(invalid_values, collapse = ", "), "' is not part of\n",
                    "            the data frame. This variable will be omitted as value to transpose.")
        }

        if (length(values) == 0){
            message(" X ERROR: No valid value to transpose provided. Transposition will be aborted.")
            return(NULL)
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Double entries
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Make sure provided variable list has no double entries
    provided_preserve <- preserve
    preserve          <- collapse::funique(preserve)

    if (length(provided_preserve) > length(preserve)){
        message(" ! WARNING: Some preserve variables are provided more than once. The doubled entries will be omitted.")
    }

    if (long_to_wide){
        provided_pivot <- pivot
        pivot          <- collapse::funique(pivot)

        if (length(provided_pivot) > length(pivot)){
            message(" ! WARNING: Some pivot variables are provided more than once. The doubled entries will be omitted.")
        }

        provided_values <- values
        values          <- collapse::funique(values)

        if (length(provided_values) > length(values)){
            message(" ! WARNING: Some value variables are provided more than once. The doubled entries will be omitted.")
        }

        rm(preserve_temp, provided_preserve, invalid_preserve,
           provided_pivot, invalid_pivot,
           values_temp, provided_values, invalid_values)
    }
    else{
        rm(preserve_temp, provided_preserve, invalid_preserve, invalid_pivot)
    }

    ###########################################################################
    # Transposition starts
    ###########################################################################

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Preparation", "Preparation")
    #-------------------------------------------------------------------------#

    # Check for pre summarised data, to be able to take a shortcut
    pre_summed <- data_frame |> is_pre_summed(c(preserve, pivot_vars))

    # Determine transposition method - only necessary for long to wide
    # transposition. For wide to long a named list is already given.
    # If variable names are combined with a + sign, then they will be crossed
    # during transposition.
    if (long_to_wide){
        # Store variable combinations in a list
        transpose_methods <- stats::setNames(
            lapply(pivot, function(variables){
                trimws(strsplit(variables, "\\+", fixed = FALSE)[[1]])
            }), pivot)
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Long -> Wide
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (long_to_wide){
        # Summarise data first in order to apply formats, if specified
        if (!pre_summed && !is.null(formats)){
            #-----------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next("Summarise", "Long to wide")
            #-----------------------------------------------------------------#

            group_vars <- c(preserve, pivot_vars)

            data_frame <- suppressMessages(data_frame |>
                summarise_plus(class      = group_vars,
                               values     = values,
                               statistics = "sum",
                               formats    = formats,
                               weight     = weight_var,
                               nesting    = "deepest",
                               notes      = FALSE,
                               na.rm      = na.rm)) |>
                remove_stat_extension("sum")
        }

        #---------------------------------------------------------------------#
        monitor_df <- monitor_df |> monitor_next("Transpose", "Long to wide")
        #---------------------------------------------------------------------#

        combined_df <- NULL

        # The intended behavior of the collapse pivot is to cross all variables provided as
        # 'names'. If a variable combination is provided, the variables will be crossed but
        # if they are provided separately, they will be put beside each other. Therefore
        # each pivot has to be done sequentially.
        for (method in transpose_methods){
            transpose_df <- data_frame |>
                collapse::pivot(id     = preserve,
                                names  = method,
                                values = values,
                                how    = "wider")

            # Remove NA columns afterwards.
            if (na.rm){
                none_NA_columns <- which(!is.na(names(transpose_df)) &
                                         !grepl("NA_|_NA", names(transpose_df)))
                transpose_df    <- transpose_df |> collapse::fselect(none_NA_columns)
            }

            # Combine transposed data frames
            if (is.null(combined_df)){
                combined_df <- transpose_df
            }
            # Following iterations
            else{
                # Only keep new value columns
                transpose_df <- transpose_df |> dropp(preserve)

                # Check for duplicate variable names. If any duplicate is found abort.
                duplicates <- intersect(names(combined_df), names(transpose_df))

                if (length(duplicates) > 0) {
                    message(" X ERROR: Duplicate column names found: ", paste(duplicates, collapse = ", "), ".\n",
                            "          If you are working with original values, consider making them unique by using formats.")
                    return(invisible(NULL))
                }

                # cbind current data frame to the iterations before
                combined_df <- cbind(combined_df, transpose_df)
            }
        }
    }
    # Wide to long
    else{
        combined_df <- NULL

        # Each given list entry will be transposed sequentially
        for (variable in names(pivot)){
            #-----------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next("Transpose", "Wide to long")
            #-----------------------------------------------------------------#

            # Only keep the necessary variables because otherwise all variables will be transposed.
            # Since it should be possible to transpose multiple variables into multiple categories,
            # this step is essential.
            vars_to_keep <- c(preserve, pivot[[variable]])

            # Determine new variable name. If only one new variable provided take the name from
            # the named list, otherwise use a general name.
            var_name <- "VARIABLE"

            if (length(names(pivot)) == 1){
                var_name <- variable
            }

            # After transposition into long format a BY variable is computed containing the list
            # elements name. This way one can identify which category the expressions belong to,
            # if multiple transpositions are put back together afterwards.
            transpose_df <- data_frame |>
                keep(vars_to_keep) |>
                collapse::pivot(id    = preserve,
                                names = list(variable = var_name,
                                              value   = "VALUE"),
                                how   = "longer",
                                na.rm = na.rm) |>
                collapse::fmutate(BY = variable) |>
                data.table::setcolorder(c("BY", var_name, "VALUE"), after = length(preserve))

            transpose_df[[var_name]] <- as.character(transpose_df[[var_name]])

            # Recode variable, if format is given
            if (!is.null(formats) && variable %in% names(formats)){
                #-------------------------------------------------------------#
                monitor_df <- monitor_df |> monitor_next("Summarise", "Wide to long")
                #-------------------------------------------------------------#

                # Set up new group vars for summarise. This also could be done with a
                # recode, but in case of a multilabel there has to be an additional summarise.
                # Therefore just use the summarise in both versions.
                group_vars <- c(preserve, "BY", var_name)

                # Keeping it simple and just rename the variable name to the generic one
                # in a copy of formats vector. Always copy because otherwise on the next
                # iteration there would be two entries called "VARIABLE".
                formats_copy <- formats
                names(formats_copy)[names(formats_copy) == variable] <- var_name

                transpose_df <- suppressMessages(transpose_df |>
                   summarise_plus(class      = group_vars,
                                  values     = VALUE,
                                  statistics = "sum",
                                  formats    = formats_copy,
                                  nesting    = "deepest",
                                  notes      = FALSE,
                                  na.rm      = na.rm)) |>
                    remove_stat_extension("sum") |>
                    drop_type_vars()

                transpose_df <- suppressMessages(transpose_df |> sort_plus(by = var_name))
            }

            # Combine transposed data frames
            if (is.null(combined_df)){
                # If only one new variable provided drop BY-variable
                if (length(names(pivot)) == 1){
                    transpose_df <- transpose_df |> collapse::fselect(-BY)
                }

                combined_df <- transpose_df
            }
            # Following iterations
            else{
                # rbind current data frame to the iterations before
                combined_df <- rbind(combined_df, transpose_df)
            }
        }
    }

    # Drop pseudo preserve variable if it is there
    if (names(combined_df)[[1]] == ".pseudo_preserve"){
        combined_df <- combined_df |> collapse::fselect(-.pseudo_preserve)
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'transpose_plus' execution time: ", end_time, " seconds\n")

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    combined_df
}
