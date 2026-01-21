#' Different Facets of Retain
#'
#' @name retain
#'
#' @description
#' These retain functions all have one thing in common: transferring a value from one case
#' to the next. What they make out of this functionality can be quiet different. Therefor
#' there is a function for each different use case.
#'
#' @param data_frame The data frame in which to compute retained variables.
#' @param by By group in which to compute the retained variable.
#' @param values One or multiple variables of which a value should be retained.
#'
#' @details
#' The functions listed here are based on the 'SAS' function retain. On a very basic level retain
#' can do two things, depending on the position in the 'SAS' code: It can either sort variables
#' column wise or it can - since it works row wise - remember a value from one row to the next.
#' The functions here concentrate on the second part.
#'
#' Remembering a value from a previous observation offers multiple use cases. E.g. always adding
#' +1 to the previous case creates a running number. Or if an observation knows the value of the
#' previous one, it can check whether it is of the same value or another, e.g. to mark first or last
#' cases within a group.
#'
#' In it's simplest form it can remember a value from the first observation and transfer it to all
#' other observations.
#'
#' All of these functions work on the whole data frame as well as on groups, e.g. to transfer
#' a value from the first person in a household to all other persons of the same household.ame retain
#'
#' @keywords internal
NULL


#' Compute Running Numbers
#'
#' @description
#' [running_number()] computes running numbers in a data frame. Without specifying
#' a by variable results in the row number. With by variable computes the running
#' number within each group of expressions.
#'
#' @return
#' [running_number()]: Returns a vector containing a running number.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Get row numbers
#' my_data[["run_nr"]] <- my_data |> running_number()
#'
#' # Running number per variable expression
#' my_data[["run_nr_by"]] <- my_data |> running_number(by = year)
#'
#' @rdname retain
#'
#' @export
running_number <- function(data_frame,
                           by = NULL){
    # Measure the time
    start_time <- Sys.time()

    ###########################################################################
    # Error handling
    ###########################################################################

    # Convert to character vectors
    by <- get_origin_as_char(by, substitute(by))

    # Make sure that the variables provided are part of the data frame.
    by <- data_frame |> part_of_df(by)

    ###########################################################################
    # Retain
    ###########################################################################

    # If the user specified a vector of by variables, only take the last one, as this is
    # most likely the group in which the running number should be generated.
    if (length(by) > 1){
        message(" ~ NOTE: Running number is generated in current data frame order. Only last variable\n",
                "         '", by, "' inside provided by vector will be used.")

        by <- by[length(by)]
    }

    # In case of a by variable
    if (length(by) == 1){
        variable <- stats::ave(seq_len(collapse::fnrow(data_frame)),
                                       data.table::rleid(data_frame[[by]]),
                                       FUN = seq_along)
    }
    # In case of no by variable
    else{
        variable <- seq_len(collapse::fnrow(data_frame))
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'running_number' execution time: ", end_time, " seconds\n")

    variable
}


#' @description
#' [mark_case()] sets a flag for the first or last case within the provided by group.
#'
#' @param first [mark_case()]: If TRUE marks the first case within a group, otherwise
#' the last case.
#'
#' @return
#' [mark_case()]: Returns a vector containing a the marking for first or last cases.
#'
#' @examples
#' # Mark first and last cases
#' my_data[["first"]] <- my_data |> mark_case(by = household_id)
#' my_data[["last"]]  <- my_data |> mark_case(by    = household_id,
#'                                            first = FALSE)
#'
#' @rdname retain
#'
#' @export
mark_case <- function(data_frame,
                      by       = NULL,
                      first    = TRUE){
    # Measure the time
    start_time <- Sys.time()

    ###########################################################################
    # Error handling
    ###########################################################################

    # Convert to character vectors
    by <- get_origin_as_char(by, substitute(by))

    # Make sure that the variables provided are part of the data frame.
    by <- data_frame |> part_of_df(by)

    # If the user specified a vector of by variables, only take the last one, as this is
    # most likely the group in which the first and last cases should be marked.
    if (length(by) > 1){
        message(" ~ NOTE: Cases are marked in current data frame order. Only last variable\n",
                "         '", by[length(by)], "' inside provided <by> vector will be used.")

        by <- by[length(by)]
    }

    ###########################################################################
    # Retain
    ###########################################################################

    # In case by variable is specified, mark first/last case per group in current order
    if (length(by) == 1){
        # The trick in marking cases is, to offset check the same vector by one.
        # A TRUE offset check means, that the value has changed. FALSE means value is the
        # same, so we are still in the same group.
        # Mark first cases by placing the missing TRUE value at the front, meaning: Shift down the whole
        # vector by one.
        if (first){
            variable <- c(TRUE, data_frame[[by]][-1] != data_frame[[by]][-collapse::fnrow(data_frame)])
        }
        # Mark last cases by placing the missing TRUE value at the back, meaning: Shift up the whole
        # vector by one.
        else{
            variable <- c(data_frame[[by]][-1] != data_frame[[by]][-collapse::fnrow(data_frame)], TRUE)
        }
    }
    # In case no by variable is specified, mark first/last case of whole data frame
    else{
        number_of_obersvations <- collapse::fnrow(data_frame)

        # Create vector of all FALSE values with the length of the provided data frame
        variable <- logical(number_of_obersvations)

        if (first){
            variable[1] <- TRUE
        }
        else{
            variable[number_of_obersvations] <- TRUE
        }
    }

    # Output as 1/0 instead of TRUE/FALSE
    variable <- as.integer(variable)

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'mark_cases' execution time: ", end_time, " seconds\n")

    variable
}


#' @description
#' [retain_value()] retains the first value for all cases of the same group and saves
#' it into a new variable.
#'
#' @return
#' [retain_value()]: Returns a vector containing a retained value.
#'
#' @examples
#' # Retain first value without grouping
#' my_data[["first_weight"]] <- my_data |> retain_value(value = weight)
#'
#' # Retain first value inside a group for multiple variables
#' my_data[, c("household_weight", "household_icome")] <- my_data |>
#'     retain_value(values = c(weight, income),
#'                  by     = c(state, household_id))
#'
#' @rdname retain
#'
#' @export
retain_value <- function(data_frame,
                         values,
                         by = NULL){
    # Measure the time
    start_time <- Sys.time()

    ###########################################################################
    # Error handling
    ###########################################################################

    # Convert to character vectors
    by <- get_origin_as_char(by, substitute(by))

    # Make sure that the variables provided are part of the data frame.
    by <- data_frame |> part_of_df(by)

    # Generate pseudo var if by is NULL
    if (is.null(by) || length(by) == 0){
        data_frame[[".pseudo_by"]] <- 1
        by <- ".pseudo_by"
    }

    # Convert to character vectors
    values <- get_origin_as_char(values, substitute(values))

    # Make sure that the variables provided are part of the data frame.
    values <- data_frame |> part_of_df(values)

    # Abort if no value provided
    if (length(values) <= 1){
        if (length(values) == 0 || values == ""){
            message(" X ERROR: Must provide <values> to retain. Retain will be aborted.")
            return(invisible(data_frame))
        }
    }

    ###########################################################################
    # Retain
    ###########################################################################

    group <- collapse::GRP(data_frame, by)

    # If there are as much new variable names provided, as there are values given, then rename them
    data_frame <- data_frame[values] |> collapse::ffirst(g = group, TRA = "fill")

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'retain_value' execution time: ", end_time, " seconds\n")

    if (length(values) == 1){
        data_frame[[1]]
    }
    else{
        as.list(data_frame)
    }
}


#' @description
#' [retain_sum()] retains the summarised values for all cases of the same group and saves
#' it into a new variable.
#'
#' @return
#' [retain_sum()]: Returns a vector containing a retained sum.
#'
#' @examples
#' # Retain sum without grouping
#' my_data[["total_sum"]] <- my_data |> retain_sum(values = weight)
#'
#' # Retain sum inside a group for multiple variables
#' my_data[, c("weight_sum", "income_sum")] <- my_data |>
#'     retain_sum(values = c(weight, income),
#'                  by   = c(state, household_id))
#'
#' @rdname retain
#'
#' @export
retain_sum <- function(data_frame,
                       values,
                       by       = NULL){
    # Measure the time
    start_time <- Sys.time()

    ###########################################################################
    # Error handling
    ###########################################################################

    # Convert to character vectors
    by <- get_origin_as_char(by, substitute(by))

    # Make sure that the variables provided are part of the data frame.
    by <- data_frame |> part_of_df(by)

    # Generate pseudo var if by is NULL
    if (is.null(by) || length(by) == 0){
        data_frame[[".pseudo_by"]] <- 1
        by <- ".pseudo_by"
    }

    # Convert to character vectors
    values <- get_origin_as_char(values, substitute(values))

    # Make sure that the variables provided are part of the data frame.
    values <- data_frame |> part_of_df(values)

    # Abort if no value provided
    if (length(values) <= 1){
        if (length(values) == 0 || values == ""){
            message(" X ERROR: Must provide a <values> to retain. Retain will be aborted.")
            return(invisible(data_frame))
        }
    }

    # Abort if non-numeric value provided
    if (!all(collapse::vtypes(data_frame[values]) %in% c("numeric", "integer", "double"))){
        message(" X ERROR: <Values> must be numeric. Retain will be aborted.")
        return(invisible(data_frame))
    }

    ###########################################################################
    # Retain
    ###########################################################################

    # In case by variable is specified, compute sum per group
    data_frame <- suppressMessages(data_frame |>
           summarise_plus(class      = by,
                          values     = values,
                          statistics = "sum",
                          nesting    = "deepest",
                          merge_back = TRUE))

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'retain_sum' execution time: ", end_time, " seconds\n")

    if (length(values) == 1){
        data_frame[[collapse::fncol(data_frame)]]
    }
    else{
        as.list(data_frame[-(1:(collapse::fncol(data_frame) - length(values)))])
    }
}


#' @description
#' [retain_variables()] orders the provided variables to the front or back of the
#' data frame. If a variable is not part of the data frame it will be added with
#' all NA values at the desired position.
#'
#' @param ... [retain_variables()]: Put in single variable names or variable ranges
#' (var_name1:var_name10) which should be ordered to the front or back of the data frame.
#' It is also possible to provide none existent variable names which will then be added
#' to the data frame.
#' @param retain_variables [retain_variables()]: FALSE by default. If TRUE puts the
#' variables at the end of the data frame instead of the beginning.
#'
#' @return
#' [retain_sum()]: Return the data frame with a new variable containing a retained sum.
#'
#' @examples
#' # Retain columns inside data frame, which orders them to the front
#' my_data <- my_data |> retain_variables(age, sex, income)
#'
#' # Retain columns inside data frame, but order them to the end.
#' # Variable ranges can also be used.
#' my_data <- my_data |> retain_variables(age:income, order_last = TRUE)
#'
#' # Retain columns inside data frame and add new variables with all NA values
#' my_data <- my_data |> retain_variables(age, sex, income, status1:status5)
#'
#' # You can also use the colon as a placeholder for any text
#' start1   <- my_data |> retain_variables("s:")   # Variable names start with "s"
#' end1     <- my_data |> retain_variables(":id")  # Variable names end with "id"
#' contain1 <- my_data |> retain_variables(":on:") # Variable names which contain "on"
#'
#' @rdname retain
#'
#' @export
retain_variables <- function(data_frame, ..., order_last = FALSE){
    # Measure the time
    start_time <- Sys.time()

    # Translate ... into a list if possible
    retain_list <- tryCatch({
        # Force evaluation to see if it exists
        dots_to_char(...)
    }, error = function(e){
        # Evaluation failed
        NULL
    })

    if (is.null(retain_list)){
        message('X ERROR: Unknown object found. Retaining will be aborted.')
        return(invisible(data_frame))
    }

    # Check if there are any colons in the selection and deparse variables accordingly
    var_names <- names(data_frame)
    variables <- character(0)

    for (variable in retain_list){
        if (grepl(":", variable, fixed = TRUE)){
            parts <- strsplit(variable, ":", fixed = TRUE)[[1]]

            # If there is only one part the selection is "text:" and if the first element
            # is empty the selection was either ":text" or ":text:". In these cases a selection
            # of variables inside the data frame happens.
            if (length(parts) == 1 || parts[[1]] == ""){
                variables <- c(variables, data_frame |> deparse_colon(variable))
            }
            # If both variables are not part of the data frame, add the variables as new ones
            else if (!any(c(parts[1], parts[2]) %in% var_names)){
                data_frame <- data_frame |> add_variable_range(variable)
                variables  <- c(variables, data_frame |> vars_between(parts[1], parts[2]))
            }
            # Add variables in range to retain vector
            else{
                variables <- c(variables, data_frame |> vars_between(parts[1], parts[2]))
            }
        }
        # If element is just a single variable
        else{
            # If variable is not part of the data frame add it as empty variable
            if (!variable %in% var_names){
                new_columns <- collapse::qDF(matrix(NA_integer_,
                                                    nrow = collapse::fnrow(data_frame),
                                                    ncol = 1))

                names(new_columns) <- variable

                # Add new empty variables to data frame
                data_frame <- data_frame |> collapse::add_vars(new_columns)
            }

            # Add variables in range to retain vector
            variables <- c(variables, variable)
        }
    }

    # Only keep unique values. It can happen that a colon selection at the end of the
    # provided variable list catches variables another time which already have been
    # added to the variable vector.
    variables <- variables |> collapse::funique()

    # Reorder variables
    if (!order_last){
        ordered_df <- data.table::copy(data_frame) |>
            data.table::setcolorder(variables,
                                    before = 1)
    }
    else{
        ordered_df <- data.table::copy(data_frame) |>
            data.table::setcolorder(variables,
                                    after = collapse::fncol(data_frame))
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'retain_variables' execution time: ", end_time, " seconds")

    ordered_df
}
