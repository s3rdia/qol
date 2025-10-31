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
#' @param var_name The name of the newly created variable.
#' @param value [retain_value]: One or multiple variables of which a value should be retained.
#'
#' [retain_sum]: One or multiple variables of which the sum should be retained.
#' @param by By group in which to compute the retained variable.
#' @param first [mark_case()]: If TRUE marks the first case within a group, otherwise
#' the last case.
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
#' a value from the first person in a household to all other persons of the same household.
#'
#' @return
#' [running_number()]: Returns the data frame with a new variable containing a running number.
#'
#' [mark_case()]: Returns the data frame with a new variable marking first or last cases.
#'
#' [retain_value()]: Return the data frame with a new variable containing a retained value.
#'
#' [retain_sum()]: Return the data frame with a new variable containing a retained sum.
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
#' # Mark first and last cases
#' my_data <- my_data |>
#'     mark_case(by = household_id) |>
#'     mark_case(var_name = "last", by = household_id, first = FALSE)
#'
#' # Retain first value inside a group
#' my_data <- my_data |>
#'     retain_value(var_name = c("household_weight", "household_icome"),
#'                  value    = c(weight, income),
#'                  by       = c(state, household_id))
#'
#' # Retain sum inside a group
#' my_data <- my_data |>
#'     retain_sum(var_name = c("weight_hh_sum", "icome_hh_sum"),
#'                value    = c(weight, income),
#'                by       = c(state, household_id))
#'
#' @rdname retain
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
#' @rdname retain
#'
#' @export
running_number <- function(data_frame,
                           var_name = "run_nr",
                           by       = NULL){
    # Measure the time
    start_time <- Sys.time()

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

    # If the user specified a vector of by variables, only take the last one, as this is
    # most likely the group in which the running number should be generated.
    if (length(by) > 1){
        by <- by[length(by)]

        message(" ~ NOTE: Running number is generated in current data frame order. Only last variable\n",
                "         '", by, "' inside provided by vector will be used.")
    }

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

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'running_number' execution time: ", end_time, " seconds\n")

    data_frame
}


#' @description
#' [mark_case()] sets a flag for the first or last case within the provided by group.
#'
#' @rdname retain
#'
#' @export
mark_case <- function(data_frame,
                      var_name = "first",
                      by       = NULL,
                      first    = TRUE){
    # Measure the time
    start_time <- Sys.time()

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

    # If the user specified a vector of by variables, only take the last one, as this is
    # most likely the group in which the first and last cases should be marked.
    if (length(by) > 1){
        by <- by[length(by)]

        message(" ~ NOTE: Cases are marked in current data frame order. Only last variable\n",
                "         '", by, "' inside provided by vector will be used.")
    }

    # In case by variable is specified, mark first/last case per group in current order
    if (length(by) == 1){
        # The trick in marking cases is, to offset check the same vector by one.
        # A TRUE offset check means, that the value has changed. FALSE means value is the
        # same, so we are still in the same group.
        # Mark first cases by placing the missing TRUE value at the front, meaning: Shift down the whole
        # vector by one.
        if (first){
            data_frame[[var_name]] <- c(TRUE, data_frame[[by]][-1] != data_frame[[by]][-nrow(data_frame)])
        }
        # Mark last cases by placing the missing TRUE value at the back, meaning: Shift up the whole
        # vector by one.
        else{
            data_frame[[var_name]] <- c(data_frame[[by]][-1] != data_frame[[by]][-nrow(data_frame)], TRUE)
        }
    }
    # In case no by variable is specified, mark first/last of whole data frame
    else{
        # Mark first case
        if (first){
            data_frame[[var_name]]  <- FALSE
            data_frame[1, var_name] <- TRUE
        }
        # Mark last cases
        else{
            data_frame[[var_name]] <- FALSE
            data_frame[nrow(data_frame), var_name] <- TRUE
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'mark_cases' execution time: ", end_time, " seconds\n")

    data_frame
}


#' @description
#' [retain_value()] retains the first value for all cases of the same group and saves
#' it into a new variable.
#'
#' @rdname retain
#'
#' @export
retain_value <- function(data_frame,
                         var_name = "retain_value",
                         value,
                         by       = NULL){
    # Measure the time
    start_time <- Sys.time()

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

    # Generate pseudo var if by is NULL
    if (is.null(by)){
        data_frame[[".pseudo_by"]] <- 1
        by <- ".pseudo_by"
    }

    # Convert to character vectors
    value_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(value))))

    if (substr(value_temp, 1, 2) == "c("){
        value <- as.character(substitute(value))
    }
    else if (!is_error(value)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while variables_temp is evaluated to the symbol passed into the function.
    }
    else{
        value <- value_temp
    }

    # Remove extra first character created with substitution
    value <- value[value != "c"]

    # Abort if no value provided
    if (length(value) == 1){
        if (value == ""){
            message(" X ERROR: Must provide a value to retain. Retain will be aborted.")
            return(data_frame)
        }
    }

    # In case by variable is specified, mark first/last case per group in current order
    data_frame <- suppressMessages(data_frame |>
            summarise_plus(class      = by,
                           values     = value,
                           statistics = c("first"),
                           nesting    = "deepest",
                           merge_back = TRUE) |>
            dropp(".pseudo_by"))

    # If there are as much new variable names provided, as there are values given, then rename them
    if (length(value) == length(var_name)){
        data_frame <- data_frame |> collapse::frename(stats::setNames(var_name, paste0(value, "_first")))
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'retain_value' execution time: ", end_time, " seconds\n")

    data_frame
}


#' @description
#' [retain_sum()] retains the summarised values for all cases of the same group and saves
#' it into a new variable.
#'
#' @rdname retain
#'
#' @export
retain_sum <- function(data_frame,
                       var_name = "retain_sum",
                       value,
                       by       = NULL){
    # Measure the time
    start_time <- Sys.time()

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

    # Generate pseudo var if by is NULL
    if (is.null(by)){
        data_frame[[".pseudo_by"]] <- 1
        by <- ".pseudo_by"
    }

    # Convert to character vectors
    value_temp <- sub("^list\\(", "c(", gsub("\"", "", deparse(substitute(value))))

    if (substr(value_temp, 1, 2) == "c("){
        value <- as.character(substitute(value))
    }
    else if (!is_error(value)){
        # Do nothing. In this case variables already contains the substituted variable names
        # while variables_temp is evaluated to the symbol passed into the function.
    }
    else{
        value <- value_temp
    }

    # Remove extra first character created with substitution
    value <- value[value != "c"]

    # Abort if no value provided
    if (length(value) == 1){
        if (value == ""){
            message(" X ERROR: Must provide a value to retain. Retain will be aborted.")
            return(data_frame)
        }
    }

    # In case by variable is specified, mark first/last case per group in current order
    data_frame <- suppressMessages(data_frame |>
           summarise_plus(class      = by,
                          values     = value,
                          statistics = c("sum"),
                          nesting    = "deepest",
                          merge_back = TRUE) |>
           dropp(".pseudo_by"))

    # If there are as much new variable names provided, as there are values given, then rename them
    if (length(value) == length(var_name)){
        data_frame <- data_frame |> collapse::frename(stats::setNames(var_name, paste0(value, "_sum")))
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("\n- - - 'retain_sum' execution time: ", end_time, " seconds\n")

    data_frame
}
