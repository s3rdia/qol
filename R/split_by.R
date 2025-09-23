#' Split Data Frame by Variable Expressions or Condition
#'
#' @name split_by
#'
#' @description
#' Split up a data frame based on variable expressions or on conditions to receive
#' multiple smaller data frames.
#'
#' @param data_frame A data frame which should be split up into multiple data frames.
#' @param variable In [split_by_var()] pass in a variable name which expressions
#' are used for splitting up the data frame.
#' @param ... In [split_by_condition()] pass in one or multiple conditions on which
#' the provided data frame should be splitted.
#' @param inverse In [split_by_condition()] if only one condition is provided, the
#' data frame can be split into two parts. The second returned data frame will be the
#' inverse group of the first.
#'
#' @details
#' [split_by()] is based on the explicit Output from 'SAS'. With the Output function
#' one can - among other things - explicitly tell 'SAS' which observation to output into
#' which data set. Which enables the user to output one observation into one or multiple
#' data sets.
#'
#' Instead of subsetting the same data frame multiple times manually, you can subset it
#' multiple times at once with this function.
#'
#' @return [split_by_var()]: Returns a list of data frames split by variable expressions.
#' The lists names are the variable expressions.
#'
#' [split_by_condition()]: Returns a list of data frames split conditionally.
#' The lists names are the conditions.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Split by variable expressions
#' split_var_df <- my_data |> split_by_var(sex)
#'
#' # Split by conditions
#' split_cond_df <- my_data |> split_by_condition(sex == 1 & age <  18,
#'                                                sex == 2 & age >= 18)
#'
#' # Split by condition with inverse group
#' split_inv_df <- my_data |> split_by_condition(sex == 1, inverse = TRUE)
#'
#' @rdname split_by
#'
#' @export
split_by_var <- function(data_frame, variable){
    # Measure the time
    start_time <- Sys.time()

    # Convert to character
    variable <- gsub("\"", "", deparse(substitute(variable)))

    if (!variable %in% names(data_frame)){
        message(" X ERROR: Variable '", variable, "' not found in the input data frame. Splitting will be aborted.")
        return(data_frame)
    }

    # Get unique expressions
    expressions <- unique(data_frame[[variable]])

    # Pre define list
    data_list        <- vector("list", length(expressions))
    names(data_list) <- expressions
    data_list        <- data_list[!is.na(names(data_list))]

    # Split data frame by variable expressions and store the split data frames into list
    for (expression in expressions){
        if (!is.na(expression)){
            data_list[[as.character(expression)]] <- data_frame |>
                collapse::fsubset(data_frame[[variable]] == expression)
        }
        else{
            data_list[["Missing"]] <- data_frame |>
                collapse::fsubset(is.na(data_frame[[variable]]))
        }
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'split_by_var' execution time: ", end_time, " seconds")

    data_list
}


#' @rdname split_by
#'
#' @export
split_by_condition <- function(data_frame, ..., inverse = FALSE){
    # Measure the time
    start_time <- Sys.time()

    # Convert to character
    conditions <- as.list(substitute(list(...)))[-1]

    # Inverse condition not wanted or multiple conditions specified
    if (!inverse | length(conditions) > 1){
        if (inverse){
            message(" ! WARNING: Inverse condition only possible if only one condition is provided.")
        }

        # Pre define list
        data_list        <- vector("list", length(conditions))
        names(data_list) <- conditions

        # Split data frame by conditions and store the split data frames into list
        index <- 1

        for (condition in conditions){
            # Evaluate the condition first to make it usable below
            real_condition <- eval(substitute(condition), envir = data_frame, enclos = parent.frame())

            data_list[[index]] <- data_frame |>
                collapse::fsubset(real_condition)

            index <- index + 1
        }
    }
    else{
        # Pre define list
        data_list        <- vector("list", 2)
        names(data_list) <- c(conditions, "inverse")

        # Evaluate the condition first to make it usable below
        real_condition <- eval(conditions[[1]], envir = data_frame, enclos = parent.frame())
        real_condition[is.na(real_condition)] <- FALSE

        # First the provided condition
        data_list[[1]] <- data_frame |>
            collapse::fsubset(real_condition)

        # Reverse condition and save as separate entry
        data_list[[2]] <- data_frame |>
            collapse::fsubset(!real_condition)
    }

    end_time <- round(difftime(Sys.time(), start_time, units = "secs"), 3)
    message("- - - 'split_by_condition' execution time: ", end_time, " seconds")

    data_list
}
