#' Keep and Drop Variables Inside a Data Frame
#'
#' @name keep_dropp
#'
#' @param data_frame A data frame which should be reduced to (keep) or by (drop) the specified variables.
#' @param ... The variable names to keep/drop.
#' @param order_vars keep: At the end variables are ordered as specified in the command.
#'
#' @return Returns a reduced data table.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' new_dt1 <- my_data |> keep(year, age, sex)
#' new_dt2 <- my_data |> keep(weight, income, education, sex, order_vars = TRUE)
#' new_dt3 <- my_data |> dropp(year, age, sex)
#'
#' # Also works with characters
#' new_dt4 <- my_data |> keep("year", "age", "sex")
#' new_dt5 <- my_data |> dropp("year", "age", "sex")
#'
#' # Or variable names stored as a character vector
#' var_names <- c("age", "sex", "income", "weight")
#'
#' new_dt6 <- my_data |> keep(var_names)
#' new_dt7 <- my_data |> dropp(var_names)
#'
#' @rdname keep_dropp
#'
#' @keywords internal
NULL


#' @description
#' [keep()] enables you to put in a vector of variable names which then are kept
#' inside the given data frame. All other variables are dropped.
#'
#' @rdname keep_dropp
#'
#' @export
keep <- function(data_frame, ..., order_vars = FALSE){
    # Convert to character vectors
    variables <- args_to_char(...)

    if (length(variables) == 0){
        return(data_frame)
    }

    # Check if all variables are part of the data frame
    provided_vars <- variables
    invalid_vars  <- variables[!variables %in% names(data_frame)]
    variables     <- variables[variables %in% names(data_frame)]

    if (length(invalid_vars) > 0){
        message(" ! WARNING: The provided variable to keep '", paste(invalid_vars, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted.")
        rm(provided_vars, invalid_vars)
    }

    # Convert to data.table if necessary
    if (!data.table::is.data.table(data_frame)){
        data_frame <- data.table::as.data.table(data_frame)
    }

    # Get the negative vector of variables to keep and every variable name inside
    # given data frame to produce a vector with the variables to drop
    name_vector <- names(data_frame)
    drop_variables <- name_vector[!name_vector %in% variables]

    # Use data.table function set to drop the desired variables
    data_frame <- data_frame |> collapse::fselect(variables)

    # Order variables
    if (order_vars){
        data.table::setcolorder(data_frame, variables)
    }

    data_frame
}


#' @description
#' [dropp()] enables you to put in a vector of variable names which then are dropped
#' from the given data frame. All other variables are kept.
#'
#' @rdname keep_dropp
#'
#' @export
dropp <- function(data_frame, ...){
    # Convert to character vectors
    variables <- args_to_char(...)

    if (length(variables) == 0){
        return(data_frame)
    }

    # Check if all variables are part of the data frame
    provided_vars <- variables
    invalid_vars  <- variables[!variables %in% names(data_frame)]
    variables     <- variables[variables %in% names(data_frame)]

    if (length(invalid_vars) > 0){
        message(" ! WARNING: The provided variable to drop '", paste(invalid_vars, collapse = ", "), "' is not part of\n",
                "            the data frame. This variable will be omitted.")

        if (length(variables) == 0){
            return(data_frame)
        }
    }

    # Convert to data.table if necessary
    if (!data.table::is.data.table(data_frame)){
        data_frame <- data.table::as.data.table(data_frame)
    }

    # Use data.table function set to drop the desired variables
    collapse::fselect(data_frame, variables) <- NULL

    data_frame
}
