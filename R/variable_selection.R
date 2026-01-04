#' Get Variable Names Which Are Not Part Of The Given Vector
#'
#' @description
#' If you have stored variable names inside a character vector, this function gives you
#' the inverse variable name vector.
#'
#' @param data_frame The data frame from which to take the variable names.
#' @param var_names A character vector of variable names.
#'
#' @return
#' Returns the inverse vector of variable names compared to the given vector.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Get variable names
#' var_names <- c("year", "age", "sex")
#' other_names <- my_data |> inverse(var_names)
#'
#' # Can also be used to just get all variable names
#' all_names <- my_data |> inverse()
#'
#' @export
inverse <- function(data_frame, var_names){
    # Convert to character vectors
    var_names <- get_origin_as_char(var_names, substitute(var_names))

    names(data_frame)[!names(data_frame) %in% var_names]
}


#' Get All Variable Names Between Two Variables
#'
#' @description
#' Get all the variable names inside a data frame between two variables (including the provided ones)
#' as a character vector
#'
#' @param data_frame The data frame from which to take the variable names.
#' @param from Starting variable of variable range.
#' @param to Ending variable of variable range.
#'
#' @return
#' Returns a character vector of variable names.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Get variable names
#' var_names <- my_data |> vars_between(state, income)
#'
#' # Get variable names in reverse order
#' vars_reverse <- my_data |> vars_between(income, state)
#'
#' # If you only provide "from" or "to" you get all variable names from a point to
#' # the end or from the beginning to a given point.
#' vars_from <- my_data |> vars_between(state)
#' vars_to   <- my_data |> vars_between(to = state)
#'
#' # Or just get all variable names
#' vars_all <- my_data |> vars_between()
#'
#' @export
vars_between <- function(data_frame, from, to){
    # Convert to character vectors
    from <- get_origin_as_char(from, substitute(from))
    to   <- get_origin_as_char(to, substitute(to))

    # Check if a vector and not a single variable name was given. If a vector was
    # given, only consider the first element.
    if (length(from) > 1 || length(to) > 1){
        message(" ~ NOTE: Only single variable names allowed for 'from' and 'to'. The respective\n",
                "         first element will be considered.")

        from <- from[1]
        to   <- to[1]
    }

    # Check if "from" and "to" variable names are part of the data frame.
    # If not, adjust accordingly.
    var_names <- names(data_frame)

    if (from != "" && !from %in% var_names){
        message(" ! WARNING: 'from' variable '", from, "' is not part of the data frame.\n",
                "            Selection will start from the first variable.")

        from = ""
    }
    if (to != "" && !to %in% var_names){
        message(" ! WARNING: 'to' variable '", to, "' is not part of the data frame.\n",
                "            Selection will go until the last variable.")

        to = ""
    }

    # If "from" is not entered, return all names from the first position to the end
    if (from == ""){
        start <- 1L
    }
    # Else find position in data frame names
    else{
        start <- match(from, var_names)
    }

    # If "to" is not entered, return all names up to the last position from the start
    if (to == ""){
        end <- length(var_names)
    }
    # Else find position in data frame names
    else{
        end <- match(to, var_names)
    }

    # Return
    columns <- start:end
    names(data_frame[columns])
}
