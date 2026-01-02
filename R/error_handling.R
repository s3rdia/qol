#' Error Handling
#'
#' @name error_handling
#'
#' @description
#' [resolve_intersection()]: Compares if two vectors have intersecting values. If TRUE,
#' removes the intersection values from the base vector
#'
#' @param base The base vector from which to remove any intersecting values.
#' @param vector_to_check The vector for which intersections should be checked.
#' @param check_only Returns a list of invalid entries instead of a vector. Additionally
#' it doesn't throw a warning.
#'
#' @return
#' Returns a vector or list.
#'
#' @examples
#' # Resolve intersection between two vectors
#' vec1 <- c("a", "b", "c", "d")
#' vec2 <- c("e", "f", "a", "g")
#'
#' vec1 <- resolve_intersection(vec1, vec2)
#'
#' @rdname error_handling
#'
#' @export
resolve_intersection <- function(base, vector_to_check, check_only = FALSE){
    # Get argument names used in the error message below
    base_name            <- deparse(substitute(base))
    vector_to_check_name <- deparse(substitute(vector_to_check))

    # Check for intersection between the two vectors
    intersection <- base %in% vector_to_check

    invalid_base <- base[intersection]
    base         <- base[!intersection]

    # Check only, if another function will be aborted because of this check.
    if (check_only){
        if (length(invalid_base) > 0){
            return(list(invalid_base, FALSE))
        }
    }
    # Otherwise throw a warning
    else{
        if (length(invalid_base) > 0){
            message(" ! WARNING: The provided <", base_name, "> variable '", paste(invalid_base, collapse = ", "), "' is also part of\n",
                    "            the <", vector_to_check_name, "> variables. This variable will be omitted as <", base_name, "> variable during computation.")
        }
    }

    base
}


#' Check If Variable Is Part Of A Data Frame
#'
#' @description
#' [part_of_df()]: Check if variable names are part of a data frame. If not, remove them
#' from the given vector.
#'
#' @param data_frame A data frame in which to look up variable names.
#' @param var_names A character vector of variable names.
#'
#' @examples
#' # Check if variables are part of a data frame
#' my_data   <- dummy_data(100)
#' var_names <- c("year", "state", "age", "test")
#'
#' var_names <- my_data |> part_of_df(var_names)
#'
#' @rdname error_handling
#'
#' @export
part_of_df <- function(data_frame, var_names, check_only = FALSE){
    # Get argument name used in the error message below
    vector_name <- deparse(substitute(var_names))

    # Check if variable names are part of the data frame
    intersection <- var_names %in% names(data_frame)

    invalid_vars <- var_names[!intersection]
    var_names    <- var_names[intersection]

    # Check only, if another function will be aborted because of this check.
    if (check_only){
        if (length(invalid_vars) > 0){
            return(list(invalid_vars, FALSE))
        }
    }
    # Otherwise throw a warning
    else{
        if (length(invalid_vars) > 0){
            message(" ! WARNING: The provided <", vector_name, "> variable '", paste(invalid_vars, collapse = ", "), "' is not part of\n",
                    "            the data frame. This variable will be omitted during computation.")
        }
    }

    var_names
}


#' Remove Doubled Values From A Vector
#'
#' @description
#' [remove_doubled_values()]: Remove values from a vector that appear more than once.
#'
#' @examples
#' # Remove doubled values
#' var_names <- c("year", "state", "state", "age")
#'
#' var_names <- remove_doubled_values(var_names)
#'
#' @rdname error_handling
#'
#' @export
remove_doubled_values <- function(var_names){
    # Get argument name used in the error message below
    vector_name <- deparse(substitute(var_names))

    # Only keep unique values
    var_names_unique <- collapse::funique(var_names)

    if (length(var_names) > length(var_names_unique)){
        message(" ! WARNING: Some <", vector_name, "> variables are provided more than once. The doubled entries will be omitted.")
    }

    var_names_unique
}


#' Check For Suitable Weight Variable
#'
#' @description
#' [check_weight()]: Check if a weight variable was provided. If TRUE, check whether it
#' can be used else add a temporary weight variable.
#'
#' @examples
#' # Check the provided weight variable
#' var_names <- my_data |> check_weight("weight")
#'
#' @rdname error_handling
#'
#' @export
check_weight <- function(data_frame, var_names){
    # Create temporary weight column if none is provided.
    # Also get the name of the weight variable as string.
    if (is.null(var_names) || length(var_names) > 1){
        data_frame[[".temp_weight"]] <- 1

        if (length(var_names) > 1){
            message(" ! WARNING: Only one variable for weight allowed. Evaluations will be unweighted.")
        }
    }
    else if (!var_names %in% names(data_frame)){
        data_frame[[".temp_weight"]] <- 1

        message(" ! WARNING: Provided weight variable is not part of the data frame. Unweighted results will be computed.")
    }
    else if (!is.numeric(data_frame[[var_names]])){
        data_frame[[".temp_weight"]] <- 1

        message(" ! WARNING: Provided weight variable is not numeric. Unweighted results will be computed.")
    }
    else{
        # NA values in weight lead to errors therefor convert them to 0
        if (anyNA(data_frame[[var_names]])){
            message(" ~ NOTE: Missing values in weight variable '", var_names, "' will be converted to 0.")
        }
        data_frame[[var_names]][is.na(data_frame[[var_names]])] <- 0

        # @Hack: so I don't have to check if .temp_weight exists later on
        data_frame[[".temp_weight"]] <- data_frame[[var_names]]
    }

    data_frame
}
