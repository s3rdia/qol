#' Convert Ellipsis to Character Vector
#'
#' @description
#' When you define a function and want the user to be able to pass variable
#' names without the need to have them stored in a vector c() or list() beforehand and without
#' putting the names into quotation marks, you can convert this variable list passed as ...
#' into a character vector.
#'
#' Note: If the user passes a list of characters it is returned as given.
#'
#' @param ... Used for variable names listed in ... without the need to put them in c() or list()
#'
#' @return
#' Returns a character vector
#'
#' @examples
#' # Example function
#' print_vnames <- function(...){
#'     var_names <- args_to_char(...)
#'     print(var_names)
#' }
#'
#' print_vnames(age, sex, income, weight)
#' print_vnames("age", "sex", "income", "weight")
#'
#' # You can also pass in a character vector, if you have stored variable names elsewhere
#' var_names <- c("age", "sex", "income", "weight")
#' print_vnames(var_names)
#'
#' @export
args_to_char <- function(...){
    tryCatch(unlist(list(...)),
             error = function(e) single_args_to_char(...))
}


#' Convert Ellipsis to Character Vector
#'
#' @description
#' Converts ellipsis to character vector and split it up into single character elements.
#'
#' @param ... Used for variable names listed in ... without the need to put them in c() or list()
#'
#' @return
#' Returns a character vector..
#'
#' @noRd
single_args_to_char <- function(...){
    # First get all arguments as one string
    arg_vector <- gsub("\"", "", deparse(substitute(c(...)), width.cutoff = 500L))

    # Split up the single arguments
    arguments  <- strsplit(sub("c\\(", "", sub(")", "", arg_vector)), ",")

    # Get rid of any extra blanks
    gsub("\\s", "", unlist(arguments))
}


#' Check and Convert to Numeric
#'
#' @name convert_numeric
#'
#' @description
#' [is_numeric()] checks whether all values of the given variable, that are not
#' NA, are numerical.
#'
#' @param variable A vector with values to check.
#' @param data_frame A data frame containing variables to convert.
#' @param variables Variables from the data frame which should be converted
#' to numeric.
#'
#' @return
#' [is_numeric()] returns TRUE if all none NA values are numerical, otherwise FALSE.
#'
#' @examples
#' # Check if vectors contain only numeric values
#' test_vector1 <- c(1, 2, 3, NA, 4, 5)
#' test_vector2 <- c(1, 2, "Hello", NA, 4, 5)
#'
#' numeric_check1 <- is_numeric(test_vector1)
#' numeric_check2 <- is_numeric(test_vector2)
#'
#' @rdname convert_numeric
#'
#' @export
is_numeric <- function(variable){
    # Just try converting without errors surpressed
    result <- suppressWarnings(as.numeric(variable))

    # If NAs are equal before and after conversion was successful.
    # If there are more NAs after than before, then there where some character values.
    all(is.na(result) == is.na(variable))
}


#' @description
#' [convert_numeric()] converts all given variables to numeric if possible. If
#' a variable contains none numerical values (not including NAs), the variable
#' will not be converted.
#'
#' @return
#' [convert_numeric()] returns the same data frame with converted variables where possible.
#'
#' @examples
#' # Convert variables in a data frame to numeric where possible
#' test_df <- data.frame(var_a = c(1, 2, 3, NA, 4, 5),
#'                       var_b = c(1, 2, "Hello", NA, 4, 5))
#'
#' convert_df <- test_df |> convert_numeric(c("var_a", "var_b"))
#'
#' @rdname convert_numeric
#'
#' @export
convert_numeric <- function(data_frame, variables){
    for (variable in variables) {
        # Only convert if all none NA values are numeric
        if (is_numeric(data_frame[[variable]])){
            data_frame[[variable]] <- as.numeric(data_frame[[variable]])
        }
    }

    data_frame
}
