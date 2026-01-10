#' Convert Function Arguments to Character Vector
#'
#' @name convert_arguments
#'
#' @description
#' [args_to_char()]: Converts any argument passed as a single character or symbol as well
#' as character vectors or vector of symbols back as character vector.
#'
#' @param argument Function argument to convert.
#'
#' @return
#' Returns a character vector.
#'
#' @examples
#' # Example function with function parameter
#' print_vnames <- function(parameter){
#'     var_names <- args_to_char(substitute(parameter))
#'     print(var_names)
#' }
#'
#' print_vnames(age)
#' print_vnames("age")
#' print_vnames(c(age, sex, income, weight))
#' print_vnames(c("age", "sex", "income", "weight"))
#'
#' # You can also pass in a character vector, if you have stored variable names elsewhere
#' var_names <- c("age", "sex", "income", "weight")
#' print_vnames(var_names)
#'
#' # If you plan to use the function within other functions, better use get_origin_as_char()
#' print_vnames <- function(parameter){
#'     var_names <- get_origin_as_char(parameter, substitute(parameter))
#'     print(var_names)
#' }
#'
#' another_function <- function(parameter){
#'     print_vnames(parameter)
#' }
#'
#' another_function("age")
#' another_function(c("age", "sex", "income", "weight"))
#'
#' # Example function with ellipsis
#' print_vnames <- function(...){
#'     var_names <- dots_to_char(...)
#'     print(var_names)
#' }
#'
#' print_vnames(age)
#' print_vnames("age")
#' print_vnames(age, sex, income, weight)
#' print_vnames("age", "sex", "income", "weight")
#'
#' # You can also pass in a character vector, if you have stored variable names elsewhere
#' var_names <- c("age", "sex", "income", "weight")
#' print_vnames(var_names)
#'
#' @rdname convert_arguments
#'
#' @export
args_to_char <- function(argument){
    if (is.null(argument)){
        return(NULL)
    }

    # Single Character: Just return as is
    if (is.character(argument)){
        return(argument)
    }

    # Single symbol: Convert to character and return
    if (is.symbol(argument)){
        return(as.character(argument))
    }

    # Vector/List: If argument is a character vector or a vector of symbols,
    # c("year", "age") or c(year, age), then convert all elements to character.
    if (is.call(argument) &&  (identical(argument[[1]], quote(c)) ||
                               identical(argument[[1]], quote(list)))){
        return(vapply(argument[-1], as.character, character(1)))
    }

    # If there is a ":" in the call, this returns the call as is so that it can be used
    # as in e.g. keep/dropp to make use of ranges.
    if (is.call(argument) && identical(argument[[1]], quote(`:`))) {
        return(paste(as.character(argument[[2]]),
                     as.character(argument[[3]]),
                     sep = ":"))
    }

    stop(" X ERROR: Something went wrong with the argument conversion.\n",
         "          Only single character and symbols, as well as vectors and flat lists are allowed.\n",
         "          Function will be aborted.")
}


#' Convert Ellipsis to Character Vector
#'
#' @description
#' [dots_to_char()]: When you define a function and want the user to be able to pass variable
#' names without the need to have them stored in a vector c() or list() beforehand and without
#' putting the names into quotation marks, you can convert this variable list passed as ...
#' into a character vector.
#'
#' Note: If the user passes a list of characters it is returned as given.
#'
#' @param ... Used for variable names listed in ... without the need to put them in c() or list().
#'
#' @rdname convert_arguments
#'
#' @export
dots_to_char <- function(...){
    expressions <- as.list(substitute(list(...)))[-1]

    # Resolve the different expressions into a character vector
    unlist(lapply(expressions, function(expression){
        # Character: Just return as is
        if (is.character(expression)){
            return(expression)
        }

        # Symbol: Try evaluation
        if (is.symbol(expression)){
            # Find the origin of the symbol by searching through the parent environments
            value <- get_origin_symbol(expression)

            if (is.character(value)){
                return(value)
            }

            return(as.character(expression))
        }

        # Calls: c(), list(), :
        args_to_char(expression)}), use.names = FALSE)
}


#' Get The Original Contents Of An Argument As Character
#'
#' @description
#' [get_origin_as_char()] is a wrapper that allows to retrieve the original contents
#' of the provided variable, whether called directly or nested in multiple function calls,
#' as a character vector.
#'
#' @param original The data frame which contains the columns to be checked.
#' @param substituted The grouping variables which potentially form unique combinations.
#'
#' @rdname convert_arguments
#'
#' @export
get_origin_as_char <- function(original, substituted){
    tryCatch({
        # Force evaluation to see if it exists
        if (is.character(original)){
            original
        }
        else{
            args_to_char(substituted)
        }
    }, error = function(e) {
        # If evaluation failed (object not found), convert the symbol name
        args_to_char(substituted)
    })
}


#' Get The Original Symbol From Parent Environments
#'
#' @description
#' When nesting functions and trying to catch symbols, it often happens that 'R'
#' looses contact to the original symbol. This function looks up the parent environments
#' until it finds the original symbol and returns it.
#'
#' @param symbol The symbol to look up in the parent environments.
#'
#' @return
#' The original symbol.
#'
#' @noRd
get_origin_symbol <- function(symbol){
    symbol <- as.character(symbol)

    # Loop through the thee of parent environments
    for (i in rev(seq_len(sys.nframe()))){
        env <- sys.frame(i)

        # If the symbol is found in the current parent environment return it
        value <- get0(substitute(symbol), envir = env, inherits = FALSE)

        if (!is.null(value) && is.character(value)){
            return(value)
        }
    }

    symbol
}


#' Convert Variables
#'
#' @name convert_variables
#'
#' @description
#' [convert_numeric()] converts all given variables to numeric if possible. If
#' a variable contains none numerical values (not including NAs), the variable
#' will not be converted.
#'
#' @return
#' [convert_numeric()] returns the same data frame with converted variables where possible.
#'
#' @param data_frame A data frame containing variables to convert.
#' @param variables Variables from the data frame which should be converted.
#'
#' @examples
#' # Convert variables in a data frame to numeric where possible
#' test_df <- data.frame(var_a = c(1, 2, 3, NA, 4, 5),
#'                       var_b = c(1, 2, "Hello", NA, 4, 5))
#'
#' convert_df <- test_df |> convert_numeric(c("var_a", "var_b"))
#'
#' @rdname convert_variables
#'
#' @export
convert_numeric <- function(data_frame, variables){
    data_frame[variables] <-lapply(data_frame[variables], function(variable){
        # Try to convert to numeric
        var_converted <- suppressWarnings(as.numeric(variable))

        # If NA values are equal, conversion was successful
        if (all(is.na(variable) == is.na(var_converted))){
            var_converted
        }
        else{
            variable
        }
    })

    data_frame
}


#' @description
#' [convert_factor()] converts all given variables to factor.
#'
#' @return
#' [convert_factor()] returns the same data frame with converted variables.
#'
#' @examples
#' # Convert variables in a data frame to factor
#' test_df <- data.frame(var_a = c(1, 2, 3, 4, 5),
#'                       var_b = c("e", "c", "a", "d", "b"))
#'
#' convert_df <- test_df |> convert_factor("var_b")
#'
#' @rdname convert_variables
#'
#' @export
convert_factor <- function(data_frame, variables){
    for (variable in variables){
        if (is.character(data_frame[[variable]])){
            # Extract the number of labels from variable
            label_levels <- data_frame[[variable]] |>
                unlist(use.names = FALSE) |>
                collapse::funique() |>
                collapse::na_omit()

            # Convert variable to factor
            data_frame[[variable]] <- factor(
                data_frame[[variable]],
                levels  = label_levels,
                ordered = TRUE)
        }
    }

    data_frame
}
