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
        print_message("NOTE", c("Only single variable names allowed for 'from' and 'to'. The respective",
								"first element will be considered."))

        from <- from[1]
        to   <- to[1]
    }

    # Check if "from" and "to" variable names are part of the data frame.
    # If not, adjust accordingly.
    var_names <- names(data_frame)

    if (from != "" && !from %in% var_names){
        print_message("WARNING", c("'from' variable '[from]' is not part of the data frame.",
								   "Selection will start from the first variable."), from = from)

        from = ""
    }
    if (to != "" && !to %in% var_names){
        print_message("WARNING", c("'to' variable '[to]' is not part of the data frame.",
								   "Selection will go until the last variable."), to = to)

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


#' Split A Pattern Based Variable Range Into Its Parts
#'
#' @description
#' Splits a pattern based variable range like 'age1-age10' into the shared name
#' prefix and the numeric start and end value.
#'
#' @param variable An expression to check for a variable range.
#'
#' @return
#' Returns a list with the elements 'prefix', 'from' and 'to', or NULL if the
#' given string is not a pattern based variable range.
#'
#' @noRd
parse_pattern_range <- function(variable){
    # The name parts must not contain digits, so a variable name is split
    # unambiguously into its name prefix and its number.
    pattern <- "^([A-Za-z_.][A-Za-z_.]*)([0-9]+)-([A-Za-z_.][A-Za-z_.]*)([0-9]+)$"

    if (!grepl(pattern, variable)){
        return(NULL)
    }

    parts <- regmatches(variable, regexec(pattern, variable))[[1]]

    # Both sides of the range must share the same prefix, otherwise the selection
    # is not a pattern based range but rather two unrelated variable names
    if (!identical(parts[2], parts[4])){
        return(NULL)
    }

    list(prefix = parts[2],
         from   = as.numeric(parts[3]),
         to     = as.numeric(parts[5]))
}


#' Select Variables By Name Pattern Instead Of By Position
#'
#' @description
#' Selects all variables which share the same name prefix and whose number lies
#' inside the given numeric range. In contrast to a colon range, which selects
#' everything between two variables inside the data frame, this selects by name
#' pattern only.
#'
#' @param data_frame The data frame which contains the variable names to be selected.
#' @param variable An expression to check for a variable range.
#'
#' @return
#' Returns the matching variable names in the order they appear inside the data
#' frame, or NULL if the given selection is not a pattern based variable range.
#'
#' @noRd
deparse_pattern_range <- function(data_frame, variable){
    # Selections with a colon are handled in another function
    if (grepl(":", variable, fixed = TRUE)){
        return(NULL)
    }

    # A variable which is part of the data frame is never treated as a range, so
    # variable names which really contain a hyphen keep working.
    var_names <- names(data_frame)

    if (variable %in% var_names){
        return(NULL)
    }

    # Actually parse the expression
    range <- parse_pattern_range(variable)

    if (is.null(range)){
        return(NULL)
    }

    pattern  <- paste0("^", range[["prefix"]], "([0-9]+)$")
    is_match <- grepl(pattern, var_names)

    # Compare the numbers behind the prefix instead of the variable names themselves,
    # so age1 and age10 are both inside the range age1-age10
    numbers  <- suppressWarnings(as.numeric(gsub(pattern, "\\1", var_names[is_match])))
    in_range <- numbers >= min(range[["from"]], range[["to"]]) &
                numbers <= max(range[["from"]], range[["to"]])

    var_names[is_match][in_range]
}
