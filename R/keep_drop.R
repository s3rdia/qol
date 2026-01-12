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
#' # You can also keep or drop a range of variables
#' new_dt8 <- my_data |> keep(year, state:income)
#' new_dt9 <- my_data |> dropp(year, state:income)
#'
#' # You can also use the colon as a placeholder for any text
#' start1 <- my_data |> keep("s:") # Variable names start with "s"
#' start2 <- my_data |> dropp("s:")
#'
#' end1 <- my_data |> keep(":id") # Variable names end with "id"
#' end2 <- my_data |> dropp(":id")
#'
#' contain1 <- my_data |> keep(":on:") # Variable names which contain "on"
#' contain2 <- my_data |> dropp(":on:")
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
    variables_temp <- dots_to_char(...)

    if (length(variables_temp) == 0){
        return(data_frame)
    }

    original_order <- names(data_frame)

    # Check if there are any colons in the selection and deparse variables accordingly
    variables <- data_frame |> deparse_colon(variables_temp)

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
    if (!order_vars){
        data.table::setcolorder(data_frame, original_order, skip_absent = TRUE)
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
    variables_temp <- dots_to_char(...)

    if (length(variables_temp) == 0){
        return(data_frame)
    }

    # Check if there are any colons in the selection and deparse variables accordingly
    variables <- data_frame |> deparse_colon(variables_temp)

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

#' Helper For Selecting Variable Ranges
#'
#' @description
#' When using a colon in certain places, variable ranges should be selected, depending
#' on where the colon is placed.
#'
#' @param data_frame The data frame which contains the variable names to be selected.
#' @param variable_vector A vector containing the variable names to be checked.
#'
#' @return
#' A deparsed vector of variable names.
#'
#' @noRd
deparse_colon <- function(data_frame, variable_vector){
    variables <- character(0)

    # Loop through variable vector to decide whether it is a single variable or
    # a variable range with colon should be selected.
    for (variable in variable_vector){
        only_colons <- gsub(":", "", variable)

        # If it's just colons with no text, skip "variable" and put out a warning
        if (nchar(only_colons) == 0) {
            message(" ! WARNING: A selection only contained colons. Selection will be ignored.")
            next
        }

        # Count the colons, there may not be more than two, otherwise the function
        # should skip the variable.
        colon_count <- nchar(gsub("[^:]", "", variable))

        # In case of a single variable just add it to the vector
        if (colon_count == 0){
            variables <- c(variables, variable)
        }
        # In case of a variable range
        else if (colon_count == 1){
            # When at the end ("text:"), select all variables which start with the characters
            # coming before the colon. ":" acts as a placeholder for those who come after.
            if (endsWith(variable, ":")){
                search_term <- substring(variable, 1, nchar(variable) - 1)
                matches     <- names(data_frame)[startsWith(names(data_frame), search_term)]

                variables   <- c(variables, matches)
            }
            # When at the start (":text"), select all variables which end with the characters
            # following the colon. ":" acts as a placeholder for everything that comes before
            else if (startsWith(variable, ":")){
                search_term <- substring(variable, 2)
                matches     <- names(data_frame)[endsWith(names(data_frame), search_term)]

                variables   <- c(variables, matches)
            }
            # If colon is in the middle ("var1:var10"), select a range of varaibles between
            # these two.
            else{
                parts     <- strsplit(variable, ":", fixed = TRUE)[[1]]

                variables <- c(variables, data_frame |> vars_between(parts[1], parts[2]))
            }
        }
        # In case there are two colons, one at the start and one at the end (":text:"),
        # look for the text inbetween inside the variable names.
        else if (colon_count == 2 && startsWith(variable, ":") && endsWith(variable, ":")){
            search_term <- gsub(":", "", variable)
            matches     <- names(data_frame)[grepl(search_term, names(data_frame), fixed = TRUE)]

            variables   <- c(variables, matches)

        }
        # If there are more than two colons skip variable
        else{
            message(" ! WARNING: Selection '", variable, "' contains more than two colons which is\n",
                    "            not allowed. Selection will be ignored.")
        }
    }

    variables
}
