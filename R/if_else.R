#' If - Else if - Else Statements
#'
#' @description
#' These functions make if statements more readable. Especially if an if block becomes bigger
#' it can be hard to read with multiple nested if_else statements. With these new functions
#' if blocks can be written like in other languages with a clear and simpler structure. In
#' addition not only for one variable can a new value be assigned, but for multiple.
#'
#' @name if_else
#'
#' @param data_frame A data frame on which to apply an if statement.
#' @param condition The condition on which a value should be passed to a variable.
#' @param ... The Assignment of what should happen when condition becomes TRUE.
#'
#' @return Returns a data frame with conditionally computed variables. If assigned values
#' are of different types a character variable will be returned.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Call function
#' new_df <- my_data |>
#'          if.(age < 18,             age_group = "under 18") |>
#'     else_if.(age >= 18 & age < 65, age_group = "18 to under 65") |>
#'     else.   (                      age_group = "65 and older")
#'
#' # Or with multiple variables
#' new_df <- my_data |>
#'          if.(age < 18,             age_group = "under 18"      , age_num = 1L) |>
#'     else_if.(age >= 18 & age < 65, age_group = "18 to under 65", age_num = 2L) |>
#'     else.   (                      age_group = "65 and older",   age_num = 3L)
#'
#' # NOTE: As in other languages the following if blocks won't produce the same result.
#' #       if.() will overwrite existing values, while else_if.() will not.
#' state_df <- my_data |>
#'          if.(state == 1, state_a = "State 1") |>
#'     else_if.(state < 11, state_a = "West") |>
#'     else.   (            state_a = "East")
#'
#' state_df <- state_df |>
#'       if.(state == 1, state_b = "State 1") |>
#'       if.(state < 11, state_b = "West") |>
#'     else.(            state_b = "East")
#'
#' # Select observations by condition instead of generating new variable
#' subset_df <- my_data |> if.(sex == 1)
#'
#' @rdname if_else
#'
#' @keywords internal
NULL


#' @description
#' [if.()] always creates a new variable if the given variable name is not part of the given
#' data frame. If there already is a variable with the given name, the existing values will
#' be overwritten if the condition is TRUE.
#'
#' If no new variable is provided, [if.()] will select observations by the given condition
#' instead.
#'
#' @rdname if_else
#'
#' @export
if. <- function(data_frame, condition, ...) {
    condition   <- eval(substitute(condition), envir = data_frame, enclos = parent.frame())
    assignments <- as.list(substitute(list(...)))[-1]

    if (length(assignments) > 0){
        # Go trough each assignment and calculate the values individually
        for (variable in names(assignments)){
            # Evaluate complete assignment first without condition
            value <- eval(assignments[[variable]], envir = data_frame, enclos = parent.frame())

            # If there already is a variable with the given name pick the existing value as fallback
            if (variable %in% names(data_frame)){
                # Check if existing variable type is of same type as assigned value.
                # Put out a warning on type mismatch.
                if (check_types(data_frame, variable, value)){
                    data_frame[[variable]] <- as.character(data_frame[[variable]])
                    value <- as.character(value)
                }

                data_frame <- generate_new_var(data_frame, condition, variable, value)
            }
            # If there is not an existing variable pass NA as fallback
            else{
                data_frame <- generate_new_var(data_frame, condition, variable, value)
            }
        }
    }
    else{
        # Evaluate normal condition
        if (is.logical(condition)){
            data_frame <- data_frame |> collapse::fsubset(condition)
        }
        # If a single variable name is given like 'age' this will be evaluated as:
        # !is.na(age). So it is basically a short form like 'If age;' in SAS.
        else{
            # If it is a single character variable it has to be checked first if it is part
            # of the data frame. If not, no subsetting will take place.
            if (is.character(condition) && length(condition) == 1){
                condition <- data_frame |> part_of_df(condition)

                if (length(condition) == 0){
                    message(" X ERROR: No variable for subsetting provided. Data frame remains as is.")
                }
                else{
                    data_frame <- data_frame |> collapse::fsubset(!is.na(data_frame[[condition]]))
                }
            }
            else if (length(condition) > collapse::fnrow(data_frame) || (is.character(condition) && length(condition) > 1)){
                message(" X ERROR: Only single variables and conditions allowed. Data frame remains as is.")
            }
            # Evaluate single variable
            else{
                data_frame <- data_frame |> collapse::fsubset(!is.na(condition))
            }
        }
    }

    data_frame
}


#' @description
#' [else_if.()] only acts if there already is a variable with the given name. Only NA values
#' will get new values if condition is TRUE. The existing values will not be overwritten.
#'
#' @rdname if_else
#'
#' @export
else_if. <- function(data_frame, condition, ...){
    condition   <- eval(substitute(condition), envir = data_frame, enclos = parent.frame())
    assignments <- as.list(substitute(list(...)))[-1]

    # Go trough each assignment and calculate the values individually
    for (variable in names(assignments)){
        # Variable has to exist in data frame
        if (!variable %in% names(data_frame)){
            next
        }

        # Evaluate complete assignment first without condition
        value <- eval(assignments[[variable]], envir = data_frame, enclos = parent.frame())

        # Check if existing variable type is of same type as assigned value.
        # Put out a warning on type mismatch.
        if (check_types(data_frame, variable, value)){
            data_frame[[variable]] <- as.character(data_frame[[variable]])
            value <- as.character(value)
        }

        data_frame <- generate_new_var(data_frame, is.na(data_frame[[variable]]) & condition, variable, value)
    }

    data_frame
}


#' @description
#' [else.()] only acts if there already is a variable with the given name. Sets every
#' remaining NA in given variable to the given value.
#'
#' @rdname if_else
#'
#' @export
else. <- function(data_frame, ...){
    assignments <- as.list(substitute(list(...)))[-1]

    # Go trough each assignment and calculate the values individually
    for (variable in names(assignments)){
        # Variable has to exist in data frame
        if (!variable %in% names(data_frame)){
            next
        }

        # Evaluate complete assignment first without condition
        value <- eval(assignments[[variable]], envir = data_frame, enclos = parent.frame())

        # Check if existing variable type is of same type as assigned value.
        # Put out a warning on type mismatch.
        if (check_types(data_frame, variable, value)){
            data_frame[[variable]] <- as.character(data_frame[[variable]])
            value <- as.character(value)
        }

        data_frame <- generate_new_var(data_frame, is.na(data_frame[[variable]]), variable, value)
    }

    data_frame
}


#' Check for Identical Types
#'
#' @description
#' Check if a certain value is of the same type as a variable.
#'
#' @param data_frame The data frame which contains the variables to check.
#' @param variable The main variable whose data type should be compared to value.
#' @param current The current value whose data type should be compared to variable.
#'
#' @return
#' Returns a further formatted workbook.
#'
#' @noRd
check_types <- function(data_frame, variable, current){
    # Abort if all values are NA
    if (all(is.na(data_frame[[variable]]))){
        return(FALSE)
    }

    type_c <- typeof(current)
    type_d <- typeof(data_frame[[variable]])

    # Abort if types are identical
    if (identical(type_c, type_d)){
        return(FALSE)
    }

    message(" ! WARNING: Type mismatch: Current value ", current[1], " is of type ", type_c, " but should be of\n",
            "            type ", type_d, ". ", variable, " will be converted to character.")

    TRUE
}


#' Handle Conditional Variable Generation
#'
#' @description
#' Conditionally generate new variables depending on single values or whole vectors.
#'
#' @param data_frame The data frame to which a new variable should be added.
#' @param condition The condition on which a value should be passed to a variable.
#' @param variable The new variable name.
#' @param value The value which should be assigned to the new variable.
#'
#' @return
#' Returns a data frame with a conditionally added variable.
#'
#' @noRd
generate_new_var <- function(data_frame, condition, variable, value){
    # In case of a single value, assign this value to the filtered rows
    if (length(value) == 1){
        data_frame[[variable]][condition] <- value
    }
    # In case of a vector, set the same condition to the vector as to the data frame
    else{
        data_frame[[variable]][condition] <- value[condition]
    }

    data_frame
}
