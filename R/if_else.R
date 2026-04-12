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
#' @param ... The assignment of what should happen when condition becomes TRUE.
#'
#' @return Returns a data frame with conditionally computed variables.
#'
#' @seealso
#' The following functions can make use of the [do_if()] filter variables:
#'
#' Conditions: [if.()], [else_if.()], [else.()]
#'
#' Filter Data Frame: [where.()]
#'
#' Create new Variables: [compute()]
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
#' # Add multiple variables based on single conditions
#' multi_df <- my_data |>
#'          if.(age < 18 & education == "low",                  var1 = 1,
#'                                                              var2 = TRUE,
#'                                                              var3 = "Category 1") |>
#'     else_if.(age >= 18 & education %in% c("middle", "high"), var1 = 2,
#'                                                              var2 = FALSE,
#'                                                              var3 = "Category 2") |>
#'     else.   (                                                var1 = 3,
#'                                                              var2 = FALSE,
#'                                                              var3 = "Category 3")
#'
#' # Use if.() as a do-over-loop. In this kind of loop all vectors will be
#' # advanced one iteration at a time in parallel.
#' money    <- c("income", "expenses", "balance", "probability")
#' new_vars <- c("var1", "var2", "var3", "var4")
#' result   <- c(1, 2, 3, 4)
#'
#' do_over_df <- my_data |>
#'       if.(money > 0, new_vars = result) |>
#'     else.(           new_vars = 0)
#'
#' # It is also possible to select character expressions based on whether they
#' # start/end with or contain a certain text.
#' text_select_df <- my_data |>
#'     if.(income_class == "01.:",  start    = 1) |>
#'     if.(income_class == ":more", end      = 1) |>
#'     if.(education    == ":i:",   contains = 1)
#'
#' # Select observations by condition instead of generating new variable
#' subset_df <- my_data |> if.(sex == 1)
#'
#' # Select all non NA observations by variable
#' subset_df <- my_data |> if.(sex)
#'
#' # All these functions can be used in a do_if() situation and are aware of
#' # overarching conditions.
#' do_if_df <- my_data |>
#'     do_if(state < 11) |>
#'           if.(age < 18, new_var = 1) |>
#'         else.(          new_var = 2) |>
#'     else_do() |>
#'           if.(age < 18, new_var = 3) |>
#'         else.(          new_var = 4) |>
#'     end_do()
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
if. <- function(data_frame, condition, ...){
    # Measure the time
    print_start_message(suppress = TRUE)

    parent_env  <- parent.frame()

    # This step is important to make this function work in a nested situation.
    # Normally condition would be the name of what was last passed as a parameter.
    # If "if.()" is used nested inside a function this can basically be any placeholder.
    # So here we go up the ladder to get the original condition as a call.
    condition <- substitute(condition)
    while (is.name(condition) && exists(as.character(condition), parent_env)){
        condition <- eval(substitute(substitute(expression, parent_env), list(expression = condition)))
    }

    assignments <- as.list(substitute(list(...)))[-1]

    # The condition and the variable assignments are torn apart here, so that
    # only the unique variable and vector names are captured as characters.
    used_variables <- unique(c(all.vars(condition), # Get all variables from the condition
                               names(assignments), # Get all variables to which a value should be assigned
                               unlist(lapply(assignments, all.names)))) # Get all assigned variables

    # Get the original contents of vectors if there are any and put them in a list.
    # List names are the symbols names from above and the elements hold the actual contents.
    content_list <- mget(used_variables, envir = parent_env, ifnotfound = list(NULL))

    # Remove invalid empty entries. This is the case if a variable is an original
    # variable name and not a vector of variable names.
    content_list <- Filter(is_valid_vector, content_list)

    list_entry_lengths <- lengths(content_list)

    # If no vector was passed in the condition or the assignments, then evaluate
    # as normal if statement.
    flag_filter    <- FALSE
    condition_list <- list()

    if (length(assignments) > 0){
        for (entry in seq_along(assignments)){
            variable    <- names(assignments)[[entry]]
            calculation <- assignments[[variable]]
            calc_text   <- deparse(calculation)

            # If no vector was passed in the condition or the assignments, then evaluate
            # as normal.
            if (!variable %in% names(content_list)){
                condition <- translate_condition(condition)
                condition <- eval(condition, envir = data_frame, enclos = parent_env)

                # Check whether there are additional conditions active via do_if
                full_condition <- data_frame |> combined_condition(condition)

                # Add to call list
                condition_list <- c(condition_list, list(full_condition))
            }
            # If a vector was passed in the condition or the assignments, then evaluate
            # the if statement as a do over loop. Meaning for each vector the same elements
            # are used simultaneously one after another.
            else{
                list_entry_lengths <- lengths(content_list)

                if (length(collapse::funique(list_entry_lengths)) != 1){
                    print_message("ERROR", c("Passed vectors are of unequal lengths. All vectors must have an",
											 "equal number of elements. Evaluation will be aborted."))
                    return(invisible(data_frame))
                }

                # Do over loop per element
                for (element in seq_len(list_entry_lengths[1])){
                    # Get the respective first elements as symbols in a new list
                    replace_list <- lapply(content_list, function(vector){
                            if (is.numeric(vector)){
                                expression <- vector[element]
                            }
                            else{
                                expression <- as.name(vector[element])
                            }

                            # Check if there are colons as placeholders. If this is the
                            # case, convert these expressions to character so that they
                            # can be evaluated correctly later on.
                            number_of_colons <- nchar(gsub("[^:]", "", as.character(expression)))

                            if (number_of_colons > 0){
                                as.character(expression)
                            }
                            else{
                                expression
                            }
                        })

                    # Insert the current variables into the condition
                    current_condition <- do.call(substitute, list(condition, replace_list))
                    current_condition <- translate_condition(current_condition)
                    condition_list[[element]] <- eval(substitute(current_condition), envir = data_frame, enclos = parent_env)

                    # Check whether there are additional conditions active via do_if and
                    # add to call list
                    condition_list[[element]] <- data_frame |> combined_condition(condition_list[[element]])
                }
            }
        }
    }
    # Filter observations, if there are no assignments given
    else{
        flag_filter <- TRUE

        # If no vector was passed in the condition, then evaluate as normal
        if (length(content_list) == 0){
            condition <- translate_condition(condition)
            condition <- eval(condition, envir = data_frame, enclos = parent_env)

            # Remember rows to tell the user how many rows have been removed
            rows_before <- data_frame |> collapse::fnrow()

            # Evaluate normal condition
            if (is.logical(condition)){
                # Check whether there are additional conditions active via do_if
                full_condition <- data_frame |> combined_condition(condition)

                data_frame <- data_frame |> collapse::fsubset(full_condition)
            }
            # If a single variable name is given like 'age' this will be evaluated as:
            # !is.na(age). So it is basically a short form like 'If age;' in SAS.
            else{
                # If it is a single character variable it has to be checked first if it is part
                # of the data frame. If not, no subsetting will take place.
                if (is.character(condition) && length(condition) == 1){
                    condition <- data_frame |> part_of_df(condition)

                    if (length(condition) == 0){
                        print_message("ERROR", "No variable for subsetting provided. Data frame remains as is.")
                    }
                    else{
                        # Check whether there are additional conditions active via do_if
                        full_condition <- data_frame |> combined_condition(!is.na(data_frame[[condition]]))

                        data_frame <- data_frame |> collapse::fsubset(full_condition)
                    }
                }
                else if (length(condition) != collapse::fnrow(data_frame)){
                    print_message("ERROR", "Only single variables and conditions allowed. Data frame remains as is.")
                }
                # Evaluate single variable
                else{
                    # Check whether there are additional conditions active via do_if
                    full_condition <- data_frame |> combined_condition(!is.na(condition))

                    data_frame <- data_frame |> collapse::fsubset(full_condition)
                }
            }

            # Output info message
            rows_after <- data_frame |> collapse::fnrow()

            print_step("MAJOR", "Removed [removed] observations. Data frame now has [still_there] observations.",
                    removed = format(rows_before - rows_after,
                                     format = "d", decimal.mark = ",", big.mark = ".", scientific = FALSE),
                    still_there = format(rows_after,
                                         format = "d", decimal.mark = ",", big.mark = ".", scientific = FALSE))
        }
        # If a vector was passed in the condition, then evaluate the if statement
        # as a do over loop. Meaning for each vector the same elements are used
        # simultaneously one after another.
        else{
            original_condition <- condition

            for (element in seq_len(list_entry_lengths[1])){
                # Get the respective first elements as symbols in a new list
                replace_list <- lapply(content_list, function(vector){
                    if (is.numeric(vector)){
                        expression <- vector[element]
                    }
                    else{
                        expression <- as.name(vector[element])
                    }
                })

                # Evaluate complete assignment first without condition
                condition  <- do.call(substitute, list(original_condition, replace_list))

                condition <- translate_condition(condition)
                condition <- eval(condition, envir = data_frame, enclos = parent_env)

                # Remember rows to tell the user how many rows have been removed
                rows_before <- data_frame |> collapse::fnrow()

                # Evaluate normal condition
                if (is.logical(condition)){
                    # Check whether there are additional conditions active via do_if
                    full_condition <- data_frame |> combined_condition(condition)

                    data_frame <- data_frame |> collapse::fsubset(full_condition)
                }
                # If a single variable name is given like 'age' this will be evaluated as:
                # !is.na(age). So it is basically a short form like 'If age;' in SAS.
                else{
                    # If it is a single character variable it has to be checked first if it is part
                    # of the data frame. If not, no subsetting will take place.
                    if (is.character(condition) && length(condition) == 1){
                        condition <- data_frame |> part_of_df(condition)

                        if (length(condition) == 0){
                            print_message("ERROR", "No variable for subsetting provided. Data frame remains as is.")
                        }
                        else{
                            # Check whether there are additional conditions active via do_if
                            full_condition <- data_frame |> combined_condition(!is.na(data_frame[[condition]]))

                            data_frame <- data_frame |> collapse::fsubset(full_condition)
                        }
                    }
                    else if (length(condition) != collapse::fnrow(data_frame)){
                        print_message("ERROR", "Only single variables and conditions allowed. Data frame remains as is.")
                    }
                    # Evaluate single variable
                    else{
                        # Check whether there are additional conditions active via do_if
                        full_condition <- data_frame |> combined_condition(!is.na(condition))

                        data_frame <- data_frame |> collapse::fsubset(full_condition)
                    }
                }

                # Output info message
                rows_after <- data_frame |> collapse::fnrow()

                print_step("MAJOR", "Removed [removed] observations. Data frame now has [still_there] observations.",
                           removed = format(rows_before - rows_after,
                                            format = "d", decimal.mark = ",", big.mark = ".", scientific = FALSE),
                           still_there = format(rows_after,
                                                format = "d", decimal.mark = ",", big.mark = ".", scientific = FALSE))
            }
        }
    }

    # Evaluate calculations conditionally. Making use of hidden parameters.
    if (!flag_filter){
        data_frame <- data_frame |> compute(..., .if_condition   = condition_list,
                                                 .if_parent_frame = parent_env,
                                                 .if_suppressed   = TRUE)
    }

    print_closing()

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
    # Measure the time
    print_start_message(suppress = TRUE)

    parent_env  <- parent.frame()

    # This step is important to make this function work in a nested situation.
    # Normally condition would be the name of what was last passed as a parameter.
    # If "if.()" is used nested inside a function this can basically be any placeholder.
    # So here we go up the ladder to get the original condition as a call.
    condition <- substitute(condition)
    while (is.name(condition) && exists(as.character(condition), parent_env)){
        condition <- eval(substitute(substitute(expression, parent_env), list(expression = condition)))
    }

    assignments <- as.list(substitute(list(...)))[-1]

    # The condition and the variable assignments are torn apart here, so that
    # only the unique variable and vector names are captured as characters.
    used_variables <- unique(c(all.vars(condition), # Get all variables from the condition
                               names(assignments), # Get all variables to which a value should be assigned
                               unlist(lapply(assignments, all.names)))) # Get all assigned variables

    # Get the original contents of vectors if there are any and put them in a list.
    # List names are the symbols names from above and the elements hold the actual contents.
    content_list <- mget(used_variables, envir = parent_env, ifnotfound = list(NULL))

    # Remove invalid empty entries. This is the case if a variable is an original
    # variable name and not a vector of variable names.
    content_list <- Filter(is_valid_vector, content_list)

    # If no vector was passed in the condition or the assignments, then evaluate
    # as normal if statement.
    condition_list <- list()

    if (length(assignments) > 0){
        for (entry in seq_along(assignments)){
            variable    <- names(assignments)[[entry]]
            calculation <- assignments[[variable]]
            calc_text   <- deparse(calculation)

            # If no vector was passed in the condition or the assignments, then evaluate
            # as normal.
            if (!variable %in% names(content_list)){
                condition <- translate_condition(condition)
                condition <- eval(condition, envir = data_frame, enclos = parent_env)

                # This step is important to make this function work in a nested situation.
                # Normally variable would be the name of what was last passed as a parameter.
                # If "if.()" is used nested inside a function this can basically be any placeholder.
                # So here we go up the ladder to get the original name of the variable.
                original_var <- get_origin_symbol(variable)

                # Check whether there are additional conditions active via do_if
                full_condition <- data_frame |> combined_condition(is.na(data_frame[[original_var]]) & condition)

                # Add to call list
                condition_list <- c(condition_list, list(full_condition))
            }
            # If a vector was passed in the condition or the assignments, then evaluate
            # the if statement as a do over loop. Meaning for each vector the same elements
            # are used simultaneously one after another.
            else{
                list_entry_lengths <- lengths(content_list)

                if (length(collapse::funique(list_entry_lengths)) != 1){
                    print_message("ERROR", c("Passed vectors are of unequal lengths. All vectors must have an",
											 "equal number of elements. Evaluation will be aborted."))
                    return(invisible(data_frame))
                }

                if (length(assignments) == 0){
                    print_message("ERROR", c("When using vectors in conditions, there must be a variable assignment.",
											 "Evaluation will be aborted."))
                    return(invisible(data_frame))
                }

                # Do over loop per element
                for (element in seq_len(list_entry_lengths[1])){
                    # Get the respective first elements as symbols in a new list
                    replace_list <- lapply(content_list, function(vector){
                        if (is.numeric(vector)){
                            expression <- vector[element]
                        }
                        else{
                            expression <- as.name(vector[element])
                        }

                        # Check if there are colons as placeholders. If this is the
                        # case, convert these expressions to character so that they
                        # can be evaluated correctly later on.
                        number_of_colons <- nchar(gsub("[^:]", "", as.character(expression)))

                        if (number_of_colons > 0){
                            as.character(expression)
                        }
                        else{
                            expression
                        }
                    })

                    # Get the target variable from the assignment list and look whether
                    # it is a vector or not. If it is a vector, extract the current element
                    # and use its name.
                    target_variable <- names(assignments)[1]

                    if (target_variable %in% names(content_list)){
                        target_variable <- content_list[[target_variable]][element]
                    }

                    # Variable has to exist in data frame
                    if (!target_variable %in% names(data_frame)){
                        next
                    }

                    # Check whether there are additional conditions active via do_if
                    # Insert the current variables into the condition
                    current_condition <- do.call(substitute, list(condition, replace_list))
                    current_condition <- translate_condition(current_condition)
                    condition_list[[element]] <- eval(substitute(current_condition), envir = data_frame, enclos = parent_env)

                    # Check whether there are additional conditions active via do_if and
                    # add to call list
                    condition_list[[element]] <- data_frame |> combined_condition(is.na(data_frame[[target_variable]]) & condition_list[[element]])
                }
            }
        }

        # Evaluate calculations conditionally. Making use of hidden parameters.
        data_frame <- data_frame |> compute(..., .if_condition    = condition_list,
                                                 .if_parent_frame = parent_env,
                                                 .if_suppressed   = TRUE)
    }
    else{
        print_message("WARNING", "No assignments found. If you want to filter observations use if.() instead.")
    }

    print_closing()

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
    # Measure the time
    print_start_message(suppress = TRUE)

    parent_env  <- parent.frame()
    assignments <- as.list(substitute(list(...)))[-1]

    # The condition and the variable assignments are torn apart here, so that
    # only the unique variable and vector names are captured as characters.
    used_variables <- unique(c(names(assignments), # Get all variables to which a value should be assigned
                               unlist(lapply(assignments, all.names)))) # Get all assigned variables

    # Get the original contents of vectors if there are any and put them in a list.
    # List names are the symbols names from above and the elements hold the actual contents.
    content_list <- mget(used_variables, envir = parent_env, ifnotfound = list(NULL))

    # Remove invalid empty entries. This is the case if a variable is an original
    # variable name and not a vector of variable names.
    content_list <- Filter(is_valid_vector, content_list)

    # If no vector was passed in the condition or the assignments, then evaluate
    # as normal if statement.
    condition_list <- list()

    if (length(assignments) > 0){
        for (entry in seq_along(assignments)){
            variable    <- names(assignments)[[entry]]
            calculation <- assignments[[variable]]
            calc_text   <- deparse(calculation)

            # If no vector was passed in the condition or the assignments, then evaluate
            # as normal.
            if (!variable %in% names(content_list)){
                # This step is important to make this function work in a nested situation.
                # Normally variable would be the name of what was last passed as a parameter.
                # If "if.()" is used nested inside a function this can basically be any placeholder.
                # So here we go up the ladder to get the original name of the variable.
                original_var <- get_origin_symbol(variable)

                # Check whether there are additional conditions active via do_if
                full_condition <- data_frame |> combined_condition(is.na(data_frame[[original_var]]))

                # Add to call list
                condition_list <- c(condition_list, list(full_condition))
            }
            # If a vector was passed in the condition or the assignments, then evaluate
            # the if statement as a do over loop. Meaning for each vector the same elements
            # are used simultaneously one after another.
            else{
                list_entry_lengths <- lengths(content_list)

                if (length(collapse::funique(list_entry_lengths)) != 1){
                    print_message("ERROR", c("Passed vectors are of unequal lengths. All vectors must have an",
											 "equal number of elements. Evaluation will be aborted."))
                    return(invisible(data_frame))
                }

                if (length(assignments) == 0){
                    print_message("ERROR", c("When using vectors in conditions, there must be a variable assignment.",
											 "Evaluation will be aborted."))
                    return(invisible(data_frame))
                }

                # Do over loop per element
                for (element in seq_len(list_entry_lengths[1])){
                    # Get the respective first elements as symbols in a new list
                    replace_list <- lapply(content_list, function(vector){
                        if (is.numeric(vector)){
                            expression <- vector[element]
                        }
                        else{
                            expression <- as.name(vector[element])
                        }

                        # Check if there are colons as placeholders. If this is the
                        # case, convert these expressions to character so that they
                        # can be evaluated correctly later on.
                        number_of_colons <- nchar(gsub("[^:]", "", as.character(expression)))

                        if (number_of_colons > 0){
                            as.character(expression)
                        }
                        else{
                            expression
                        }
                    })

                    # Get the target variable from the assignment list and look whether
                    # it is a vector or not. If it is a vector, extract the current element
                    # and use its name.
                    target_variable <- names(assignments)[1]

                    if (target_variable %in% names(content_list)){
                        target_variable <- content_list[[target_variable]][element]
                    }

                    # Variable has to exist in data frame
                    if (!target_variable %in% names(data_frame)){
                        next
                    }

                    # Check whether there are additional conditions active via do_if and
                    # add to call list
                    condition_list[[element]] <- data_frame |> combined_condition(is.na(data_frame[[target_variable]]))
                }
            }
        }
    }

    # Evaluate calculations conditionally. Making use of hidden parameters.
    data_frame <- data_frame |> compute(..., .if_condition    = condition_list,
                                             .if_parent_frame = parent_env,
                                             .if_suppressed   = TRUE)

    print_closing()

    data_frame
}


#' Translate Colon In Condition
#'
#' @description
#' Translates colons used as placeholders in a condition. A colon can stand for
#' an expression starting/ending with or containing a letter combination.
#'
#' @param condition The condition on which the data frame should be subsetted.
#'
#' @return
#' Returns a translated condition.
#'
#' @noRd
translate_condition <- function(condition){
    if (is.call(condition)){
        operator <- condition[[1]]

        # Only check the expression, if its an equality check
        if (identical(operator, as.name("==")) || identical(operator, as.name("!="))){
            variable   <- condition[[2]]
            expression <- condition[[3]]

            if (is.character(expression) && length(expression) == 1){
                # Count the colons, there may not be more than two, otherwise the function
                # should skip the expression.
                colon_count <- nchar(gsub("[^:]", "", expression))

                if (colon_count == 1){
                    # When at the end ("text:"), select all variables which start with the characters
                    # coming before the colon. ":" acts as a placeholder for those who come after.
                    if (endsWith(expression, ":")){
                        search_term <- substring(expression, 1, nchar(expression) - 1)

                        translated_call <- call("startsWith", variable, search_term)
                    }
                    # When at the start (":text"), select all variables which end with the characters
                    # following the colon. ":" acts as a placeholder for everything that comes before.
                    else if (startsWith(expression, ":")){
                        search_term <- substring(expression, 2)

                        translated_call <- call("endsWith", variable, search_term)
                    }
                }
                # In case there are two colons, one at the start and one at the end (":text:"),
                # look for the text inbetween inside the variable expressions
                else if (colon_count == 2){
                    if (startsWith(expression, ":") && endsWith(expression, ":")){
                        search_term <- gsub(":", "", expression)

                        translated_call <- call("grepl", search_term, variable, fixed = TRUE)
                    }
                }
                else{
                    translated_call <- condition
                }

                # Return call as is or as NOT depending on the operator
                if (identical(operator, as.name("!="))){
                    return(call("!", translated_call))
                }
                else{
                    return(translated_call)
                }
            }
        }

        # Tear up the condition into its single expressions and check each expression
        # for a colon. Then put the single translated expressions back into a fully
        # functional condition.
        as.call(lapply(condition, translate_condition))
    }
    # Return condition as is, if it is a logical instead of a call
    else{
        condition
    }
}


#' Translate Colon In Condition
#'
#' @description
#' Translates colons used as placeholders in a condition. A colon can stand for
#' an expression starting/ending with or containing a letter combination.
#'
#' @param condition The condition on which the data frame should be subsetted.
#'
#' @return
#' Returns a translated condition.
#'
#' @noRd
combined_condition <- function(data_frame, condition){
    # Check whether there are any filter variables
    df_names <- names(data_frame)

    filter_variables <- df_names[startsWith(df_names, ".do_if_select")]

    # If there are no filter variables return a pseudo always TRUE
    if (length(filter_variables) == 0){
        filter_variables <- TRUE
    }
    # Otherwise get the logical filter variables
    else{
        filter_variables <- data_frame |> collapse::fselect(filter_variables)
    }

    # Merge the given condition as well as the active filter variables together
    # into one logical vector
    Reduce(`&`, c(list(condition), filter_variables))
}


###############################################################################
# Quick filtering
###############################################################################
#' Filter Data Frame With Direct View
#'
#' @description
#' Filter observations and variables and directly view the result on screen.
#'
#' @param data_frame A data frame on which to apply filters.
#' @param condition The condition on which to filter observations.
#' @param keep The Variables to keep in the result data frame.
#'
#' @return Returns a filtered data frame.
#'
#' @seealso
#' The following functions can make use of the [do_if()] filter variables:
#'
#' Conditions: [if.()], [else_if.()], [else.()]
#'
#' Filter Data Frame: [where.()]
#'
#' Create new Variables: [compute()]
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Get a quick filtered view
#' my_data |> where.(sex == 1 & age < 25,
#'                   c(sex, age, household_id, education))
#'
#' @export
where. <- function(data_frame,
                   condition = NULL,
                   keep      = NULL){
    # Manage conditional subsetting
    condition <- substitute(condition)
    condition <- translate_condition(condition)
    condition <- eval(condition, envir = data_frame, enclos = parent.frame())

    full_condition <- data_frame |> combined_condition(condition)

    data_frame <- data_frame |> collapse::fsubset(full_condition)

    # Manage keeping variables
    keep <- get_origin_as_char(keep, substitute(keep))

    if (!is.null(keep)){
        data_frame <- data_frame |> keep(keep, order_vars = TRUE)
    }

    # View data frame in new window
    if (interactive()){
        data_frame |> utils::View()
    }

    invisible(data_frame)
}


###############################################################################
# Do if
###############################################################################
#' Lock In A Condition
#'
#' @name do_if
#'
#' @description
#' Creates a filter variable based on the given condition. This variable can be
#' accessed by [if.()], [else_if.()], [else.()], [where.()] and [compute()], enabling these
#' functions to work with an overarching condition. This function can also be
#' used to nest multiple overarching conditions.
#'
#' @param data_frame A data frame on which to apply filters.
#' @param condition The condition lock in.
#'
#' @return Returns a data frame with a new filter variable.
#'
#' @seealso
#' The following functions can make use of the [do_if()] filter variables:
#'
#' Conditions: [if.()], [else_if.()], [else.()]
#'
#' Filter Data Frame: [where.()]
#'
#' Create new Variables: [compute()]
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(1000)
#'
#' # Create a simple do-if-block
#' do_if_df <- my_data |>
#'     do_if(state < 11) |>
#'           if.(age < 18, new_var = 1) |>
#'         else.(          new_var = 2) |>
#'     else_do() |>
#'           if.(age < 18, new_var = 3) |>
#'         else.(          new_var = 4) |>
#'     end_do()
#'
#' # do_if() can also be nested
#' do_if_df <- my_data |>
#'     do_if(state < 11) |>
#'         do_if(sex == 1) |>
#'               if.(age < 18, new_var = 1) |>
#'             else.(          new_var = 2) |>
#'         else_do() |>
#'               if.(age < 18, new_var = 3) |>
#'             else.(          new_var = 4) |>
#'         end_do() |>
#'     else_do() |>
#'         do_if(sex == 1) |>
#'               if.(age < 18, new_var = 5) |>
#'             else.(          new_var = 6) |>
#'         else_do() |>
#'               if.(age < 18, new_var = 7) |>
#'             else.(          new_var = 8) |>
#'         end_do() |>
#'     end_do()
#'
#' # NOTE: Close the do-if-blocks with end_do() to remove the temporary logical
#' #       filter variables.
#'
#' # Probably a logical filter variable is exactly what you want. In this case
#' # just run do_if() without closing the block.
#' logic_filter_df <- my_data |> do_if(state < 11)
#'
#' @rdname do_if
#'
#' @export
do_if <- function(data_frame, condition){
    parent_env <- parent.frame()

    # Evaluate condition to get a logical vector
    condition <- translate_condition(substitute(condition))
    condition <- eval(condition, envir = data_frame, enclos = parent_env)

    # Check whether there are already filter variables and add a new one. Every
    # filter variable receives a running number.
    filter_variables <- grep("^\\.do_if_select", names(data_frame), value = TRUE)
    new_filter       <- paste0(".do_if_select", length(filter_variables) + 1)

    # Add logical condition vector as new filter variable and return
    data_frame[[new_filter]] <- condition

    data_frame
}


#' @description
#' [else_do()]: Checks for existing filter variables and reverses the condition
#' of the last filter variable.
#'
#' @rdname do_if
#'
#' @export
else_do <- function(data_frame){
    # Get existing filter variables
    filter_variables <- grep("^\\.do_if_select", names(data_frame), value = TRUE)

    if (length(filter_variables) == 0){
        print_message("WARNING", "No active filter variable found. else_do() will be ignored.")
        return(invisible(data_frame))
    }

    # Get last filter variable and get the inverse condition
    last_filter               <- filter_variables[[length(filter_variables)]]
    data_frame[[last_filter]] <- !data_frame[[last_filter]]

    data_frame
}


#' @description
#' [end_do()]: Drops the last filter variable.
#'
#' @rdname do_if
#'
#' @export
end_do <- function(data_frame){
    # Get existing filter variables
    filter_variables <- grep("^\\.do_if_select", names(data_frame), value = TRUE)

    if (length(filter_variables) == 0){
        print_message("WARNING", "No active filter variable found. end_do() will be ignored.")
        return(invisible(data_frame))
    }

    # Get last filter variable and drop it
    last_filter <- filter_variables[[length(filter_variables)]]

    data_frame |> dropp(last_filter)
}


#' @description
#' [end_all_do()]: Drops all filter variables.
#'
#' @rdname do_if
#'
#' @export
end_all_do <- function(data_frame){
    # Get existing filter variables
    filter_variables <- grep("^\\.do_if_select", names(data_frame), value = TRUE)

    if (length(filter_variables) == 0){
        print_message("WARNING", "No active filter variable found. end_all_do() will be ignored.")
        return(invisible(data_frame))
    }

    # Drop all filter variables
    data_frame |> dropp(filter_variables)
}
