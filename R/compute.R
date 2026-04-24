#' Compute New Variables
#'
#' @description
#' Compute new variables without having to write the name of the data frame multiple times.
#' It can handle all kinds of operations like simple value assignment or calculations, but also
#' can make use of other functions.
#'
#' In addition it can be used in a conditional [do_if()] block.
#'
#' @param data_frame A data frame in which to compute new variables.
#' @param ... The calculations that should be executed.
#' @param monitor FALSE by default. If TRUE, outputs two charts to visualize the
#' functions time consumption.
#'
#' @details
#' The loop you can use within [compute()] is based on the 'SAS' do-over-loop. This
#' type of loop iterates over every vector that appears in the loop in parallel.
#' Means that in the first iteration all the first vector elements are used, in the
#' second iteration all second elements of every vector, and so on. With this loop
#' you don't have the need to construct an outer loop, but can directly pass in
#' different vectors and let the function handle the loop inside.
#'
#' @return Returns a data frame with newly computed variables.
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
#' # Simple assignment
#' assign_df <- my_data |> compute(new_var1 = 1,
#'                                 new_var2 = "Hello")
#'
#' # Simple calculation
#' sum_df <- my_data |> compute(new_sum = age + sex)
#'
#' # Using functions
#' mean_df <- my_data |> compute(new_mean = collapse::fmean(age))
#'
#' # Using qol functions
#' qol_df <- my_data |> compute(row_sum = row_calculation("sum", state, age, sex))
#'
#' # Use compute() as a do-over-loop. In this kind of loop all vectors will be
#' # advanced one iteration at a time in parallel.
#' new_vars <- c("var1", "var2", "var3")
#' money    <- c("income", "expenses", "balance")
#' multi    <- c(1, 2, 3)
#'
#' do_over_df <- my_data |> compute(new_vars = money * multi)
#'
#' # You can also do all at once
#' all_df <- my_data |> compute(new_var1 = 1,
#'                              new_var2 = "Hello",
#'                              new_sum  = age + sex,
#'                              new_mean = mean(age),
#'                              row_sum  = row_calculation("sum", state, age, sex),
#'                              new_vars = multi * money)
#'
#' # compute() can be used in a do_if() situation and is aware of overarching
#' # conditions.
#' age_west. <- discrete_format("under 18"     = 0:17,
#'                              "18 and older" = 18:100)
#'
#' age_east. <- discrete_format("under 65"     = 0:64,
#'                              "65 and older" = 65:100)
#'
#' do_if_df <- my_data |>
#'     do_if(state < 11) |>
#'           compute(region    = "West",
#'                   age_group = recode(age = age_west.)) |>
#'     else_do() |>
#'           compute(region    = "East",
#'                   age_group = recode(age = age_east.)) |>
#'     end_do()
#'
#' @export
compute <- function(data_frame,
                    ...,
                    monitor = .qol_options[["monitor"]]){
    # Measure the time
    print_start_message()

    #-------------------------------------------------------------------------#
    monitor_df <- NULL |> monitor_start("Get do_if() condition", "do_if()")
    #-------------------------------------------------------------------------#
    parent_env  <- parent.frame()
    assignments <- as.list(substitute(list(...)))[-1]

    if (length(assignments) == 0){
        print_message("ERROR", "No assignments. Evaluation will be aborted.")
        return(invisible(data_frame))
    }

    # Check whether compute is used within an if.() statement. If that is the
    # case, then a hidden parameter is passed, carrying the condition. Additionally
    # check, if there are any do_if() conditions active and apply them
    if_condition_list <- NULL
    if_suppressed     <- FALSE

    if (".if_condition" %in% names(assignments)){
        condition                      <- TRUE
        if_condition_list              <- eval(assignments[[".if_condition"]], envir = parent_env)
        assignments[[".if_condition"]] <- NULL
    }
    else{
        condition <- data_frame |> combined_condition(TRUE)
    }

    if (".if_parent_frame" %in% names(assignments)){
        parent_env                        <- eval(assignments[[".if_parent_frame"]], envir = parent_env)
        assignments[[".if_parent_frame"]] <- NULL
    }

    if (".if_suppressed" %in% names(assignments)){
        assignments[[".if_suppressed"]] <- NULL
        if_suppressed <- TRUE
    }

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

    list_entry_lengths <- lengths(content_list)

    if (length(collapse::funique(list_entry_lengths)) > 1){
        print_message("ERROR", c("Passed vectors are of unequal lengths. All vectors must have an",
								 "equal number of elements. Evaluation will be aborted."))
        return(invisible(data_frame))
    }

    print_step("MAJOR", "Computing stats")

    # Go trough each assignment and collect the translated calls
    call_list <- list()

    for (entry in seq_along(assignments)){
        variable    <- names(assignments)[[entry]]
        calculation <- assignments[[variable]]
        calc_text   <- deparse(calculation)

        # If no vector was passed then evaluate as normal.
        if (!variable %in% names(content_list)){
            #-------------------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next(paste0(variable, " = ", calc_text), "Non vector")
            #-------------------------------------------------------------------------#
            print_step("MINOR", "[var] = [calc]", var = variable, calc = calc_text)

            # This step is important to make this function work in a nested situation.
            # Normally variable would be the name of what was last passed as a parameter.
            # If "if.()" is used nested inside a function this can basically be any placeholder.
            # So here we go up the ladder to get the original name of the variable.
            original_var <- get_origin_symbol(variable)

            # Evaluate complete assignment first without condition
            expression <- get_custom_functions(calculation, parent_env)
            value      <- suppressMessages(eval(expression, envir = data_frame))

            # If there already is a variable with the given name pick the existing value as fallback
            flag_var_in_data <- FALSE

            if (original_var %in% names(data_frame)){
                flag_var_in_data <- TRUE

                # Check if existing variable type is of same type as assigned value.
                # Put out a warning on type mismatch.
                is_type_missmatch <- check_types(data_frame, original_var, value)

                # If result is NULL, then there is a variable with all NA values
                # which has to be converted into the right type.
                if (is.null(is_type_missmatch)){
                    if (is.character(value)){
                        data_frame[[variable]] <- NA_character_
                    }
                    else if (is.numeric(value)){
                        data_frame[[variable]] <- NA_real_
                    }
                }
                # Convert to character on type miss match
                else if (is_type_missmatch){
                    data_frame[[original_var]] <- as.character(data_frame[[original_var]])
                    value <- as.character(value)
                }
            }

            # Merge the given condition as well as the active filter variables together
            # into one logical vector
            if (!is.null(if_condition_list)){
                full_condition <- condition & unlist(if_condition_list[[entry]])
            }
            else{
                full_condition <- condition
            }

            # Values are now conditionally evaluated. Which route is used depends on whether
            # this function is called on its own or via an if statement.
            if (!is.null(if_condition_list)){
                # When function is called via if statement and variable is in the
                # data frame, then variable will be created, but fallback value is
                # not NA, but the values which are already there. This is in case of
                # else_if.() or else.().
                if (flag_var_in_data){
                    conditional_value <- data.table::fifelse(full_condition, value, data_frame[[original_var]])
                }
                # When function is called via if statement and variable is not already in the
                # data frame, then variable will be created.
                else{
                    if (length(full_condition) == 1){
                        conditional_value <- value
                    }
                    else{
                        conditional_value <- data.table::fifelse(full_condition, value, NA)
                    }
                }
            }
            # When function is called on its own, variable will be created, ignoring whether
            # it is already in the data frame or not.
            else{
                if (length(full_condition) == 1){
                    conditional_value <- value
                }
                else{
                    # When function is called with active do_if statement and variable
                    # is in the data frame, then variable will be created, but fallback
                    # value is not NA, but the values which are already there.
                    if (flag_var_in_data){
                        conditional_value <- data.table::fifelse(full_condition, value, data_frame[[original_var]])
                    }
                    else{
                        conditional_value <- data.table::fifelse(full_condition, value, NA)
                    }
                }
            }

            # Only add call, if there are evaluated values. With else_if.() or else.()
            # it can happen that no values are generated, in case there is no original
            # variable in the data frame.
            if (length(conditional_value) > 0){
                new_call <- stats::setNames(list(conditional_value), variable)

                if (names(new_call) %in% names(call_list)){
                    print_message("WARNING", "Duplicate variable name '{var}' in compute. Only the first entry will be used.",
                                  var = names(new_call))
                }
                else{
                    # Add to call list
                    call_list <- c(call_list, new_call)
                }
            }
        }
        # If a vector was passed, then evaluate as a do over loop. Meaning for each
        # vector the same elements are used simultaneously one after another.
        else{
            #-------------------------------------------------------------------------#
            monitor_df <- monitor_df |> monitor_next(paste0(variable, " = ", calc_text), "Vector based")
            #-------------------------------------------------------------------------#

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
                })

                # Get the target variable from the assignment list and look whether
                # it is a vector or not. If it is a vector, extract the current element
                # and use its name.
                target_variable <- variable

                if (target_variable %in% names(content_list)){
                    target_variable <- content_list[[target_variable]][element]
                }

                # Evaluate complete assignment first without condition
                value_var  <- do.call(substitute, list(calculation, replace_list))
                expression <- get_custom_functions(value_var, parent_env)
                value      <- eval(expression, envir = data_frame)

                print_step("MINOR", "{target} = [value]", target = target_variable, value = deparse(value_var))

                # Look up, if single value was passed or vector of values. Only if a
                # vector of values is passed, which has fewer observations than the
                # data frame, it will be processed element wise.
                number_of_values <- length(value)
                number_of_rows   <- collapse::fnrow(data_frame)

                if (number_of_values > 1 && number_of_values < number_of_rows){
                    value <- value[element]
                }

                # If there already is a variable with the given name pick the existing value as fallback
                flag_var_in_data <- FALSE

                if (target_variable %in% names(data_frame)){
                    flag_var_in_data <- TRUE

                    # Check if existing variable type is of same type as assigned value.
                    # Put out a warning on type mismatch.
                    is_type_missmatch <- check_types(data_frame, target_variable, value)

                    # If result is NULL, then there is a variable with all NA values
                    # which has to be converted into the right type.
                    if (is.null(is_type_missmatch)){
                        if (is.character(value)){
                            data_frame[[variable]] <- NA_character_
                        }
                        else if (is.numeric(value)){
                            data_frame[[variable]] <- NA_real_
                        }
                    }
                    # Convert to character on type miss match
                    else if (is_type_missmatch){
                        data_frame[[target_variable]] <- as.character(data_frame[[target_variable]])
                        value <- as.character(value)
                    }
                }

                # Check whether there are additional conditions active via do_if
                if (!is.null(if_condition_list)){
                    full_condition <- condition & if_condition_list[[element]]
                }
                else{
                    full_condition <- condition
                }

                # Values are now conditionally evaluated. Which route is used depends on whether
                # this function is called on its own or via an if statement.
                if (!is.null(if_condition_list)){
                    # When function is called via if statement and variable is in the
                    # data frame, then variable will be created, but fallback value is
                    # not NA, but the values which are already there. This is in case of
                    # else_if.() or else.().
                    if (flag_var_in_data){
                        conditional_value <- data.table::fifelse(full_condition, value, data_frame[[target_variable]])
                    }
                    # When function is called via if statement and variable is not already in the
                    # data frame, then variable will be created.
                    else{
                        if (length(full_condition) == 1){
                            conditional_value <- value
                        }
                        else{
                            conditional_value <- data.table::fifelse(full_condition, value, NA)
                        }
                    }
                }
                # When function is called on its own, variable will be created, ignoring whether
                # it is already in the data frame or not.
                else{
                    if (length(full_condition) == 1){
                        conditional_value <- value
                    }
                    else{
                        # When function is called with active do_if statement and variable
                        # is in the data frame, then variable will be created, but fallback
                        # value is not NA, but the values which are already there.
                        if (flag_var_in_data){
                            conditional_value <- data.table::fifelse(full_condition, value, data_frame[[target_variable]])
                        }
                        else{
                            conditional_value <- data.table::fifelse(full_condition, value, NA)
                        }
                    }
                }

                new_call <- stats::setNames(list(conditional_value), target_variable)

                # Add to call list
                call_list <- c(call_list, new_call)
            }
        }
    }

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_next("Add variables", "Vector based")
    #-------------------------------------------------------------------------#
    print_step("MAJOR", "Add variables to data frame")

    if (length(call_list) > 0){
        data_frame <- collapse::ftransform(data_frame, call_list)
    }

    if (if_suppressed){
        print_closing(suppress = TRUE)
    }
    else{
        print_closing()
    }

    #-------------------------------------------------------------------------#
    monitor_df <- monitor_df |> monitor_end()
    monitor_df |> monitor_plot(draw_plot = monitor)
    #-------------------------------------------------------------------------#

    data_frame
}


#' Translate Expressions
#'
#' @description
#' Translates expressions like calculations, functions based on vectors and
#' custom functions from this package into the final expression to evaluate.
#' All expressions basically stay the same, just the custom functions from this
#' package get the "data_frame" argument injected.
#'
#' @param expression The expression to be translated.
#' @param env The parent environment in which the expression has to be evaluated.
#'
#' @return
#' Returns a translated expression
#'
#' @noRd
get_custom_functions <- function(expression, env){
    # If the expression is not just a number or text, check what type of call it is
    if (is.call(expression)){
        # Decide whether expression is a function or a simple calculation
        custom_function <- tryCatch({
            # Force evaluation to see if it exists
            eval(expression[[1]], env)
        }, error = function(e){
            # Evaluation failed
            NULL
        })

        # If expression is a function check whether it is a function from this package
        if (is.function(custom_function)){
            function_args <- names(formals(custom_function))

            # If the first function argument is "data_frame" then the function is
            # from this package. In this case "data_frame" need to be added as first
            # argument so that the function can be evaluated correctly.
            if (length(function_args) > 0 && function_args[1] == "data_frame"){
                expression <- as.call(c(expression[[1]], quote(data_frame), as.list(expression[-1])))
            }
        }
    }

    expression
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
        return(NULL)
    }

    type_c <- typeof(current)
    type_d <- typeof(data_frame[[variable]])

    # Abort, if types are identical
    if (identical(type_c, type_d)){
        return(FALSE)
    }
    # Also abort, if both types are of a numerical type
    else if(type_c %in% c("integer", "double") && type_d %in% c("integer", "double")){
        return(FALSE)
    }

    print_message("WARNING", c("Type mismatch: Current value [current] is of type [type_c] but should be of",
							   "type [type_d]. [variable] will be converted to character."),
                  current = current[1], type_c = type_c, type_d = type_d, variable = variable,
				  always_print = TRUE)

    TRUE
}
