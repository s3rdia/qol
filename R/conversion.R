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
    if (is.call(argument) && identical(argument[[1]], quote(`:`))){
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
            # If the value is a column of a data frame inside the calling stack
            # (e.g. when the function is evaluated inside compute.() with the
            # data frame as evaluation environment), the substituted symbol is
            # the variable name. Return the name instead of the column contents.
            if (is.symbol(substituted) &&
                symbol_is_data_frame_column(as.character(substituted), original)){
                as.character(substituted)
            }
            else{
                original
            }
        }
        else{
            args_to_char(substituted)
        }
    }, error = function(e){
        # If evaluation failed (object not found), convert the symbol names.
        # Symbols which are parameters of an enclosing custom function are
        # resolved to the argument values provided by the user of that custom
        # function.
        args_to_char_with_params(substituted)
    })
}


#' Convert Variable Names From An Expression
#'
#' @description
#' args_to_char_with_params: is used when the direct evaluation of an argument fails
#' (e.g. because the argument contains a  bare variable name which is not defined
#' in the calling environment). It also resolves symbols which correspond to
#' parameters of an enclosing custom function.
#'
#' @param argument The captured expression of the function argument.
#'
#' @return
#' Returns a character vector of variable names.
#'
#' @noRd
args_to_char_with_params <- function(argument){
    # Build a snapshot of the calling stack once and resolve all symbols
    # against that same snapshot.
    args_to_char_with_params_rec(argument, build_resolution_cache())
}


#' Convert Variable Names From An Expression
#'
#' @description
#' The snapshot of the calling stack is passed down so that every symbol is resolved
#' against the same set of frames.
#'
#' @param argument The captured expression of the function argument.
#' @param snapshot The snapshot of the calling stack.
#'
#' @return
#' Returns a character vector of variable names.
#'
#' @noRd
args_to_char_with_params_rec <- function(argument, snapshot){
    # Null: Return as is
    if (is.null(argument)){
        return(NULL)
    }

    # Single Character: Just return as is
    if (is.character(argument)){
        return(argument)
    }

    # Single symbol: Convert to character and return. Symbols which are
    # parameters of an enclosing custom function are resolved to the argument
    # values provided by the user before that.
    if (is.symbol(argument)){
        value <- resolve_custom_function_argument(argument, snapshot)

        if (!is.null(value)){
            return(value)
        }

        return(as.character(argument))
    }

    # Vector/List: Convert all elements to character with parameter resolution.
    # vapply replicates the exact behaviour of args_to_char: names are
    # preserved and a nested element which resolves to more than one value
    # results in an error.
    if (is.call(argument) && (identical(argument[[1]], quote(c)) ||
                              identical(argument[[1]], quote(list)))){
        return(vapply(argument[-1], args_to_char_with_params_rec,
                      character(1), snapshot = snapshot))
    }

    # If there is a ":" in the call, this returns the call as is so that it can be used
    # as in e.g. keep/dropp to make use of ranges.
    if (is.call(argument) && identical(argument[[1]], quote(`:`))){
        return(paste(as.character(argument[[2]]),
                     as.character(argument[[3]]),
                     sep = ":"))
    }

    stop(" X ERROR: Something went wrong with the argument conversion.\n",
         "          Only single character and symbols, as well as vectors and flat lists are allowed.\n",
         "          Function will be aborted.")
}


#' Build A Snapshot Of The Calling Stack
#'
#' @description
#' Walks the calling stack once and stores every frame together with its formal
#' argument names and a flag whether the frame belongs to a package (i.e. a
#' namespace). Additionally the union of all formals of custom (non-package)
#' functions is computed.
#'
#' @return
#' A list with the elements "frames" (one entry per frame, NULL when the call
#' could not be retrieved) and "union" (the unique formal names of all custom
#' functions on the stack).
#'
#' @noRd
build_resolution_cache <- function(){
    number_of_frames <- sys.nframe()
    frames           <- vector("list", number_of_frames)
    main_union       <- character(0)

    for (i in seq_len(number_of_frames)){
        fn <- tryCatch(sys.function(i), error = function(e) NULL)

        if (is.null(fn)){
            frames[[i]] <- NULL
            next
        }

        frame <- list(ns    = isNamespace(environment(fn)),
                      fname = names(formals(fn)),
                      env   = sys.frame(i))

        frames[[i]] <- frame

        if (!frame[["ns"]]){
            main_union <- c(main_union, frame[["fname"]])
        }
    }

    list(frames = frames, union = unique(main_union))
}


#' Resolve The Argument Value Of An Enclosing Custom Function
#'
#' @description
#' Looks up the snapshot of the calling stack for a function which is not a
#' package function and contains a formal argument with the name of the given
#' symbol. In that case the argument value provided to the custom function by
#' its caller is returned as character.
#'
#' @param argument The symbol to look up in the snapshot of the calling stack.
#' @param snapshot The snapshot of the calling stack.
#'
#' @return
#' The argument value as character or NULL when no enclosing custom function
#' parameter matches the symbol.
#'
#' @noRd
resolve_custom_function_argument <- function(argument, snapshot){
    argument <- as.character(argument)

    # An empty expression (e.g. a missing function argument which was captured
    # with substitute()) cannot be resolved.
    if (length(argument) == 0 || is.na(argument)){
        return(NULL)
    }

    # Fast path: the symbol does not name a parameter of any custom function on
    # the stack, so it cannot be resolved at all.
    if (!(argument %in% snapshot$union)){
        return(NULL)
    }

    # Walk the frames from the innermost to the outermost one
    for (fr in snapshot$frames){
        if (is.null(fr) || fr$ns){
            next
        }

        if (!(argument %in% fr$fname)){
            next
        }

        # Get the expression handed over for this parameter. This does not
        # evaluate the argument, it only substitutes the symbol in the calling
        # frame.
        arg_expr <- tryCatch(
            eval(substitute(substitute(sym, env),
                            list(sym = as.name(argument), env = fr$env))),
            error = function(e) NULL
        )

        if (is.null(arg_expr)){
            next
        }

        # Literal values provided by the user of the custom function
        if (is.character(arg_expr) || is.numeric(arg_expr)){
            return(as.character(arg_expr))
        }

        # A bare symbol (e.g. a wrapper parameter which was passed on from an
        # even more outer custom function, or a data frame column name). Try to
        # force the contained value. If that fails, keep the symbol name.
        if (is.symbol(arg_expr)){
            value <- tryCatch(get(as.character(arg_expr), envir = fr$env,
                                  inherits = FALSE),
                              error = function(e) NULL)

            if (is.character(value) || is.numeric(value)){
                return(as.character(value))
            }

            return(as.character(arg_expr))
        }

        # Any other expression (e.g. c(...)) is converted recursively
        return(args_to_char_with_params_rec(arg_expr, snapshot))
    }

    NULL
}


#' Get The Original Symbol From Parent Environments
#'
#' @description
#' When nesting functions and trying to catch symbols, it
#' often happens that 'R' looses contact to the original symbol. This function looks
#' up the parent environments until it finds the original symbol and returns its
#' contents as character.
#'
#' In case the originally passed argument is an unresolvable symbol (e.g. a variable
#' name), this function returns original argument passed down as characxter instead of
#' the contents.
#'
#' @param symbol The symbol to look up in the parent environments.
#'
#' @return
#' The original symbols contents or name as character.
#'
#' @noRd
get_origin_symbol <- function(symbol){
    symbol <- as.character(symbol)

    # Loop through the tree of parent environments
    for (i in rev(seq_len(sys.nframe()))){
        env <- sys.frame(i)

        # If the environment is a data frame (e.g. when the expression is
        # evaluated inside compute.() with the data frame as evaluation
        # environment), the symbol is a column name. Return the name instead
        # of the column contents in this case.
        if (inherits(env, "data.frame")){
            if (symbol %in% names(env)){
                return(symbol)
            }

            next
        }

        # If the symbol is found in the current parent environment return it
        value <- tryCatch({
            # Force evaluation to see if it exists
            get0(substitute(symbol), envir = env, inherits = FALSE)
        }, error = function(e){
            # Nested substitute:
            # 1. Inner: substitute(substitute(sym, env), list(sym = symbol))
            #    This builds the expression "substitute(var_name, env)"
            # 2. Outer: eval(...)
            #    This gets the expression in the correct context
            as.character(eval(substitute(
                substitute(sym, env), list(sym = as.name(symbol)))))
        })

        if (!is.null(value) && is.character(value)){
            # If the value found is a column of a data frame inside the calling
            # stack, the symbol refers to that column. Return the column name
            # instead of the column contents in this case. This happens when
            # the expression is evaluated inside compute.() with the data frame
            # as evaluation environment where the columns are bound as separate
            # objects.
            if (symbol_is_data_frame_column(symbol, value)){
                return(symbol)
            }

            return(value)
        }
    }

    symbol
}



#' Get The Original Symbol From Parent Environments
#'
#' @description
#' Check whether the given symbol refers to a column of a data frame in the
#' calling stack. The found value is compared against the column contents in
#' order to avoid false positives with ordinary character vectors.
#'
#' @param symbol The symbol to look up in the parent environments.
#' @param value The value to look up in the parent environments.
#'
#' @return
#' TRUE or FALSE
#'
#' @noRd
symbol_is_data_frame_column <- function(symbol, value){
    for (i in seq_len(sys.nframe())){
        frame <- sys.frame(i)

        # Data frames used as evaluation environments (e.g. inside compute.())
        # are part of the calling stack directly. In this case the columns can
        # be compared without having to force any bindings.
        if (inherits(frame, "data.frame") &&
            symbol %in% names(frame) &&
            identical(value, frame[[symbol]])){
            return(TRUE)
        }

        # Get the formal argument names of the function which created this
        # frame. Forcing such unevaluated argument promises out of context
        # interrupts their evaluation. When they are evaluated for real later
        # on, this can lead to "restarting interrupted promise evaluation"
        # warnings. It can also trigger the recursive evaluation of complete
        # pipelines when the data frame argument is still unforced.
        frame_function <- tryCatch(sys.function(i), error = function(e) NULL)

        skippable <- if (is.function(frame_function)){
            names(formals(frame_function))
        }
        else{
            NULL
        }

        # Only force bindings which are not unforced argument promises of the
        # current frame
        for (binding in setdiff(ls(frame, all.names = TRUE), skippable)){
            candidate <- tryCatch(get0(binding, envir = frame, inherits = FALSE),
                                  error = function(e) NULL)

            if (inherits(candidate, "data.frame") &&
                symbol %in% names(candidate) &&
                identical(value, candidate[[symbol]])){
                return(TRUE)
            }
        }

        # The data frame which serves as the evaluation environment (e.g. the
        # "data_frame" argument of compute.()) normally ends up in "skippable"
        # because its name matches a formal argument name, although it is
        # already fully evaluated at that point.
        if ("data_frame" %in% ls(frame, all.names = TRUE)){
            data_frame_binding <- tryCatch(get0("data_frame", envir = frame, inherits = FALSE),
                                           error = function(e) NULL)

            if (inherits(data_frame_binding, "data.frame") &&
                symbol %in% names(data_frame_binding) &&
                identical(value, data_frame_binding[[symbol]])){
                return(TRUE)
            }
        }
    }

    FALSE
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
			# NOTE: I use base unique here instead of collapse::funique, because the
			#       base function looks at the visual characters while the collapse
			#       function looks at the raw binary resulting in e.g. ä != ä and
			#       therefore causes duplicate expressions.
            label_levels <- data_frame[[variable]] |>
                unlist(use.names = FALSE) |>
                unique() |>
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


#' Convert Numeric Character Variables Into Sortable Factors
#'
#' @description
#' convert_ordered_factor: converts all given character variables that only
#' contain numbers into ordered factors. The factor levels are sorted in numerical
#' order so that the variable is sorted numerically, while its original textual
#' representation is preserved. Variables which are not character or which contain
#' non numerical values are left unchanged.
#'
#' @param data_frame A data frame containing variables to convert.
#' @param variables Variables from the data frame which should be converted.
#'
#' @return
#' Returns the same data frame with converted variables.
#'
#' @rdname convert_variables
#'
#' @noRd
convert_ordered_factor <- function(data_frame, variables){
    data_frame[variables] <- lapply(data_frame[variables], function(variable){
        # Only character variables will be converted
        if (!is.character(variable)){
            return(variable)
        }

        # Try to convert to numeric
        var_converted <- suppressWarnings(as.numeric(variable))

        # If NA values are equal, conversion was successful
        if (!all(is.na(variable) == is.na(var_converted))){
            return(variable)
        }

        # Sort the unique values in numerical order to get the factor levels
        label_levels <- unique(variable)
        label_levels <- label_levels[!is.na(label_levels)]
        label_levels <- label_levels[order(as.numeric(label_levels))]

        # Convert variable to a numerically ordered factor
        factor(variable, levels = label_levels, ordered = TRUE)
    })

    data_frame
}


#' Convert Color Codes
#'
#' @name hex_ansi
#'
#' @description
#' [hex_to_256()]: Generate a 256-color 6x6x6 color cube.
#'
#' @param hex_color The hex color code to convert.
#'
#' @return
#' [hex_to_256()]: Returns 6x6x6 color cube.
#'
#' @examples
#' color_cube <- hex_to_ansi("#32CD32")
#'
#' @rdname hex_ansi
#'
#' @export
hex_to_256 <- function(hex_color){
    rgb <- as.vector(grDevices::col2rgb(hex_color))

    # Generate a 256-color 6x6x6 color cube: 16 + 36*r + 6*g + b
    # where r,g,b are 0-5
    values <- floor(rgb / 51)

    16 + (36 * values[1]) + (6 * values[2]) + values[3]
}


#' @description
#' [hex_to_ansi()]: Applies hex color and font weight to a text as ansi codes.
#'
#' @return
#' [hex_to_ansi()]: Returns formatted text.
#'
#' @param text The text which contains hex color codes to be convert into ansi
#' style formatting.
#' @param hex_color The hex color code to convert.
#' @param bold FALSE by default. If TRUE inserts ansi code for bold printing
#'
#' @examples
#' formatted_text <- hex_to_ansi("This is a text to test the coloring",
#'                               hex_color = "#32CD32", bold = TRUE)
#'
#' @rdname hex_ansi
#'
#' @export
hex_to_ansi <- function(text,
                        hex_color = NULL,
                        bold      = FALSE){
    prefix <- ""

    # If bold, text has to be surrounded with bold code
    if (bold){
        prefix <- paste0(prefix, "\033[1m")
    }

    # If color should be applied
    if (!is.null(hex_color)){
        color_256 <- hex_to_256(hex_color)

        # Use the 8-bit extended sequence: 38;5;Nm
        prefix <- paste0(prefix, sprintf("\033[38;5;%dm", color_256))
    }

    # Return fully styled text. Add old end as suffix
    paste0(prefix, text, "\033[0m")
}
