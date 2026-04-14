###############################################################################
# Notes, warnings and errors
###############################################################################

#' Print Styled Messages
#'
#' @name messages
#'
#' @description
#' Printing styled messages for different occasions. There are notifications,
#' warnings, errors, function call headlines, progress and function closing
#' messages. Or just a neutral one.
#'
#' @param type The message type, so that the function knows which symbol and coloring
#' to use.
#' @param text The message text to display.
#' @param ... Additional information to display like variable names. To use
#' these write &#91;<NAME YOU PUT IN>&#93; in the text.
#' @param always_print FALSE by default. If TRUE, prints headlines even in deeper
#' nested situations.
#' @param utf8 Whether to display complex characters or just plain text.
#'
#' @details
#' The message types in which you can enter custom texts, are capable of using
#' different styling operators. These are:
#'
#' - Insert list of elements: &#91;vector_name&#93;
#' - Adding conditional words, if list of elements has more than one element: &#91;?word&#93;
#' - Adding conditional singular/plural, depending on list of element length: &#91;?singular/plural&#93;
#' - Bold, italic and underline: &#91;b&#93;some text&#91;/b&#93;, &#91;i&#93;some text&#91;/i&#93;, &#91;u&#93;some text&#91;/u&#93;
#' - Coloring parts of the message: &#91;#FF00FF some text&#93;
#'
#' @seealso
#' Also have a look at the small helpers: [get_message_stack()], [set_no_print()],
#' [print_stack_as_messages()], [convert_square_brackets()]
#'
#' @return
#' Return text without styling or total running time.
#'
#' @examples
#' # Example messages
#' print_message("NOTE", c("Just a quick note that you can also insert e.g.[? a / ]variable",
#'                         "name[?s] like this: [listing].",
#'                         "Depending on the number of variables you can also alter the text."),
#'              listing = c("age", "state", "NUTS3"))
#'
#' print_message("WARNING", "Just a quick [#FF00FF colored warning]!")
#'
#' print_message("ERROR", "Or an error")
#'
#' print_message("NEUTRAL", c("You can also just output [u]plain text[/u] if you like and use",
#'                            "[#FFFF00[b]all the different[/b] [i]formatting options.[/i]]"))
#'
#' @rdname messages
#'
#' @export
print_message <- function(type,
                          text,
                          ...,
                          always_print = FALSE,
                          utf8         = .qol_messages[["format"]][["utf8"]]){
    suppressed <- FALSE

    # Identify which function called the message
    caller <- sys.call(-1)

    if (is.call(caller) && is.name(caller[[1]])){
        # Get calling function and code line the message was called from
        caller <- ifelse(is.null(caller), "global", as.character(caller[[1]]))
    }
    else{
        caller <- "do.call"
    }

    if (!is.list(type) && !tolower(type) %in% c("note", "warning", "error", "neutral")){
        type <- "neutral"
    }

    depth <- sys.nframe()

    # Whenever a code block is executed, a unique token is generated. Based on this
    # token it can be decided, whether we are starting a new message stack (in case we
    # have a new token) or we keep the current stack (in case of multiple functions
    # running as a block)
    current_session_token   <- format(sys.frame(1))
    current_execution_token <- get_execution_token()

    # In case it is the first starting message in the current run or if the stack
    # has a not suppressed end not. Meaning a function or pipe operation has finished.
    if (!identical(current_session_token, .qol_messages[["last_session"]]) ||
        !identical(current_execution_token, .qol_messages[["last_execution"]])){

        reset_messages()
    }

    # Add message to global stack
    if (is_stack() && !is_full_stack()){
        entry_nr <- length(.qol_messages[["stack"]]) + 1
    }
    # If no starting point is set, the message is called inside a function where no
    # starting message was called. In this case only capture the last message and reset
    # the message stack. Otherwise the stack would just grow and never reset.
    else{
        if (length(.qol_messages[["stack"]]) > 1){
            .qol_messages[["stack"]]        <- list()
            .qol_messages[["last_message"]] <- NULL
        }

        entry_nr <- 1
    }

    # Don't print message, if messages are suppressed
    if (!always_print && is_message_suppressed()){
        text       <- paste(text, collapse = " ")
        suppressed <- TRUE
    }
    # If messages are not suppressed
    else{
        last_message <- get_last_active_message()

        # If last message is a timed message, print a new line first, otherwise the
        # function would print on the same line.
        if (is_time_stamp_allowed(last_message) && !is_no_print_active()){
            cat("\n")
        }

        text <- print_to_console(type = type, text = text, ..., utf8 = utf8)
    }

    # Catch type for message stack on custom messages
    if (is.list(type)){
        type <- type[["type"]]
    }

    # Add message to global stack
    .qol_messages[["stack"]][[entry_nr]] <- list(type       = toupper(type),
                                                 text       = text,
                                                 suppressed = suppressed,
                                                 new_line   = TRUE,
                                                 print_time = FALSE,
                                                 depth      = depth,
                                                 caller     = caller,
                                                 call_stack = sys.calls(),
                                                 time       = Sys.time())

    invisible(text)
}

# TODO: Unit tests
# TODO: Update README

#' @description
#' This is the core printing function which formats the text and prints it to the
#' console.
#'
#' @param new_line TRUE by default. Whether to directly add a new line to the console
#' or not.
#'
#' @noRd
print_to_console <- function(type,
                             text,
                             ...,
                             utf8     = .qol_messages[["format"]][["utf8"]],
                             new_line = TRUE){
    # If normal pre defined message
    if (!is.list(type)){
        type <- tolower(type)

        if (!type %in% c("note", "warning", "error", "neutral", "major", "minor", "grey")){
            type <- "neutral"
        }

        # Output message
        if (utf8){
            # First format all line breaks so that the next text lines are indented
            seperator <- paste0("\n", .qol_messages[["format"]][[paste0(type, "_indent_ansi")]])
            text      <- paste(text, collapse = seperator)

            # Insert special grey message format
            if (type == "grey"){
                text <- paste0("[i][", .qol_messages[["format"]][["time_color"]], " ", text, "][/i]")
            }

            # Insert formatting
            text_orig <- convert_square_brackets(text, ...)
            text      <- paste(.qol_messages[["format"]][[paste0(type, "_ansi")]], text_orig, sep = "")
        }
        else{
            # First format all line breaks so that the next text lines are indented
            seperator <- paste0("\n", .qol_messages[["format"]][[paste0(type, "_indent_pt")]])
            text      <- paste(text, collapse = seperator)

            # Insert special grey message format
            if (type == "grey"){
                text <- paste0("[i][", .qol_messages[["format"]][["time_color"]], " ", text, "][/i]")
            }

            # Insert formatting
            text_orig <- convert_square_brackets(text, ...)
            text      <- paste(.qol_messages[["format"]][[paste0(type, "_pt")]], text_orig, sep = "")
        }
    }
    # If custom message
    else{
        # First format all line breaks so that the next text lines are indented
        if (utf8){
            message_start <- paste0(rep("\u00a0", type[["indent"]]), type[["ansi_icon"]])
        }
        else{
            message_start <- paste0(rep("\u00a0", type[["indent"]]), type[["text_icon"]])
        }

        # Build the line indentation for each line after the first one
        seperator <- paste0("\n", message_start, " ", strrep("\u00a0", nchar(type[["type"]]) + 2))

        # Insert special message format
        if (!is.null(type[["text_color"]])){
            text <- paste0("[", type[["text_color"]], " ", text, "]")
        }
        if (type[["text_bold"]]){
            text <- paste0("[b]", text, "[/b]")
        }
        if (type[["text_italic"]]){
            text <- paste0("[i]", text, "[/i]")
        }
        if (type[["text_underline"]]){
            text <- paste0("[u]", text, "[/u]")
        }

        # Paste multiline text together
        text <- paste(text, collapse = seperator)

        # Insert formatting
        text_orig <- convert_square_brackets(text, ...)

        # Generate timed type and put final message together
        if ("timed" %in% names(type)){
            note_text <- hex_to_ansi(paste(message_start, " ", sep = ""), hex_color = type[["color"]], bold = TRUE)
            text      <- paste(note_text, text_orig, sep = "")
        }
        # Generate colored note type and put final message together
        else{
            note_text <- hex_to_ansi(paste(message_start, " ", type[["type"]], ": ", sep = ""), hex_color = type[["color"]], bold = TRUE)
            text      <- paste(note_text, text_orig, sep = "")
        }
    }

    # Print message to the console conditionally.
    prefix <- ifelse(is_new_line_necessary(), "\n", "")

    if (!is_no_print_active()){
        # Put a line break afterwards, otherwise the next message would be printed on the same line.
        if (new_line){
            caller <- as.character(sys.call(-1))[1]

            # Print as normal
            if (prefix == "" || caller == "print_message"){
                cat(text, "\n")
            }
            # Print a new line before text, because the previous message didn't put a line break afterwards
            else{
                cat(prefix, text, "\n", sep = "")
            }
        }
        # Print without line break afterwards to be able to put time stamps behind the messages
        else{
            cat(text)
        }
        utils::flush.console()
    }

    # Remove line breaks and doubled blanks to receive the full message in one piece
    text_orig  <- gsub(seperator, " ", text_orig, fixed = TRUE)

    # Remove individual styling
    ansi_regex <- "(?>\\x1B\\[|\\033\\[)[0-9;:]*[a-zA-Z]"
    text_orig  <- gsub(ansi_regex, "", text_orig, perl = TRUE)

    # Set global last text, which is used to print time stamps
    .qol_messages[["last_message"]] <- text

    invisible(text_orig)
}


###############################################################################
# Headline and closing
###############################################################################

#' @param line_char The character that that forms the line.
#' @param max_width The maximum number of characters drawn, which determines the
#' line length of the headline.
#'
#' @examples
#' # Different headlines
#' print_headline("This is a headline")
#'
#' print_headline("[#00FFFF This is a different headline] with some color",
#'                line_char = "-")
#'
#' print_headline("[b]This is a very small[/b] and bold headline",
#'                line_char = ".",
#'                max_width = 60)
#'
#' @rdname messages
#'
#' @export
print_headline <- function(text,
                           ...,
                           line_char    = "=",
                           max_width    = getOption("width"),
                           always_print = FALSE){
    suppressed <- FALSE

    # Identify which function called the message
    caller <- sys.call(-1)

    if (is.call(caller) && is.name(caller[[1]])){
        # Get calling function and code line the message was called from
        caller <- ifelse(is.null(caller), "global", as.character(caller[[1]]))
    }
    else{
        caller <- "do.call"
    }

    # Don't print message, if messages are suppressed
    depth <- sys.nframe()

    if (!always_print && is_message_suppressed()){
        suppressed <- TRUE
        text_orig  <- text
    }
    else{
        # Put time stamp at the end of the last message if present
        print_time_stamp()

        # Only get first element, if a vector is given
        if (length(text) > 1){
            text <- text[[1]]
        }

        # Only one character allowed
        if (nchar(line_char) > 1){
            line_char <- substr(line_char, 1, 1)
        }

        # Insert formatting
        text <- convert_square_brackets(text, ...)

        # Remove individual styling to be able to measure the right headline width
        ansi_regex <- "(?>\\x1B\\[|\\033\\[)[0-9;:]*[a-zA-Z]"
        text_orig  <- gsub(ansi_regex, "", text, perl = TRUE)

        # Pre format headline start
        text_measure <- paste0(strrep(line_char, 3), " ", text_orig, " ")
        text         <- paste0(strrep(line_char, 3), " ", text, " ")

        # Get console width and calculate remaining characters
        fill_width <- max(0, max_width - nchar(text_measure))

        if (!is_no_print_active()){
            cat("\n", text, strrep(line_char, fill_width), "\n", sep = "")
            utils::flush.console()
        }
    }

    # Add message to global stack
    entry_nr <- length(.qol_messages[["stack"]]) + 1
    .qol_messages[["stack"]][[entry_nr]] <- list(type       = "HEADLINE",
                                                 text       = text_orig,
                                                 suppressed = suppressed,
                                                 new_line   = TRUE,
                                                 print_time = FALSE,
                                                 depth      = depth,
                                                 caller     = caller,
                                                 call_stack = sys.calls(),
                                                 time       = Sys.time())

    invisible(text_orig)
}


#' @param current_time The current time to create a time stamp.
#' @param caller_color Hex color code for the displayed caller function.
#' @param suppress FALSE by default. If TRUE triggers all the message
#' procedures to create a message stack but doesn't print the message to the
#' console.
#'
#' @examples
#' # Messages with time stamps
#' test_func <- function(){
#'     print_start_message()
#'     print_step("GREY", "Probably not so important")
#'     print_step("MAJOR", "This is a major step...")
#'     print_step("MINOR", "Sub step1")
#'     print_step("MINOR", "Sub step2")
#'     print_step("MINOR", "Sub step3")
#'     print_step("MAJOR", "[b]Finishing... [/b][#00FFFF with some color again!]")
#'     print_closing()
#' }
#'
#' test_func()
#'
#' @rdname messages
#'
#' @export
print_start_message <- function(current_time = Sys.time(),
                                caller_color = "#63C2C9",
                                always_print = FALSE,
                                suppress     = FALSE){
    depth      <- sys.nframe()
    suppressed <- FALSE
    no_print   <- FALSE

    # Identify which function called the message
    caller <- sys.call(-1)

    if (is.call(caller) && is.name(caller[[1]])){
        caller <- as.character(sys.call(-1)[[1]])
        caller <- ifelse(length(caller) == 0, "global", caller)
    }
    else{
        caller <- "do.call"
    }

    # Whenever a code block is executed, a unique token is generated. Based on this
    # token it can be decided, whether we are starting a new message stack (in case we
    # have a new token) or we keep the current stack (in case of multiple functions
    # running as a block)
    current_session_token   <- format(sys.frame(1))
    current_execution_token <- get_execution_token()

    # Don't print starting message, if messages are suppressed
    if (!always_print && is_message_suppressed()){
        # In case it is the first starting message in the current run or if the stack
        # has a not suppressed end not. Meaning a function or pipe operation has finished.
        if (!identical(current_session_token, .qol_messages[["last_session"]]) ||
            !identical(current_execution_token, .qol_messages[["last_execution"]]) ||
            message_stack_needs_reset()){

            reset_messages()
        }

        suppressed <- TRUE
        entry_nr   <- length(.qol_messages[["stack"]]) + 1
        text_orig  <- paste0(caller, " started")
    }
    else{
        # In case it is the first starting message in the current run or if the stack
        # has a not suppressed end not. Meaning a function or pipe operation has finished.
        if (!identical(current_session_token, .qol_messages[["last_session"]]) ||
            !identical(current_execution_token, .qol_messages[["last_execution"]]) ||
            message_stack_needs_reset()){

            reset_messages()
            .qol_messages[["last_session"]]   <- current_session_token
            .qol_messages[["last_execution"]] <- current_execution_token

            entry_nr <- 1
        }
        # In case it is another starting message within the same session
        else{
            entry_nr <- length(.qol_messages[["stack"]]) + 1

            suppressed <- TRUE
            no_print   <- TRUE
        }

        .qol_messages[["start_time"]] <- current_time
        start_timer()

        # Get current time in hh:mm:ss format
        current_time <- format(current_time, "%H:%M:%S")

        if (!is_no_print_active()){
            if (!no_print && !suppress){
                # Print actual headline
                text      <- paste0("[b][", caller_color, " ", caller, "][/b] started at ", current_time)
                text_orig <- print_headline(text = text, always_print = TRUE)
                cat("\n")
                utils::flush.console()
            }
            else{
                text_orig <- paste0("[b][", caller_color, " ", caller, "][/b] started at ", current_time)
            }
        }
        else{
            text_orig <- paste0("[b][", caller_color, " ", caller, "][/b] started at ", current_time)
        }
    }

    # Special case, because otherwise HEADLINE is the first entry
    .qol_messages[["stack"]][[entry_nr]] <- NULL
    .qol_messages[["stack"]][[entry_nr]] <- list(type       = "START",
                                                 text       = text_orig,
                                                 suppressed = suppressed,
                                                 new_line   = TRUE,
                                                 print_time = FALSE,
                                                 depth      = depth,
                                                 caller     = caller,
                                                 call_stack = sys.calls(),
                                                 time       = Sys.time())

    invisible(text_orig)
}


#' @param time_threshold The total time spent is displayed in different colors from
#' green over yellow to red, depending on the threshold specified (in seconds).
#' @param start_time The time at which the function call started to calculate the
#' time difference and output the total time spent.
#'
#' @rdname messages
#'
#' @export
print_closing <- function(time_threshold = 2,
                          start_time     = .qol_messages[["start_time"]],
                          caller_color   = "#63C2C9",
                          always_print   = FALSE,
                          suppress       = FALSE){
    depth      <- sys.nframe()
    suppressed <- FALSE

    # Identify which function called the message
    caller <- sys.call(-1)

    if (is.call(caller) && is.name(caller[[1]])){
        caller <- as.character(sys.call(-1)[[1]])
        caller <- ifelse(length(caller) == 0, "global", caller)
    }
    else{
        caller <- "do.call"
    }

    # Don't print starting message, if messages are suppressed
    if (!always_print && is_message_suppressed()){
        suppressed <- TRUE
        entry_nr   <- length(.qol_messages[["stack"]]) + 1
        text_orig  <- paste0(caller, " ended")
    }
    else{
        # Whenever a code block is executed, a unique token is generated. Based on this
        # token it can be decided, whether we are starting a new message stack (in case we
        # have a new token) or we keep the current stack (in case of multiple functions
        # running as a block)
        current_session_token   <- format(sys.frame(1))
        current_execution_token <- get_execution_token()

        if (is_stack() && (!identical(current_session_token, .qol_messages[["last_session"]]) ||
                           !identical(current_execution_token, .qol_messages[["last_execution"]]))){
            if (!is_start_fake()){
                suppressed <- TRUE
            }
            else{
                reset_messages()
            }
        }

        # As safety net set a starting point, in case no starting message was used,
        # to imitate a full message stack.
        if (!is_stack()){
            if (length(.qol_messages[["stack"]]) == 0){
                .qol_messages[["stack"]] <- list(list(type = "START", suppressed = TRUE, text = "FAKE"))
            }
            else{
                .qol_messages[["stack"]] <- c(list(list(type = "START", suppressed = TRUE, text = "FAKE")), .qol_messages[["stack"]])
            }

            .qol_messages[["last_session"]]   <- current_session_token
            .qol_messages[["last_execution"]] <- current_execution_token
        }

        # Put time stamp at the end of the last message if present
        print_time_stamp()

        stop_timer()

        if (is.null(start_time)){
            start_time <- Sys.time()
            print_message("NOTE", c("No starting time was set. If you didn't use print_starting_message(),",
                                    "you have to capture the starting time in your function manually and pass",
                                    "it to print_closing()."))
        }

        entry_nr <- length(.qol_messages[["stack"]]) + 1

        # Calculate time difference between provided starting time and current time
        fomatted_time <- transform_time(start_time)

        # Generating a seamless color transition from green over yellow to red
        time_in_seconds <- as.numeric(Sys.time() - start_time, units = "secs")
        ratio           <- min(time_in_seconds / time_threshold, 1)
        color_ramp      <- grDevices::colorRamp(c("#32CD32", "#FFC90E", "#C93F3F"))

        # Get the RGB value for the current ratio
        rgb_value <- color_ramp(min(ratio, 1))

        # Convert RGB matrix to a Hex string
        hex_color <- grDevices::rgb(rgb_value[1], rgb_value[2], rgb_value[3], maxColorValue = 255)

        # Print actual headline
        if (!is_no_print_active() && !suppress){
            text      <- paste0("[b][", caller_color, " ", caller, "][/b] execution time: [", hex_color, " ", fomatted_time, "]")
            text_orig <- print_headline(text = text, always_print = TRUE)
            cat("\n")
            utils::flush.console()
        }
        else{
            text_orig <- paste0("[b][", caller_color, " ", caller, "][/b] execution time: [", hex_color, " ", fomatted_time, "]")
        }
    }

    # Special case, because otherwise HEADLINE would be last before the END
    .qol_messages[["stack"]][[entry_nr]] <- NULL
    .qol_messages[["stack"]][[entry_nr]] <- list(type       = "END",
                                                 text       = text_orig,
                                                 suppressed = suppressed,
                                                 new_line   = TRUE,
                                                 print_time = FALSE,
                                                 depth      = depth,
                                                 caller     = caller,
                                                 call_stack = sys.calls(),
                                                 time       = Sys.time())

    # Give list entries a name if the stack is closed
    name_stack_messages()

    invisible(text_orig)
}


#' @noRd
transform_time <- function(start_time){
    # Calculate time difference between provided starting time and current time
    time_in_seconds <- as.numeric(Sys.time() - start_time, units = "secs")

    # Convert time format into different units depending on time difference
    if (time_in_seconds < 1){
        fomatted_time <- paste0(round(time_in_seconds * 1000), " ms")
    }
    else if (time_in_seconds < 60){
        fomatted_time <- paste0(format(time_in_seconds, digits = 3, decimal.mark = ","), " sec")
    }
    else if (time_in_seconds < 3600){
        fomatted_time <- paste0(format(time_in_seconds / 60, digits = 3, decimal.mark = ","), " min")
    }
    else{
        fomatted_time <- paste0(format(time_in_seconds / 3600, digits = 3, decimal.mark = ","), " hours")
    }

    fomatted_time
}


###############################################################################
# Major and minor steps
###############################################################################

#'
#' @param type The message type, so that the function knows which symbol and coloring
#' to use. Allowed are "note", "warning", "error", "neutral", "major", "minor" and "grey".
#'
#' @rdname messages
#'
#' @export
print_step <- function(type,
                       text,
                       ...,
                       always_print = FALSE,
                       utf8 = .qol_messages[["format"]][["utf8"]]){
    suppressed <- FALSE

    # Identify which function called the message
    caller <- sys.call(-1)

    if (is.call(caller) && is.name(caller[[1]])){
        # Get calling function and code line the message was called from
        caller <- ifelse(is.null(caller), "global", as.character(caller[[1]]))
    }
    else{
        caller <- "do.call"
    }

    if (!is.list(type) && !tolower(type) %in% c("major", "minor", "grey")){
        type <- "major"
    }
    else if (is.list(type)){
        store_type      <- type[["type"]]
        type[["type"]]  <- ""
        type[["timed"]] <- TRUE
    }

    # Check how deep nested this function is called.
    depth <- sys.nframe()

    # Whenever a code block is executed, a unique token is generated. Based on this
    # token it can be decided, whether we are starting a new message stack (in case we
    # have a new token) or we keep the current stack (in case of multiple functions
    # running as a block)
    current_session_token   <- format(sys.frame(1))
    current_execution_token <- get_execution_token()

    # In case it is the first starting message in the current run or if the stack
    # has a not suppressed end not. Meaning a function or pipe operation has finished.
    if (!identical(current_session_token, .qol_messages[["last_session"]]) ||
        !identical(current_execution_token, .qol_messages[["last_execution"]])){

        if (awaits_time_print() && !is_no_print_active()){
            cat("\n")
        }

        reset_messages()
    }
    else if (is.null(.qol_messages[["timer"]])){
        .qol_messages[["timer"]] <- Sys.time()
    }

    # Add message to global stack
    if (is_stack() && !is_full_stack()){
        entry_nr <- length(.qol_messages[["stack"]]) + 1
    }
    # If no starting point is set, the message is called inside a function where no
    # starting message was called. In this case only capture the last message and reset
    # the message stack. Otherwise the stack would just grow and never reset.
    else{
        if (length(.qol_messages[["stack"]]) > 1){
            .qol_messages[["stack"]]        <- list()
            .qol_messages[["last_message"]] <- NULL
        }

        entry_nr <- 1
    }

    if (!always_print && is_message_suppressed()){
        suppressed <- TRUE
    }
    else{
        # Put time stamp at the end of the last message if present
        print_time_stamp()

        # If messages are not suppressed
        text <- paste(text)
        text <- print_to_console(type = type, text = text, ..., utf8 = utf8, new_line = FALSE)
    }

    # Catch type for message stack on custom messages
    if (is.list(type)){
        type <- store_type
    }

    # Add message to global stack
    .qol_messages[["stack"]][[entry_nr]] <- list(type       = toupper(type),
                                                 text       = text,
                                                 suppressed = suppressed,
                                                 new_line   = FALSE,
                                                 print_time = TRUE,
                                                 depth      = depth,
                                                 caller     = caller,
                                                 call_stack = sys.calls(),
                                                 time       = Sys.time())

    invisible(text)
}

###############################################################################
# Custom message
###############################################################################

#' @param ansi_icon The icon used when message is displayed in utf8 mode.
#' @param text_icon The icon used when message is displayed in text only mode.
#' @param indent How many spaces to indent the message.
#' @param type If displayed as a normal note, then this is the text displayed
#' in front of the message. This also appears as type in the message stack.
#' @param color The color of the message type.
#' @param text_bold FALSE by default. If TRUE prints the message text in bold letters.
#' @param text_italic FALSE by default. If TRUE prints the message text in italic letters.
#' @param text_underline FALSE by default. If TRUE prints the message text underlined.
#' @param text_color The color of the actual message text.
#'
#' @returns
#' [set_up_custom_message()]: Returns a list.
#'
#' @rdname messages
#'
#' @export
set_up_custom_message <- function(ansi_icon      = "\U1F984",
                                  text_icon      = "^",
                                  indent         = 1,
                                  type           = "UNICORN",
                                  color          = "#FF00FF",
                                  text_bold      = FALSE,
                                  text_italic    = FALSE,
                                  text_underline = FALSE,
                                  text_color     = NULL){
    as.list(environment())
}

###############################################################################
# Helper functions
###############################################################################

#' Message Helper Functions
#'
#' @name message_helpers
#'
#' @description
#' [get_message_stack()]: Retrieves the current message stack as list or data frame.
#'
#' @param as_data_frame FALSE by default. If TRUE returns message stack information as
#' data frame.
#'
#' @seealso
#' Main printing functions: [print_message()], [print_headline()], [print_start_message()],
#' [print_closing()], [print_step()], [set_up_custom_message()]
#'
#' @returns
#' [get_message_stack()]: Returns a list of messages or a data frame.
#'
#' @rdname message_helpers
#'
#' @export
get_message_stack <- function(as_data_frame = FALSE){
    # Get stack as is
    if (!as_data_frame){
        .qol_messages[["stack"]]
    }
    # Convert to data frame without the whole call stack
    else{
        stack_df <- do.call(rbind, lapply(.qol_messages[["stack"]], function(message){
                message[["call_stack"]] <- NULL
                data.frame(message, stringsAsFactors = FALSE)
            }))

        rownames(stack_df) <- NULL

        stack_df
    }
}


#' @description
#' [set_no_print()]: FALSE by default. If set to TRUE the messages will be formatted
#' and returned but not printed to the console. Can e.g. be used in unit test
#' situations.
#'
#' @param value Can be TRUE or FALSE.
#'
#' @returns
#' [set_no_print()]: Returns the global no_print option.
#'
#' @rdname message_helpers
#'
#' @export
set_no_print <- function(value = FALSE){
    .qol_messages[["no_print"]] <- value

    invisible(.qol_messages[["no_print"]])
}


#' @description
#' [print_stack_as_messages()]: Prints the message stack as actual messages (only
#' not suppressed messages). Can be used to trigger expect_message, expect_warning
#' or expect_error in unit tests.
#'
#' @param type The message type to filter.
#'
#' @returns
#' [print_stack_as_messages()]: Returns NULL.
#'
#' @rdname message_helpers
#'
#' @export
print_stack_as_messages <- function(type = NULL){
    for (message in .qol_messages[["stack"]]){
        # Skip suppressed messages
        if (message[["suppressed"]]){
            next
        }

        # Skip start and end point
        if (message[["type"]] %in% c("START", "END")){
            next
        }

        # Skip if messages are filtered by type
        if (!is.null(type) && toupper(type) != message[["type"]]){
            next
        }

        # Print message according to its type
        if (message[["type"]] == "ERROR"){
            stop(message[["text"]])
        }
        else if (message[["type"]] == "WARNING"){
            warning(message[["text"]])
        }
        else{
            message(message[["text"]])
        }
    }

    invisible(NULL)
}


#' @description
#' [convert_square_brackets()]: Convert different curly bracket patterns into ansi
#' formatting or passes the context of vectors into the text.
#'
#' @param text The text in which to replace the curly brackets.
#' @param ... The actual replacement vectors.
#'
#' @details
#' The message types in which you can enter custom texts, are capable of using
#' different styling operators. These are:
#'
#' - Insert list of elements: &#91;vector_name&#93;
#' - Adding conditional words, if list of elements has more than one element: &#91;?word&#93;
#' - Adding conditional singular/plural, depending on list of element length: &#91;?singular/plural&#93;
#' - Bold, italic and underline: &#91;b&#93;some text&#91;/b&#93;, &#91;i&#93;some text&#91;/i&#93;, &#91;u&#93;some text&#91;/u&#93;
#' - Coloring parts of the message: &#91;#FF00FF some text&#93;
#'
#' @returns
#' [convert_square_brackets()]: Returns formatted text.
#'
#' @rdname message_helpers
#'
#' @export
convert_square_brackets <- function(text, ...){
    arguments <- list(...)
    env       <- parent.frame()

    number_of_elements <- 1

    # Capture all extra arguments and put them into the current environment
    # so this function can see them for the {} actions
    if (length(arguments) > 0){
        number_of_elements <- length(arguments[[1]])

        env <- list2env(arguments, envir = new.env(parent = env))
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Convert square brackets to curly brackets because the regex seems to work
    # better on them.
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    text <- gsub("\\[(/?[biu])\\]", "{\\1}",                text, perl = TRUE)
    text <- gsub("\\[(#[A-Fa-f0-9]{6}\\s+.*?)\\]", "{\\1}", text, perl = TRUE)
    text <- gsub("\\[(\\?.*?)\\]", "{\\1}",                 text, perl = TRUE)
    text <- gsub("\\[([a-zA-Z0-9_.]+)\\]", "{\\1}",         text, perl = TRUE)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Handle vector/variable insertion: [variable_name]
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    var_matches <- regmatches(text, gregexpr("(?<!\\?)\\{[a-zA-Z0-9_.]+\\}", text, perl = TRUE))[[1]]

    for (single_match in var_matches){
        # Remove curly brackets
        var_name <- gsub("[{}]", "", single_match)

        # Look for variable name in th current environment. If it exists replace it with
        # its actual contents.
        if (exists(var_name, envir = env)){
            # Get actual vector
            values <- get(var_name, envir = env)

            # Get replacement vector based on length
            replacement <- ifelse(length(values) > 1,
                                  paste(values, collapse = ", "),
                                  as.character(values))

            # Insert replacement in the provided text
            text <- gsub(single_match, replacement, text, fixed = TRUE)
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Insert conditional word, if length of first vector is greater than 1: [?s]
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    word_matches <- regmatches(text, gregexpr("\\{\\?[^/\\}]+\\}", text, perl = TRUE))[[1]]

    for (single_match in word_matches){
        # Get actual word
        word <- gsub("[{}?]", "", single_match)

        # Get replacement based on length
        replacement <- ifelse(number_of_elements != 1, word, "")

        # Insert replacement in the provided text
        text <- gsub(single_match, replacement, text, fixed = TRUE)
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Conditionally insert singular or plural words, if length of first vector is
    # greater than 1: [?is/are]
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    slash_matches <- regmatches(text, gregexpr("\\{\\?[^\\}]*/[^\\}]+\\}", text))[[1]]

    for (single_match in slash_matches){
        # Get actual vector
        parts <- unlist(strsplit(gsub("[{}?]", "", single_match), "/"))

        # Get replacement based on length
        replacement <- ifelse(number_of_elements != 1, parts[2], parts[1])

        # Insert replacement in the provided text
        text <- gsub(single_match, replacement, text, fixed = TRUE)
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Handle styling of bold [b], italic [i], underlined [u]
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    style_map <- list("\\{b\\}" = "\033[1m", "\\{/b\\}" = "\033[22m",
                      "\\{i\\}" = "\033[3m", "\\{/i\\}" = "\033[23m",
                      "\\{u\\}" = "\033[4m", "\\{/u\\}" = "\033[24m")

    # Just a quick find and replace loop
    for (tag in names(style_map)){
        text <- gsub(tag, style_map[[tag]], text)
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Handle dynamic coloring with hex codes: [#FF00FF Some colored text]
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    color_matches <- regmatches(text, gregexpr("\\{#([A-Fa-f0-9]{6})\\s+([^\\}]+)\\}", text, perl = TRUE))[[1]]

    for (single_match in color_matches){
        # Remove curly brackets and hashtag
        expression <- sub("^\\{#([A-Fa-f0-9]{6})\\s+", "\\1|", single_match)
        expression <- sub("\\}$", "", expression)

        # Separate hex code and the actual text
        parts <- unlist(strsplit(expression, "\\|"))

        hex_color     <- paste0("#", parts[1])
        text_to_color <- parts[2]

        # Convert color to ansi codes
        replacement <- hex_to_ansi(text_to_color, hex_color = hex_color)

        # Add a closing code after the text to ensure the text coming after the colored
        # one is printed in normal color again.
        replacement <- paste0(replacement, "\033[0m")

        text <- gsub(single_match, replacement, text, fixed = TRUE)
    }

    invisible(text)
}


###############################################################################
# Iternal functions
###############################################################################

#' @description
#' Resets all global message options.
#'
#' @returns
#' Returns global message options.
#'
#' @noRd
reset_messages <- function(){
    .qol_messages[["stack"]]        <- list()
    .qol_messages[["start_time"]]   <- NULL
    .qol_messages[["timer"]]        <- Sys.time()
    .qol_messages[["last_message"]] <- NULL

    invisible(.qol_messages)
}


#' @description
#' Checks whether there is a START node in the message stack.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_stack <- function(){
    length(.qol_messages[["stack"]]) > 0 && .qol_messages[["stack"]][[1]][["type"]] == "START"
}


#' @description
#' Checks whether there is a START and END node in the message stack.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_full_stack <- function(){
    is_stack() &&
        .qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["type"]] == "END" &&
        !.qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["suppressed"]] &&
        .qol_messages[["stack"]][[1]][["caller"]] == .qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["caller"]] &&
        .qol_messages[["stack"]][[1]][["depth"]] == .qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["depth"]]
}


#' @description
#' Checks whether the stack has only one message which
#' is not a START message.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_single_non_starting_message <- function(){
    length(.qol_messages[["stack"]]) == 1 && .qol_messages[["stack"]][[1]][["type"]] != "START"
}


#' @description
#' Checks whether the first message in the stack
#' is suppressed.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_starting_message_suppressed <- function(){
    length(.qol_messages[["stack"]]) >= 1 && .qol_messages[["stack"]][[1]][["suppressed"]]
}


#' @description
#' Checks whether the last message in the stack is an error.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_last_message_error <- function(){
    length(.qol_messages[["stack"]]) >= 1 && .qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["type"]] == "ERROR"
}


#' @description
#' Checks whether the starting message is a fake. A fake start
#' message is inserted, if a closing message was called without starting message.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_start_fake <- function(){
    length(.qol_messages[["stack"]]) >= 1 &&
        .qol_messages[["stack"]][[1]][["type"]] == "START" &&
        .qol_messages[["stack"]][[1]][["text"]] == "FAKE"
}


#' @description
#' Condition on when the message stack needs to be reset.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
message_stack_needs_reset <- function(){
    is_full_stack() || is_single_non_starting_message() || is_starting_message_suppressed() || is_last_message_error()
}


#' @description
#' Checks whether the parent function is surrounded by suppressMessages.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_message_suppressed <- function(){
    any(sapply(sys.calls(), function(calls){
        any(as.character(calls) %in% c("suppressMessages", "suppressPackageStartupMessages", "suppressWarnings"))
    }))
}


#' @description
#' Checks whether the last message set up a new line or if it
#' is necessary to manually print a new line before printing the actual message.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_new_line_necessary <- function(){
    !is.null(.qol_messages[["last_message"]]) && !.qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["new_line"]]
}


#' @description
#' Checks whether it is possible to print a time stamp
#' at the end of the last message.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_time_stamp_possible <- function(){
    !is.null(.qol_messages[["timer"]]) && !is.null(.qol_messages[["last_message"]])
}


#' @description
#' Checks whether the last message allows to put a time stamp at its end.
#'
#' @param message Message from a message in the stack.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_time_stamp_allowed <- function(message){
    if (is.null(message)){
        return(invisible(FALSE))
    }

    if (message[["print_time"]]){
        return(invisible(TRUE))
    }

    invisible(FALSE)
}


#' @description
#' Checks whether the global timer is running.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_timer_running <- function(){
    !is.null(.qol_messages[["timer"]])
}


#' @description
#' Checks whether the global option to suppress message
#' printing is active.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
is_no_print_active <- function(){
    .qol_messages[["no_print"]]
}


#' @description
#' Checks whether the last message in the stack awaits a time stamp.
#'
#' @returns
#' Returns TRUE or FALSE.
#'
#' @noRd
awaits_time_print <- function(){
    length(.qol_messages[["stack"]]) > 0 && .qol_messages[["stack"]][[length(.qol_messages[["stack"]])]][["print_time"]]
}


#' @description
#' Get a unique token for the current code block execution.
#'
#' @returns
#' Returns upper function call as string.
#'
#' @noRd
get_execution_token <- function(){
    # Get the whole call stack
    calls <- sys.calls()

    # Convert call stack to character vector
    call_strings <- vapply(calls, function(call){
        paste(deparse(call), collapse = "")
    },
    character(1))

    # Remove internal wrappers that could potentially surround the current function call
    ignore <- c("withCallingHandlers", "tryCatch", "doTryCatch", "eval", "source",
                "local", "test_package", "test_single_file", "tinytest", "testthat",
                "expect_", "rlang", "run_test_dir", "run_test_dir", "run_test_file",
                "lapply", "FUN")

    call_strings <- call_strings[!Reduce(`|`, lapply(ignore, grepl, x = call_strings))]

    # If there is nothing left, the function was called from the global environment
    if (length(call_strings) == 0)
        return("global")

    # Return upper function call
    call_strings[1]
}


#' @description
#' Gets all information from the last not suppressed message.
#'
#' @returns
#' Returns a list.
#'
#' @noRd
get_last_active_message <- function(){
    last_entry <- length(.qol_messages[["stack"]])

    if (last_entry == 0){
        return(invisible(NULL))
    }

    for (i in last_entry:1){
        entry <- .qol_messages[["stack"]][[i]]

        if (!entry[["suppressed"]]){
            return(invisible(entry))
        }
    }

    invisible(NULL)
}


#' @description
#' Stops the global timer.
#'
#' @returns
#' Returns NULL.
#'
#' @noRd
stop_timer <- function(){
    .qol_messages[["timer"]] <- NULL

    invisible(.qol_messages[["timer"]])
}


#' @description
#' Starts the global timer.
#'
#' @returns
#' Returns the current time.
#'
#' @noRd
start_timer <- function(){
    .qol_messages[["timer"]] <- Sys.time()

    invisible(.qol_messages[["timer"]])
}


#' @description
#' Names the single messages in the global message stack.
#'
#' @returns
#' Returns the global message stack.
#'
#' @noRd
name_stack_messages <- function(){
    names(.qol_messages[["stack"]]) <- vapply(.qol_messages[["stack"]], function(message){

        caller <- message[["caller"]]
        type   <- message[["type"]]
        text   <- substr(message[["text"]], 1, 20)

        paste(caller, type, text, sep = " | ")

    }, character(1))

    invisible(.qol_messages[["stack"]])
}


#' @description
#' Converts the hourglass symbol.
#'
#' @param text Text of the last message.
#' @param last_message The last message from the stack.
#'
#' @returns
#' Returns a converted text.
#'
#' @noRd
convert_hourglass <- function(text,
                              last_message,
                              utf8 = .qol_messages[["format"]][["utf8"]]){
    text <- .qol_messages[["last_message"]]

    if (utf8){
        if (last_message[["type"]] == "MAJOR"){
            text <- gsub("\u23f3\ufe0f", "\u2794", text)
        }
        else if (last_message[["type"]] == "MINOR"){
            text <- gsub("\u23f3\ufe0f", "\u271a", text)
        }
    }
    else{
        if (last_message[["type"]] == "MAJOR"){
            text <- gsub("?", ">", text)
        }
        else if (last_message[["type"]] == "MINOR"){
            text <- gsub("?", "+", text)
        }
    }

    text
}


#' @description
#' Converts the hourglass symbol.
#'
#' @returns
#' Returns a converted text.
#'
#' @noRd
# Put time stamp at the end of the last message if present
print_time_stamp <- function(utf8 = .qol_messages[["format"]][["utf8"]]){
    if (is_time_stamp_possible()){
        last_message <- get_last_active_message()

        if (is_time_stamp_allowed(last_message)){
            # Calculate time difference between provided starting time and current time
            fomatted_time <- transform_time(.qol_messages[["timer"]])
            start_timer()

            # Format message
            last_text <- convert_hourglass(.qol_messages[["last_message"]], last_message, utf8)

            time_stamp <- paste0(last_text, " [", .qol_messages[["format"]][["time_color"]], " (", fomatted_time, ")]")
            .qol_messages[["last_message"]] <- convert_square_brackets(time_stamp)

            # Print message
            if (!is_no_print_active()){
                cat("\r", .qol_messages[["last_message"]], "\n", sep = "")
                utils::flush.console()
            }
        }
        # Otherwise do nothing and just let the timer run without reset
    }
    # Start new timer
    else{
        # But only if the message before is something else than start
        if (!is_timer_running()){
            start_timer()
        }
    }

    invisible(NULL)
}
