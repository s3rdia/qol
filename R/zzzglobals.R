###############################################################################
# Globals
###############################################################################

# Variables entered here won't appear in Notes during devtools::check().
# The from - to variables can't be handled otherwise, because they
# are used in a data.table context which doesn't support the rlang .data
# keyword.
utils::globalVariables(c("qol_ID", "qol_from", "qol_to", "from", "to", "delta",
                         "across", ":=", "TYPE", "TYPE_NR", "DEPTH", ".subheader",
                         "var_sum", "var_cum_sum", "pct_group", "var_cum_pct",
                         "mean", "sd", "min", "max", "sum_wgt", ".temp_key", ".temp_weight",
                         "var_pct_row", ".pseudo_preserve", "BY", "by_vars", "VALUE",
                         "first_person", "age", "age_factor", "income", "income_factor",
                         "expenses", "expenses_factor", "file", "result", "TYPE_ORIG"))

###############################################################################
# Start up
###############################################################################

# Start up message
.onAttach <- function(libname, pkgname){
    current_version <- utils::packageVersion("qol")

    print_headline("[#63C2C9 [b]Quality Of Life][/b]")
    print_message("NEUTRAL", c("\n The [#32CD32 [b]qol-package[/b]] brings powerful concepts from [#63C2C9 [b]SAS[/b]] to [#C93F3F [b]R[/b]] to make life easier",
                               "and produce bigger and more complex outputs in less time with less code.",
                               "",
                               "> Use [#32CD32 ?qol] to get an overview.",
                               "> To view the changelog type: [#32CD32 qol_news()]",
                               "> Chat with the repository:   [#32CD32 qol_chat()]"))
    print_headline("[#63C2C9 [b]Current version:[/b]] [b][version][/b]", version = current_version)
}

###############################################################################
# General global options
###############################################################################

# Internal environment to store global options
.qol_options <- new.env(parent = emptyenv())
.qol_options[["excel_style"]] <- excel_output_style()
.qol_options[["var_labels"]]  <- list()
.qol_options[["stat_labels"]] <- list()
.qol_options[["print"]]       <- TRUE
.qol_options[["monitor"]]     <- FALSE
.qol_options[["na.rm"]]       <- FALSE
.qol_options[["print_miss"]]  <- FALSE
.qol_options[["output"]]      <- "console"
.qol_options[["threads"]]     <- suppressMessages(fst::threads_fst())

# Graphics globals
.qol_options[["graphic_themes"]]         <- create_global_themes()
.qol_options[["graphic_color_usage"]]    <- contrast_usage
.qol_options[["graphic_visuals"]]        <- graphic_visuals()
.qol_options[["graphic_axes"]]           <- graphic_axes()
.qol_options[["graphic_dimensions"]]     <- graphic_dimensions()
.qol_options[["graphic_fine_tuning"]]    <- graphic_fine_tuning()
.qol_options[["graphic_number_formats"]] <- number_format_style()
.qol_options[["graphic_output"]]         <- graphic_output()

###############################################################################
# Message system
###############################################################################

# Internal environment to store message styling options
.qol_messages <- new.env(parent = emptyenv())
.qol_messages[["format"]] <- list(utf8                = l10n_info()[["UTF-8"]] && .Platform$GUI != "Rgui",
                                  time_color          = "#6B6B6B",
                                  note_ansi           = hex_to_ansi(" \u2139\ufe0f NOTE: ", hex_color = "#63C2C9", bold = TRUE),
                                  note_pt             = hex_to_ansi(" ~ NOTE: ",            hex_color = "#63C2C9", bold = TRUE),
                                  note_indent_ansi    = hex_to_ansi(" \u2139\ufe0f\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0", hex_color = "#63C2C9", bold = TRUE),
                                  note_indent_pt      = hex_to_ansi(" ~\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0",            hex_color = "#63C2C9", bold = TRUE),
                                  warning_ansi        = hex_to_ansi(" \u26a0\ufe0f WARNING: ", hex_color = "#FFC90E", bold = TRUE),
                                  warning_pt          = hex_to_ansi(" ! WARNING: ",            hex_color = "#FFC90E", bold = TRUE),
                                  warning_indent_ansi = hex_to_ansi(" \u26a0\ufe0f\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0", hex_color = "#FFC90E", bold = TRUE),
                                  warning_indent_pt   = hex_to_ansi(" !\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0",            hex_color = "#FFC90E", bold = TRUE),
                                  error_ansi          = hex_to_ansi(" \u274c ERROR: ", hex_color = "#C93F3F", bold = TRUE),
                                  error_pt            = hex_to_ansi(" X ERROR: ",      hex_color = "#C93F3F", bold = TRUE),
                                  error_indent_ansi   = hex_to_ansi(" \u274c\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0", hex_color = "#C93F3F", bold = TRUE),
                                  error_indent_pt     = hex_to_ansi(" X\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0\u00a0",      hex_color = "#C93F3F", bold = TRUE),
                                  neutral_ansi        = hex_to_ansi("\u00a0", hex_color = "#63C2C9", bold = TRUE),
                                  neutral_pt          = hex_to_ansi("\u00a0", hex_color = "#63C2C9", bold = TRUE),
                                  neutral_indent_ansi = hex_to_ansi("\u00a0", hex_color = "#63C2C9", bold = TRUE),
                                  neutral_indent_pt   = hex_to_ansi("\u00a0", hex_color = "#63C2C9", bold = TRUE),
                                  grey_ansi           = hex_to_ansi(" \u2601 ",                 hex_color = "#6B6B6B", bold = TRUE),
                                  grey_pt             = hex_to_ansi(" * ",                      hex_color = "#6B6B6B", bold = TRUE),
                                  grey_indent_ansi    = hex_to_ansi("\u00a0\u00a0\u00a0\u00a0", hex_color = "#6B6B6B", bold = TRUE),
                                  grey_indent_pt      = hex_to_ansi("\u00a0\u00a0\u00a0",       hex_color = "#6B6B6B", bold = TRUE),
                                  major_ansi          = hex_to_ansi(" \u23f3\ufe0f ",           hex_color = "#32CD32", bold = TRUE),
                                  major_pt            = hex_to_ansi(" > ",                      hex_color = "#32CD32", bold = TRUE),
                                  major_indent_ansi   = hex_to_ansi("\u00a0\u00a0\u00a0\u00a0", hex_color = "#32CD32", bold = TRUE),
                                  major_indent_pt     = hex_to_ansi("\u00a0\u00a0\u00a0",       hex_color = "#32CD32", bold = TRUE),
                                  minor_ansi          = hex_to_ansi(" \u00a0\u00a0\u00a0\u23f3\ufe0f ", hex_color = "#32CD32", bold = TRUE),
                                  minor_pt            = hex_to_ansi(" \u00a0\u00a0+ ",                  hex_color = "#32CD32", bold = TRUE),
                                  minor_indent_ansi   = hex_to_ansi("\u00a0\u00a0\u00a0\u00a0\u00a0",   hex_color = "#32CD32", bold = TRUE),
                                  minor_indent_pt     = hex_to_ansi("\u00a0\u00a0\u00a0",               hex_color = "#32CD32", bold = TRUE))
.qol_messages[["stack"]]           <- list()
.qol_messages[["start_time"]]      <- NULL
.qol_messages[["timer"]]           <- NULL
.qol_messages[["last_message"]]    <- NULL
.qol_messages[["last_session"]]    <- NULL
.qol_messages[["last_execution"]]  <- NULL
.qol_messages[["no_print"]]        <- FALSE
