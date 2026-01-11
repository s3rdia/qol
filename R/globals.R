# Variables entered here won't appear in Notes during devtools::check().
# The from - to variables can't be handled otherwise, because they
# are used in a data.table context which doesn't support the rlang .data
# keyword.
utils::globalVariables(c("qol_ID", "qol_from", "qol_to", "from", "to", "delta",
                         "across", ":=", "TYPE", "TYPE_NR", "DEPTH",
                         "var_sum", "var_cum_sum", "pct_group", "var_cum_pct",
                         "mean", "sd", "min", "max", "sum_wgt", ".temp_key", ".temp_weight",
                         "var_pct_row", ".pseudo_preserve", "BY", "VALUE"))


# Start up message
.onAttach <- function(libname, pkgname) {
    current_version <- utils::packageVersion(pkgname)

    packageStartupMessage("    The qol-package brings powerful concepts from 'SAS' to 'R' to make life easier\n",
                          "    and produce bigger and more complex outputs in less time with less code.\n",
                          "\n",
                          "    Current version: ", current_version, "\n",
                          "    -> Use ?qol to get an overview.\n",
                          "    -> To view the changelog type: 'qol_news()'")
}


# Internal environment to store global options
.qol_options <- new.env(parent = emptyenv())
.qol_options[["excel_style"]] <- excel_output_style()
.qol_options[["var_labels"]]  <- list()
.qol_options[["stat_labels"]] <- list()
.qol_options[["print"]]       <- TRUE
.qol_options[["monitor"]]     <- FALSE
.qol_options[["na.rm"]]       <- FALSE
.qol_options[["output"]]      <- "console"
