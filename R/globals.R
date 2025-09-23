# Variables entered here won't appear in Notes during devtools::check().
# The from - to variables can't be handled otherwise, because they
# are used in a data.table context which doesn't support the rlang .data
# keyword.
utils::globalVariables(c("qol_ID", "qol_from", "qol_to", "from", "to", "delta",
                         "across", ":=", "TYPE", "DEPTH",
                         "var_sum", "var_cum_sum", "pct_group", "var_cum_pct",
                         "mean", "sd", "min", "max",
                         "var_pct_row"))


# Start up message
.onAttach <- function(libname, pkgname) {
    packageStartupMessage("    The qol-package brings powerful concepts from 'SAS' to 'R' to make life easier\n",
                          "    and produce bigger and more complex outputs in less time with less code.")
}
