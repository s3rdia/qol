#' qol - Quality of Life
#'
#' @author Tim Siebenmorgen
#'
#' @description
#' This package brings some quality of life concepts and functions inspired
#' by 'SAS' to 'R'. The main goal is to make descriptive evaluations easier, so
#' one can create bigger and more complex outputs in less time with less code.
#' Introducing format containers with multilabels, a more powerful summarise, which
#' is capable to output every possible combination of the provided grouping variables
#' in one go, tabulation functions which can create any table in different styles
#' and other more readable functions.
#'
#' In addition it offers an error handling which often catches errors and just
#' let's your code flow, even if there are small errors. You always get an
#' understandable message which helps you to get rid of the problem.
#'
#' The package builds on the incredibly fast data.table and collapse packages for
#' maximum speed and on the wonderful openxlsx2 package for maximum style.
#'
#' @section Imports:
#' data.table, collapse, openxlsx2
#'
#' @section Minimal R Version:
#' 4.1.0 or higher
#'
#' @section Functions:
#' Creating formats: [discrete_format()], [interval_format()].
#'
#' Summarisation and tabulation: [summarise_plus()], [frequencies()], [crosstabs()], [any_table()], [export_with_style()], [combine_into_workbook()].
#'
#' Creating a custom table style: [excel_output_style()], [modify_output_style()],
#' [number_format_style()], [modify_number_formats()].
#'
#' Recoding: [recode()], [recode_multi()].
#'
#' Selecting: [keep()], [dropp()].
#'
#' Joining: [multi_join()]
#'
#' If-statement: [if.()], [else_if.()], [else.()].
#'
#' Monitoring: [monitor_start()], [monitor_end()], [monitor_next()], [monitor_plot()].
#'
#' Renaming: [rename_pattern()], [add_extension()], [remove_stat_extension()].
#'
#' Retaining: [running_number()], [mark_case()], [retain_value()], [retain_sum()]
#'
#' Generate dummy data: [dummy_data()]
#'
#' Small helpers: [libname()], [inverse()], [setcolorder_by_pattern()],
#' [drop_type_vars()], [fuse_variables()], [get_excel_range()], [replace_except()].
#'
#' Split data frame: [split_by_var()], [split_by_condition()].
#'
#' Other: [build_master()], [build_rstheme()], [args_to_char()], [convert_numeric()], [is_numeric()]
#'
#' @section Snippets:
#' \preformatted{
#' snippet splus
#'     summarise_plus(class      = c(var1, var2, ...),
#'                    values     = c(var1, var2, ...),
#'                    statistics = c("pct_group", "sum", "sum_wgt", "freq"),
#'                    formats    = list(var = format., ...),
#'                    weight     = weight_var,
#'                    nesting    = "deepest")
#'
#' snippet if.
#'          if.(condition, var = value) |>
#'     else_if.(condition, var = value) |>
#'     else.   (           var = value)
#'
#' snippet freq
#'     frequencies(variables = c(var1, var2, ...),
#'                 formats   = list(var = "format.", ...),
#'                 titles    = c(),
#'                 footnotes = c(),
#'                 weight    = weight_var)
#'
#' snippet cross
#'     crosstabs(rows       = row_var,
#'               columns    = col_var,
#'               statistics = c("sum", "pct_row", "pct_column", "pct_total", "freq"),
#'               formats   = list(var = format., ...),
#'               titles    = c(),
#'               footnotes = c(),
#'               weight    = weight_var)
#'
#' snippet any
#'     any_table(rows        = c("var1 + var2 + ...", "var1"),
#'               columns     = c("var3", "var3 + var4 + ..."),
#'               values      = c("value_var1", "value_var2"),
#'               statistics  = c("sum", "pct_group", "pct_value", "freq"),
#'               pct_group   = c("var1", "var2"),
#'               pct_value   = list(new_var = "numerator / denominator"),
#'               formats     = list(var = format., ...),
#'               titles      = c(),
#'               footnotes   = c(),
#'               var_labels  = list("var1" = "My label", ...),
#'               stat_labels = list("pct" = "%"),
#'               box         = "",
#'               weight      = weight_var)
#' }
#'
#' @keywords internal
"_PACKAGE"
