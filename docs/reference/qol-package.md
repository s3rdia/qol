# qol - Quality of Life

This package brings some quality of life concepts and functions inspired
by 'SAS' to 'R'. The main goal is to make descriptive evaluations
easier, so one can create bigger and more complex outputs in less time
with less code. Introducing format containers with multilabels, a more
powerful summarise, which is capable to output every possible
combination of the provided grouping variables in one go, tabulation
functions which can create any table in different styles and other more
readable functions.

In addition it offers an error handling which often catches errors and
just let's your code flow, even if there are small errors. You always
get an understandable message which helps you to get rid of the problem.

The package builds on the incredibly fast data.table and collapse
packages for maximum speed and on the wonderful openxlsx2 package for
maximum style.

## Imports

data.table, collapse, openxlsx2

## Minimal R Version

4.1.0 or higher

## Functions

Creating formats:
[`discrete_format()`](https://s3rdia.github.io/qol/reference/formats.md),
[`interval_format()`](https://s3rdia.github.io/qol/reference/formats.md),
[`expand_formats()`](https://s3rdia.github.io/qol/reference/expand_formats.md)

Summarisation and tabulation:
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md),
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md),
[`combine_into_workbook()`](https://s3rdia.github.io/qol/reference/combine_into_workbook.md)

Creating a custom table style:
[`excel_output_style()`](https://s3rdia.github.io/qol/reference/excel_output_style.md),
[`modify_output_style()`](https://s3rdia.github.io/qol/reference/modify_output_style.md),
[`number_format_style()`](https://s3rdia.github.io/qol/reference/number_format_style.md),
[`modify_number_formats()`](https://s3rdia.github.io/qol/reference/modify_number_formats.md)

Recoding:
[`recode()`](https://s3rdia.github.io/qol/reference/recode.md),
[`recode_multi()`](https://s3rdia.github.io/qol/reference/recode.md)

Selecting:
[`keep()`](https://s3rdia.github.io/qol/reference/keep_dropp.md),
[`dropp()`](https://s3rdia.github.io/qol/reference/keep_dropp.md),
[`inverse()`](https://s3rdia.github.io/qol/reference/inverse.md),
[`vars_between()`](https://s3rdia.github.io/qol/reference/vars_between.md)

Joining:
[`multi_join()`](https://s3rdia.github.io/qol/reference/multi_join.md)

Transposing:
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md)

Sorting:
[`sort_plus()`](https://s3rdia.github.io/qol/reference/sort_plus.md)

If-statement:
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md)

Monitoring:
[`monitor_start()`](https://s3rdia.github.io/qol/reference/monitor.md),
[`monitor_end()`](https://s3rdia.github.io/qol/reference/monitor.md),
[`monitor_next()`](https://s3rdia.github.io/qol/reference/monitor.md),
[`monitor_plot()`](https://s3rdia.github.io/qol/reference/monitor.md)

Renaming:
[`rename_pattern()`](https://s3rdia.github.io/qol/reference/rename_pattern.md),
[`add_extension()`](https://s3rdia.github.io/qol/reference/add_extension.md),
[`remove_stat_extension()`](https://s3rdia.github.io/qol/reference/remove_stat_extension.md),
[`replace_except()`](https://s3rdia.github.io/qol/reference/replace_except.md)

Retaining:
[`running_number()`](https://s3rdia.github.io/qol/reference/retain.md),
[`mark_case()`](https://s3rdia.github.io/qol/reference/retain.md),
[`retain_value()`](https://s3rdia.github.io/qol/reference/retain.md),
[`retain_sum()`](https://s3rdia.github.io/qol/reference/retain.md),
[`retain_variables()`](https://s3rdia.github.io/qol/reference/retain.md)

Character Manipulation:
[`concat()`](https://s3rdia.github.io/qol/reference/concat.md),
[`sub_string()`](https://s3rdia.github.io/qol/reference/sub_string.md),
[`remove_blanks()`](https://s3rdia.github.io/qol/reference/remove_blanks.md)

Generate dummy data:
[`dummy_data()`](https://s3rdia.github.io/qol/reference/dummy_data.md)

Conversion:
[`args_to_char()`](https://s3rdia.github.io/qol/reference/convert_arguments.md),
[`dots_to_char()`](https://s3rdia.github.io/qol/reference/convert_arguments.md),
[`get_origin_as_char()`](https://s3rdia.github.io/qol/reference/convert_arguments.md)
[`convert_numeric()`](https://s3rdia.github.io/qol/reference/convert_variables.md),
[`convert_factor()`](https://s3rdia.github.io/qol/reference/convert_variables.md)

Loading:
[`libname()`](https://s3rdia.github.io/qol/reference/libname.md),
[`set()`](https://s3rdia.github.io/qol/reference/set.md)

Reporting:
[`content_report()`](https://s3rdia.github.io/qol/reference/content_report.md)

Small helpers:
[`setcolorder_by_pattern()`](https://s3rdia.github.io/qol/reference/setcolorder_by_pattern.md),
[`drop_type_vars()`](https://s3rdia.github.io/qol/reference/drop_type_vars.md),
[`fuse_variables()`](https://s3rdia.github.io/qol/reference/fuse_variables.md),
[`get_excel_range()`](https://s3rdia.github.io/qol/reference/get_excel_range.md),
[`get_integer_length()`](https://s3rdia.github.io/qol/reference/get_integer_length.md)

Split data frame:
[split_by](https://s3rdia.github.io/qol/reference/split_by.md)

Error handling:
[`resolve_intersection()`](https://s3rdia.github.io/qol/reference/error_handling.md),
[`part_of_df()`](https://s3rdia.github.io/qol/reference/error_handling.md),
[`remove_doubled_values()`](https://s3rdia.github.io/qol/reference/error_handling.md),
[`check_weight()`](https://s3rdia.github.io/qol/reference/error_handling.md)

Global options:
[`set_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`reset_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`get_style_options()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`close_file()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_variable_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`get_variable_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_stat_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`get_stat_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_print()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_print()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_monitor()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_monitor()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_na.rm()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_na.rm()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_output()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_output()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_titles()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_titles()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_footnotes()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_footnotes()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`set_print_miss()`](https://s3rdia.github.io/qol/reference/qol_options.md),
[`get_print_miss()`](https://s3rdia.github.io/qol/reference/qol_options.md)

Other:
[`build_master()`](https://s3rdia.github.io/qol/reference/build_master.md),
[`build_rstheme()`](https://s3rdia.github.io/qol/reference/build_rstheme.md)

## Snippets

    snippet splus
        summarise_plus(class      = c(var1, var2, ...),
                       values     = c(var1, var2, ...),
                       statistics = c("pct_group", "sum", "sum_wgt", "freq"),
                       weight     = weight_var,
                       formats    = list(var = format., ...),
                       nesting    = "deepest")

    snippet if.
             if.(condition, var = value) |>
        else_if.(condition, var = value) |>
        else.   (           var = value)

    snippet freq
        frequencies(variables = c(var1, var2, ...),
                    weight    = weight_var,
                    formats   = list(var = "format.", ...),
                    titles    = c(),
                    footnotes = c())

    snippet cross
        crosstabs(rows       = row_var,
                  columns    = col_var,
                  statistics = c("sum", "pct_row", "pct_column", "pct_total", "freq"),
                  weight    = weight_var,
                  formats   = list(var = format., ...),
                  titles    = c(),
                  footnotes = c())

    snippet any
        any_table(rows        = c("var1 + var2 + ...", "var1"),
                  columns     = c("var3", "var3 + var4 + ..."),
                  values      = c("value_var1", "value_var2"),
                  statistics  = c("sum", "pct_group", "pct_value", "freq"),
                  pct_group   = c("var1", "var2"),
                  pct_value   = list(new_var = "numerator / denominator"),
                  weight    = weight_var,
                  formats     = list(var = format., ...),
                  titles      = c(),
                  footnotes   = c(),
                  var_labels  = list("var1" = "My label", ...),
                  stat_labels = list("pct" = "
                  box         = "")

## See also

Useful links:

- <https://github.com/s3rdia/qol>

- <https://s3rdia.github.io/qol/>

## Author

Tim Siebenmorgen
