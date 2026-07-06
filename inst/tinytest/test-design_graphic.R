set_no_print(TRUE)
set_graphic_options(font = "sans")

dummy_df <- dummy_data(100)

age. <- discrete_format(
    "under 18"       = 0:17,
    "18 to under 65" = 18:64,
    "65 and older"   = 65:100)

sex. <- discrete_format(
    "Male"   = 1,
    "Female" = 2)


# Simplest form of design_graphic
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   diagram        = dg_vbars,
                   print          = FALSE)

expect_inherits(result_list, "qol_graphic",        info = "Simplest form of design_graphic")
expect_equal(length(result_list), 3,               info = "Simplest form of design_graphic")
expect_true(all(c("table", "graphic", "meta") %in% names(result_list)), info = "Simplest form of design_graphic")
expect_inherits(result_list[["graphic"]], "gTree", info = "Simplest form of design_graphic")
expect_true(".temp_values_sum" %in% names(result_list[["table"]]), info = "Simplest form of design_graphic")
expect_equal(collapse::funique(result_list[["table"]][["segments"]]), "1", info = "Simplest form of design_graphic")


# design_graphic runs without anything and prints vbars
result_list <- dummy_df |> design_graphic(print = FALSE)

expect_inherits(result_list, "qol_graphic", info = "design_graphic runs without anything and prints vbars")


# design_graphic with segments
result_list <- dummy_df |>
    design_graphic(axes_variables = "sex",
                   segments       = "education",
                   diagram        = dg_vbars,
                   print          = FALSE)

expect_true(all(c("low", "middle", "high") %in% collapse::funique(result_list[["table"]][["segments"]])), info = "design_graphic with segments")


# design_graphic with multiple segments
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = c("education", "sex"),
                   diagram        = dg_vbars,
                   print          = FALSE)

expect_true(all(c("low", "middle", "high", "1", "2") %in% collapse::funique(result_list[["table"]][["segments"]])), info = "design_graphic with segments")


# design_graphic with formats
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_true(all(c("Male", "Female") %in% collapse::funique(result_list[["table"]][["segments"]])), info = "design_graphic with formats")
expect_true(all(c("under 18", "18 to under 65", "65 and older") %in% collapse::funique(result_list[["table"]][["axes"]])), info = "design_graphic with formats")


# design_graphic with multiple axes variables
result_list <- dummy_df |>
    design_graphic(axes_variables = c("age", "education"),
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_true("axes" %in% names(result_list[["table"]]), info = "design_graphic with multiple axes variables")
expect_true(all(c("under 18", "18 to under 65", "65 and older", "low", "middle", "high") %in% collapse::funique(result_list[["table"]][["axes"]])),
            info = "design_graphic with multiple axes variables")


# design_graphic with multiple nested axes variables
result_list <- dummy_df |>
    design_graphic(axes_variables = "age + education",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_true(all(c("age", "education") %in% names(result_list[["table"]])), info = "design_graphic with multiple nested axes variables")


# design_graphic mixed axes variables (nested and unnested) results in unnested behaviour
result_list <- dummy_df |>
    design_graphic(axes_variables = c("age + education", "first_person"),
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_true("axes" %in% names(result_list[["table"]]), info = "design_graphic mixed axes variables (nested and unnested) results in unnested behaviour")
expect_true(all(c("under 18", "18 to under 65", "65 and older", "low", "middle", "high", "1", "0") %in% collapse::funique(result_list[["table"]][["axes"]])),
            info = "design_graphic mixed axes variables (nested and unnested) results in unnested behaviour")


# design_graphic with multiple nested axes variables and multiple segments
result_list <- dummy_df |>
    design_graphic(axes_variables = "age + education",
                   segments       = c("sex", "first_person"),
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_true(all(c("age", "education") %in% names(result_list[["table"]])), info = "design_graphic with multiple nested axes variables and multiple segments")
expect_true(all(c("Male", "Female", "0", "1") %in% collapse::funique(result_list[["table"]][["segments"]])),
            info = "design_graphic with multiple nested axes variables and multiple segments")


# design_graphic with by variables
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   by             = "education",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_true("by_vars" %in% names(result_list[["table"]]), info = "design_graphic with by variables")
expect_true(all(c("low", "middle", "high") %in% collapse::funique(result_list[["table"]][["by_vars"]])),
            info = "design_graphic with by variables")


# design_graphic with legend
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font       = "sans",
                                                    label_type = "legend"),
                   print          = FALSE)

expect_true(any(startsWith(names(result_list[["graphic"]][["children"]]), "legend")), info = "design_graphic with legend")


# design_graphic with legend presets
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font         = "sans",
                                                    label_type   = "legend",
                                                    legend_x_pos = "left"),
                   print          = FALSE)

expect_true(any(startsWith(names(result_list[["graphic"]][["children"]]), "legend")), info = "design_graphic with legend presets")

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font         = "sans",
                                                    label_type   = "legend",
                                                    legend_x_pos = "right"),
                   print          = FALSE)

expect_true(any(startsWith(names(result_list[["graphic"]][["children"]]), "legend")), info = "design_graphic with legend presets")

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font         = "sans",
                                                    label_type   = "legend",
                                                    legend_y_pos = "top"),
                   print          = FALSE)

expect_true(any(startsWith(names(result_list[["graphic"]][["children"]]), "legend")), info = "design_graphic with legend presets")

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font         = "sans",
                                                    label_type   = "legend",
                                                    legend_y_pos = "bottom"),
                   print          = FALSE)

expect_true(any(startsWith(names(result_list[["graphic"]][["children"]]), "legend")), info = "design_graphic with legend presets")


# design_graphic with reverse colors
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font           = "sans",
                                                    reverse_colors = TRUE),
                   print          = FALSE)

expect_equal(result_list[["meta"]][["colors_to_use"]], c("#3E6F8E", "#12324A", "#3E6F8E", "#12324A", "#3E6F8E", "#12324A"),
             info = "design_graphic with reverse colors")


# design_graphic without value display
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font           = "sans",
                                                    display_values = FALSE),
                   print          = FALSE)

expect_true(!any(startsWith(names(result_list[["graphic"]][["children"]][["diagram"]][["children"]]),
                            "segment_value")), info = "design_graphic without value display")


# design_graphic with bar values outside
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font              = "sans",
                                                    bar_values_inside = FALSE),
                   print          = FALSE)

expect_inherits(result_list, "qol_graphic", info = "design_graphic with bar values outside")


# design_graphic with rotated values
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font          = "sans",
                                                    rotate_values = TRUE),
                   print          = FALSE)

expect_true(length(result_list[["meta"]][["value_heights"]]) > 0, info = "design_graphic with rotated values")


# design_graphic with rotated values outside
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font              = "sans",
                                                    rotate_values     = TRUE,
                                                    bar_values_inside = FALSE),
                   print          = FALSE)

expect_true(!"value_heights" %in% names(result_list[["meta"]]), info = "design_graphic with rotated values outside")


# design_graphic with negative values
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "expenses",
                   statistics     = "sum",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_equal(result_list[["meta"]][["zero_pos"]], 1, info = "design_graphic with negative values")


# design_graphic with positive and negative values and display plus symbol
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "balance",
                   statistics     = "sum",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font                = "sans",
                                                    display_plus_symbol = TRUE))

expect_true(result_list[["meta"]][["zero_pos"]] > 0 && result_list[["meta"]][["zero_pos"]] < 1,
            info = "design_graphic with positive and negative values and display plus symbol")


# design_graphic with x guiding lines
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font            = "sans",
                                                    guiding_lines_x = TRUE),
                   print          = FALSE)

expect_true(any(startsWith(names(result_list[["graphic"]][["children"]][["diagram"]][["children"]][["xy_guiding_lines"]][["children"]]), "x_")),
            info = "design_graphic with guiding lines")
expect_true(!any(startsWith(names(result_list[["graphic"]][["children"]][["diagram"]][["children"]][["xy_guiding_lines"]][["children"]]), "y_")),
            info = "design_graphic with guiding lines")

# design_graphic with y guiding lines
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font            = "sans",
                                                    guiding_lines_y = TRUE),
                   print          = FALSE)

expect_true(!any(startsWith(names(result_list[["graphic"]][["children"]][["diagram"]][["children"]][["xy_guiding_lines"]][["children"]]), "x_")),
            info = "design_graphic with guiding lines")
expect_true(any(startsWith(names(result_list[["graphic"]][["children"]][["diagram"]][["children"]][["xy_guiding_lines"]][["children"]]), "y_")),
            info = "design_graphic with guiding lines")


# design_graphic outputs static graphic
temp_file <- tempfile(fileext = ".png")
on.exit(unlink(temp_file), add = TRUE)

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   output         = graphic_output(save_path = dirname(temp_file),
                                                   file      = basename(temp_file)))

expect_true(file.exists(temp_file), info = "design_graphic outputs static graphic")


# design_graphic outputs multiple static graphics with by
temp_file <- tempfile(fileext = ".png")
on.exit(unlink(temp_file), add = TRUE)

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   by             = "education",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   output         = graphic_output(save_path = dirname(temp_file),
                                                   file      = basename(temp_file)),
                   na.rm          = TRUE)

filename <- tools::file_path_sans_ext(basename(temp_file))

expect_true(file.exists(paste0(dirname(temp_file), "/", filename, "_low.png")),    info = "design_graphic outputs multiple static graphics with by")
expect_true(file.exists(paste0(dirname(temp_file), "/", filename, "_middle.png")), info = "design_graphic outputs multiple static graphics with by")
expect_true(file.exists(paste0(dirname(temp_file), "/", filename, "_high.png")),   info = "design_graphic outputs multiple static graphics with by")


# design_graphic outputs static graphic as grid
temp_file <- tempfile(fileext = ".png")
on.exit(unlink(temp_file), add = TRUE)

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   by             = "education",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   output         = graphic_output(save_path  = dirname(temp_file),
                                                   file       = basename(temp_file),
                                                   by_as_grid = TRUE),
                   na.rm          = TRUE)

expect_true(file.exists(temp_file), info = "design_graphic outputs static graphic as grid")


# design_graphic outputs interactive html graphic
temp_file <- tempfile(fileext = ".html")
on.exit(unlink(temp_file), add = TRUE)

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   output         = graphic_output(save_path   = dirname(temp_file),
                                                   file        = basename(temp_file),
                                                   interactive = TRUE))

expect_true(file.exists(temp_file), info = "design_graphic outputs interactive html graphic")


# design_graphic outputs multiple interactive html graphics with by
temp_file <- tempfile(fileext = ".html")
on.exit(unlink(temp_file), add = TRUE)

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   by             = "education",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   output         = graphic_output(save_path   = dirname(temp_file),
                                                   file        = basename(temp_file),
                                                   interactive = TRUE),
                   na.rm          = TRUE)

filename <- tools::file_path_sans_ext(basename(temp_file))

expect_true(file.exists(paste0(dirname(temp_file), "/", filename, "_low.html")),    info = "design_graphic outputs interactive html graphics with by")
expect_true(file.exists(paste0(dirname(temp_file), "/", filename, "_middle.html")), info = "design_graphic outputs interactive html graphics with by")
expect_true(file.exists(paste0(dirname(temp_file), "/", filename, "_high.html")),   info = "design_graphic outputs interactive html graphics with by")


# design_graphic outputs interactive html graphic with dropdown menu
temp_file <- tempfile(fileext = ".html")
on.exit(unlink(temp_file), add = TRUE)

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   by             = "education",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   output         = graphic_output(save_path   = dirname(temp_file),
                                                   file        = basename(temp_file),
                                                   interactive = TRUE,
                                                   by_as_grid  = TRUE),
                   na.rm          = TRUE)

expect_true(file.exists(temp_file), info = "design_graphic outputs interactive html graphic with dropdown menu")

###############################################################################
# Warning checks
###############################################################################

# design_graphic with invalid diagram type throws a warning and prints vbars
result_list <- dummy_df |> design_graphic(diagram = "test",
                                          print   = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "'test' is not a function. Vertical bars will be used instead.",
             info = "design_graphic with invalid diagram type throws warning and prints vbars")


# design_graphic throws a warning with invalid segment variable
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "test",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <segments> variable 'test' is not part of",
               info = "design_graphic throws a warning with invalid segment variable")


# design_graphic throws a warning with invalid axes variable
result_list <- dummy_df |>
    design_graphic(axes_variables = "test",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <axes_vars> variable 'test' is not part of",
               info = "design_graphic throws a warning with invalid axes variable")


# design_graphic throws a warning with invalid by variable
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   by             = "test",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "The provided <by> variable 'test' is not part of",
               info = "design_graphic throws a warning with invalid by variable")


# design_graphic throws a warning on wrong legend presets
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font         = "sans",
                                                    label_type   = "legend",
                                                    legend_x_pos = "center"),
                   print          = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Horizontally only 'left' and 'right' preset available. 'left' will be used.",
               info = "design_graphic throws a warning on wrong legend presets")

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   visuals        = graphic_visuals(font         = "sans",
                                                    label_type   = "legend",
                                                    legend_y_pos = "center"),
                   print          = FALSE)

expect_warning(print_stack_as_messages("WARNING"), "Vertically only 'top' and 'bottom' preset available. 'top' will be used.",
            info = "design_graphic throws a warning on wrong legend presets")

###############################################################################
# Abort checks
###############################################################################

# design_graphic aborts, if segments contains an axes variable
result_list <- dummy_df |>
    design_graphic(axes_variables = "sex",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <segments> variable '",
             info = "design_graphic aborts, if segments contains an axes variable")


# design_graphic aborts with invalid nested axes variable
result_list <- dummy_df |>
    design_graphic(axes_variables = "test + age",
                   segments       = "sex",
                   values         = "weight",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <axes variables> 'test' is not part of",
             info = "design_graphic aborts with invalid nested axes variable")


# design_graphic aborts with axes/segment variable part of values
result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "sex",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <axes>/<segments> variable 'sex' is also part of",
             info = "any_table aborts with row/column variable part of values")

result_list <- dummy_df |>
    design_graphic(axes_variables = "age",
                   segments       = "sex",
                   values         = "age",
                   diagram        = dg_vbars,
                   formats        = list(sex = sex., age = age.),
                   print          = FALSE)

expect_error(print_stack_as_messages("ERROR"), "The provided <axes>/<segments> variable 'age' is also part of",
             info = "any_table aborts with row/column variable part of values")










# any_table aborts with missing variable combination in pre summarised data
# result_list <- sum_df2 |>
#     any_table(rows    = c("year", "age"),
#               columns = "sex",
#               values  = weight_sum,
#               print   = FALSE)
#
# expect_error(print_stack_as_messages("ERROR"), "The variable combination of '",
#              info = "any_table aborts with missing variable combination in pre summarised data")











set_no_print()
set_graphic_options(font = "Arial")
