###############################################################################
# Visuals
###############################################################################
#' Graphic Visuals
#'
#' @description
#' Set different options regarding the visual appearance of a graphic produced by
#' [design_graphic()].
#'
#' @param font Set the font to be used for the entire output.
#' @param title_font_color Font color of the title.
#' @param footnote_font_color Font color of the footnote.
#' @param primary_axes_font_color Font color of the primary axes.
#' @param secondary_axes_font_color Font color of the secondary axes.
#' @param variable_axes_font_color Font color of the variable axes.
#' @param value_font_color Font color of the values.
#' @param label_font_color Font color of the segment labels.
#' @param origin_font_color Font color of the origin text.
#' @param other_font_color Font color of every other text element.
#' @param title_font_face Font face of the title.
#' @param footnote_font_face Font face of the footnote.
#' @param primary_axes_font_face Font face of the primary axes.
#' @param secondary_axes_font_face Font face of the secondary axes.
#' @param variable_axes_font_face Font face of the variable axes.
#' @param value_font_face Font face of the values.
#' @param label_font_face Font face of the segment labels.
#' @param origin_font_face Font face of the origin text.
#' @param other_font_face Font face of every other text element.
#' @param title_alignment The graphic titles alignment.
#' @param footnote_alignment The graphic footnote alignment.
#' @param hbar_alignment The alignment of the axes group labels in horizontal bars.
#' @param other_alignment Alignment of other elements, like freely positionable textboxes.
#' @param color_theme The color theme used for the segments.
#' @param reverse_colors FALSE by default. If TRUE reverses the color order.
#' @param segment_border_color The border color of each segment.
#' @param primary_axes_color The color of the primary axes.
#' @param secondary_axes_color The color of the secondary axes.
#' @param variable_axes_color The color of the variable axes.
#' @param graphic_background_color The background color of entire graphic.
#' @param diagram_background_color The background color of the diagram area.
#' @param graphic_border_color The border color of the entire graphic.
#' @param diagram_border_color The border color of the diagram area.
#' @param line_markers TRUE by default. Draws markers in line charts. If FALSE,
#' doesn't draw markers in line charts.
#' @param line_markers_change TRUE by default. Uses different markers per line. If
#' FALSE, dots are used as markers for all lines.
#' @param guiding_lines FALSE by default. If TRUE draws guiding lines at the y axes tick
#' positions.
#' @param guiding_line_type Sets the type of guiding lines drawn at the y axes tick positions.
#' Can be "dashed", "dotted" or "solid".
#' @param guiding_line_color The color of the guiding lines drawn at the y axes tick positions.
#' @param separation_line_type Sets the type of separation lines drawn between top group
#' categories. Can be "dashed", "dotted" or "solid".
#' @param separation_line_color The color of the separation lines drawn between top group
#' categories.
#' @param remove_small_values TRUE by default. Doesn't display values if the corresponding
#' segment is to small and value would go out of bounds. If FALSE, always displays values.
#' @param bar_values_inside TRUE by default. In grouped bar charts the segment values
#' will be drawn inside the bars. If FALSE, values vill be drawn above/beside the bars.
#' @param label_type Can be "lines", which connects labels and segments with leading lines,
#' or "legend", which allows to position a legend separately.
#' @param label_group If label_Type is "lines", then this parameter determines above which
#' group of segments the labels will be drawn.
#' @param line_offset Offset in the height of the leading lines in cm. If 0, all leading lines
#' will end at the same height, meaning the labels will be drawn at the same height. If
#' an offset is set, all lines after the first one will be shorter by this amount * number
#' of the line. Enables the labels to be drawn in steps, if e.g. the labels have a long
#' text which would overlap, if they where on the same height.
#' @param legend_x_pos Horizontal position of the legend.
#' @param legend_y_pos Vertical position of the legend.
#' @param legend_columns The number of columns in which the labels should be arranged.
#' If 0, all labels will be drawn below each other. If max number of segments, then
#' all labels will be drawn beside each other.
#' @param top_category_position If "top", the top group categories will be drawn above the
#' diagram, otherwise if "bottom" they will be drawn belowthe diagram.
#' @param origin A character value that will be written in the bottom right corner of the
#' graphic.
#'
#' @return
#' Returns a list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' gv <- graphic_visuals()
#'
#' # Set specific options, the rest will be set to default values
#' gv <- graphic_visuals(title_font_color = "#00FF00",
#'                       line_markers     = FALSE)
#'
#' @export
graphic_visuals <- function(font                      = "Arial",
                            title_font_color          = "#000000",
                            footnote_font_color       = "#000000",
                            primary_axes_font_color   = "#000000",
                            secondary_axes_font_color = "#000000",
                            variable_axes_font_color  = "#000000",
                            value_font_color          = "#000000",
                            label_font_color          = "#000000",
                            origin_font_color         = "#2B2B2B",
                            other_font_color          = "#000000",
                            title_font_face           = "bold",
                            footnote_font_face        = "plain",
                            primary_axes_font_face    = "plain",
                            secondary_axes_font_face  = "plain",
                            variable_axes_font_face   = "plain",
                            value_font_face           = "bold",
                            label_font_face           = "plain",
                            origin_font_face          = "plain",
                            other_font_face           = "plain",
                            title_alignment           = "left",
                            footnote_alignment        = "left",
                            hbar_alignment            = "right",
                            other_alignment           = "left",
                            color_theme               = "ocean",
                            reverse_colors            = FALSE,
                            primary_axes_color        = "#2B2B2B",
                            secondary_axes_color      = "#2B2B2B",
                            variable_axes_color       = "#2B2B2B",
                            graphic_background_color  = "#FFFFFF",
                            diagram_background_color  = "#FFFFFF",
                            graphic_border_color      = "#000000",
                            diagram_border_color      = "#FFFFFF",
                            segment_border_color      = "#000000",
                            line_markers              = TRUE,
                            line_markers_change       = TRUE,
                            guiding_lines             = FALSE,
                            guiding_line_type         = "dotted",
                            guiding_line_color        = "#2B2B2B",
                            separation_line_type      = "dashed",
                            separation_line_color     = "#2B2B2B",
                            segment_line_type         = "solid",
                            segment_line_color        = "#000000",
                            remove_small_values       = TRUE,
                            bar_values_inside         = TRUE,
                            label_type                = "lines",
                            label_group               = 3,
                            line_offset               = 0,
                            legend_x_pos              = 11,
                            legend_y_pos              = 5,
                            legend_columns            = 1,
                            top_category_position     = "top",
                            origin                    = "Graphic: qol"){
    as.list(environment())
}


#' Modify Graphic Visuals
#'
#' @description
#' Modify previously set up graphic visual appearance with [graphic_dimensions()].
#'
#' @param visuals_to_modify A pre created graphic visuals object where only
#' certain elements should be modified while the rest is kept as is.
#' @param ... Pass in names and corresponding new values for existing graphic
#' dimension elements.
#'
#' @return
#' Returns a modified list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' gv <- graphic_visuals()
#'
#' # Set specific options, the rest will be set to default values
#' gv <- graphic_visuals(title_font_color = "#00FF00",
#'                       line_markers     = FALSE)
#'
#' # Modify the previously created graphics object
#' gv <- gv |> modify_graphic_visuals(label_group = 2)
#'
#' @export
modify_graphic_visuals <- function(visuals_to_modify, ...){
    visuals_elements <- list(...)

    # Loop through all elements to modify and if exists then set new value
    for (element in seq_along(visuals_elements)){
        name <- names(visuals_elements)[element]

        if (!name %in% names(visuals_to_modify)){
            message(" ! WARNING: Visual element '", name, "' is invalid and will be omitted.")
        }

        visuals_to_modify[[name]] <- visuals_elements[[element]]
    }

    visuals_to_modify
}


###############################################################################
# Axes
###############################################################################
#' Graphic Axes
#'
#' @description
#' Set different options regarding the axes of a graphic produced by
#' [design_graphic()].
#'
#' @param primary_axes_max Maximum value for the primary axes. If "auto", the maximum
#' value is determined by the maximum value present in the graphic.
#' @param primary_axes_min Minimum value for the primary axes. If "auto", the minimum
#' value is determined by the minimum value present in the graphic. If there is no
#' negative value it will always be set to 0.
#' @param primary_axes_steps The number of steps from minimum to maximum value for
#' the primary axes.
#' @param primary_axes_decimals Number of decimal points the values have on the primary axes.
#' @param primary_axes_big_mark Character used as the thousand separator on the primary axes.
#' @param primary_axes_decimal_mark Character used as the decimal separator on the primary axes.
#' @param primary_axes_prefix What to put in front of the value on the primary axes.
#' @param primary_axes_suffix What to put after the value on the primary axes.
#' @param primary_axes_scale Multiplier for the values on the primary axes.
#' @param primary_variable_decimals Number of decimal points the values plotted on the primary axes have.
#' @param primary_variable_big_mark Character used as the thousand separator for the values plotted
#' on the primary axes have.
#' @param primary_variable_decimal_mark Character used as the decimal separator for the values plotted
#' on the primary axes have.
#' @param primary_variable_prefix What to put in front of the values plotted on the primary axes have.
#' @param primary_variable_suffix What to put after the values plotted on the primary axes have.
#' @param primary_variable_scale Multiplier for the values plotted on the primary axes have.
#' @param secondary_axes_max Maximum value for the secondary axes. If "auto", the maximum
#' value is determined by the maximum value present in the graphic.
#' @param secondary_axes_min Minimum value for the secondary axes. If "auto", the minimum
#' value is determined by the minimum value present in the graphic. If there is no
#' negative value it will always be set to 0.
#' @param secondary_axes_steps The number of steps from minimum to maximum value for
#' the secondary axes.
#' @param secondary_axes_decimals Number of decimal points the values have on the secondary axes.
#' @param secondary_axes_big_mark Character used as the thousand separator on the secondary axes.
#' @param secondary_axes_decimal_mark Character used as the decimal separator on the secondary axes.
#' @param secondary_axes_prefix What to put in front of the value on the secondary axes.
#' @param secondary_axes_suffix What to put after the value on the secondary axes.
#' @param secondary_axes_scale Multiplier for values on the secondary axes.
#' @param secondary_variable_decimals Number of decimal points the values plotted on the secondary axes have.
#' @param secondary_variable_big_mark Character used as the thousand separator for the values plotted
#' on the secondary axes.
#' @param secondary_variable_decimal_mark Character used as the decimal separator for the values plotted
#' on the secondary axes.
#' @param secondary_variable_prefix What to put in front of the values plotted on the secondary axes.
#' @param secondary_variable_suffix What to put after the values plotted on the secondary axes.
#' @param secondary_variables_scale Multiplier for the values plotted on the secondary axes.
#' @param variable_axes_interval The number of steps in which to display labels on the variable axes.
#'
#' @return
#' Returns a list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' ga <- graphic_axes()
#'
#' # Set specific options, the rest will be set to default values
#' ga <- graphic_axes(primary_axes_max      = 100,
#'                    primary_axes_decimals = 1)
#'
#' @export
graphic_axes <- function(primary_axes_max                = "auto",
                         primary_axes_min                = "auto",
                         primary_axes_steps              = 5,
                         primary_axes_decimals           = 0,
                         primary_axes_big_mark           = ".",
                         primary_axes_decimal_mark       = ",",
                         primary_axes_prefix             = "",
                         primary_axes_suffix             = "",
                         primary_axes_scale              = 1,
                         primary_variable_decimals       = 0,
                         primary_variable_big_mark       = ".",
                         primary_variable_decimal_mark   = ",",
                         primary_variable_prefix         = "",
                         primary_variable_suffix         = "",
                         primary_variable_scale          = 1,
                         secondary_axes_max              = "auto",
                         secondary_axes_min              = "auto",
                         secondary_axes_steps            = 5,
                         secondary_axes_decimals         = 0,
                         secondary_axes_big_mark         = ".",
                         secondary_axes_decimal_mark     = ",",
                         secondary_axes_prefix           = "",
                         secondary_axes_suffix           = "",
                         secondary_axes_scale            = 1,
                         secondary_variable_decimals     = 0,
                         secondary_variable_big_mark     = ".",
                         secondary_variable_decimal_mark = ",",
                         secondary_variable_prefix       = "",
                         secondary_variable_suffix       = "",
                         secondary_variables_scale       = 1,
                         variable_axes_interval          = 1){
    as.list(environment())
}


#' Modify Graphic Axes
#'
#' @description
#' Modify previously set up graphic visual appearance with [graphic_dimensions()].
#'
#' @param axes_to_modify A pre created graphic visuals object where only
#' certain elements should be modified while the rest is kept as is.
#' @param ... Pass in names and corresponding new values for existing graphic
#' dimension elements.
#'
#' @return
#' Returns a modified list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' ga <- graphic_axes()
#'
#' # Set specific options, the rest will be set to default values
#' ga <- graphic_axes(primary_axes_max      = 100,
#'                    primary_axes_decimals = 1)
#'
#' # Modify the previously created graphics object
#' ga <- ga |> modify_graphic_axes(primary_axes_min = 50)
#'
#' @export
modify_graphic_axes <- function(axes_to_modify, ...){
    axes_elements <- list(...)

    # Loop through all elements to modify and if exists then set new value
    for (element in seq_along(axes_elements)){
        name <- names(axes_elements)[element]

        if (!name %in% names(axes_to_modify)){
            message(" ! WARNING: Visual element '", name, "' is invalid and will be omitted.")
        }

        axes_to_modify[[name]] <- axes_elements[[element]]
    }

    axes_to_modify
}


###############################################################################
# Dimensions
###############################################################################
#' Graphic Dimensions
#'
#' @description
#' Set different options regarding the dimensions of a graphic produced by [design_graphic()].
#'
#' @param graphic_width The width of the whole graphic.
#' @param graphic_height The height of the whole graphic.
#' @param diagram_start The starting position of the main diagram within the graphic from the top.
#' @param diagram_width The width of the main diagram within the graphic.
#' @param diagram_height The height of the main diagram within the graphic.
#' @param margins Inner margins to the graphic borders in cm.
#' @param title_font_size Font size of the title
#' @param footnote_font_size Font size of the footnote
#' @param axes_font_size Font size of the axes
#' @param value_font_size Font size of the values.
#' @param label_font_size Font size of the segment labels.
#' @param label_font_size Font size of the origin text.
#' @param origin_font_size Font size of the origin text.
#' @param other_font_size Font size of every other text element.
#' @param space_between_bars_pct The space between adjacent bars in percent.
#' @param line_thickness The thickness of lines in points.
#' @param segment_line_length The length of the lines leading from segments to labels in cm.
#' @param textbox_width Determines the width of any freely placed textbox.
#'
#' @return
#' Returns a list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' gd <- graphic_dimensions()
#'
#' # Set specific options, the rest will be set to default values
#' gd <- graphic_dimensions(graphic_width  = 10,
#'                          graphic_height = 10)
#'
#' @export
graphic_dimensions <- function(graphic_width          = 16,
                               graphic_height         = 9,
                               diagram_start          = "auto",
                               diagram_width          = "auto",
                               diagram_height         = "auto",
                               margins                = 0.2,
                               title_font_size        = 9,
                               footnote_font_size     = 8,
                               axes_font_size         = 9,
                               value_font_size        = 9,
                               label_font_size        = 9,
                               origin_font_size       = 8,
                               other_font_size        = 9,
                               space_between_bars_pct = 50,
                               line_thickness         = 0,
                               segment_line_length    = 3,
                               textbox_width          = 2){
    if (is.numeric(diagram_width) && diagram_width > graphic_width){
        message(" ! WANRING: Diagram width can't be greater than the whole graphic width.\n",
                "            Diagram width will be reduced to graphic width.")

        diagram_width <- graphic_width
    }

    if (is.numeric(diagram_height) && diagram_height > graphic_height){
        message(" ! WANRING: Diagram height can't be greater than the whole graphic height.\n",
                "            Diagram height will be reduced to graphic height.")

        diagram_width <- graphic_width
    }

    if (is.numeric(diagram_start) && (diagram_start < 0 || diagram_start > graphic_height)){
        message(" ! WANRING: Diagram start can't be smaller than 0 or greater than the whole graphic height.\n",
                "            Diagram will start at the margin position.")

        diagram_start <- margins
    }

    as.list(environment())
}


#' Modify Graphic Dimensions
#'
#' @description
#' Modify previously set up graphic dimensions with [graphic_dimensions()].
#'
#' @param dimension_to_modify A pre created graphic dimension object where only
#' certain elements should be modified while the rest is kept as is.
#' @param ... Pass in names and corresponding new values for existing graphic
#' dimension elements.
#'
#' @return
#' Returns a modified list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' gd <- graphic_dimensions()
#'
#' # Set specific options, the rest will be set to default values
#' gd <- graphic_dimensions(graphic_width  = 10,
#'                          graphic_height = 10)
#'
#' # Modify the previously created graphics object
#' gd <- gd |> modify_graphic_dimensions(title_font_size = 12)
#'
#' @export
modify_graphic_dimensions <- function(dimension_to_modify, ...){
    dimension_elements <- list(...)

    # Loop through all elements to modify and if exists then set new value
    for (element in seq_along(dimension_elements)){
        name <- names(dimension_elements)[element]

        if (!name %in% names(dimension_to_modify)){
            message(" ! WARNING: Dimension element '", name, "' is invalid and will be omitted.")
        }

        dimension_to_modify[[name]] <- dimension_elements[[element]]
    }

    dimension_to_modify
}


###############################################################################
# Output
###############################################################################
#' Graphic Output
#'
#' @description
#' Set different options regarding the dimensions of a graphic produced by [design_graphic()].
#'
#' @param save_path If NULL, only draws the graphic in the plot window. Otherwise
#' specify an output path.
#' @param file If NULL, only draws the graphic in the plot window. Otherwise specify
#' a filename with extension.
#' @param resolution DPI resolution.
#'
#' @return
#' Returns a list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' gout <- graphic_output()
#'
#' # Set specific options, the rest will be set to default values
#' gout <- graphic_output(save_path = "C:/MyFolder/",
#'                        file      = "MyGraphic.png")
#'
#' @export
graphic_output <- function(save_path  = NULL,
                           file       = NULL,
                           resolution = 300){
    as.list(environment())
}


#' Modify Graphic Output
#'
#' @description
#' Modify previously set up graphic output with [graphic_output()].
#'
#' @param output_to_modify A pre created graphic output object where only
#' certain elements should be modified while the rest is kept as is.
#' @param ... Pass in names and corresponding new values for existing graphic
#' output elements.
#'
#' @return
#' Returns a modified list of named graphic options.
#'
#' @seealso
#' The main graphic function: [design_graphic()]
#'
#' Other graphic options:
#'
#' Additional graphic functions: [add_textbox()]
#'
#' @examples
#' # For default values
#' gout <- graphic_output()
#'
#' # Set specific options, the rest will be set to default values
#' gout <- graphic_output(save_path = "C:/MyFolder/",
#'                        file      = "MyGraphic.png")
#'
#' # Modify the previously created graphics object
#' gout <- gout |> modify_graphic_output(file = "MyGraphic.pdf")
#'
#' @export
modify_graphic_output <- function(output_to_modify, ...){
    output_elements <- list(...)

    # Loop through all elements to modify and if exists then set new value
    for (element in seq_along(output_elements)){
        name <- names(output_elements)[element]

        if (!name %in% names(output_to_modify)){
            message(" ! WARNING: Output element '", name, "' is invalid and will be omitted.")
        }

        output_to_modify[[name]] <- output_elements[[element]]
    }

    output_to_modify
}
