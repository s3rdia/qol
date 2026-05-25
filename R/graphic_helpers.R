###############################################################################
# Textbox
###############################################################################
#' Add Textboxes As Graphical Object
#'
#' @name textboxes
#'
#' @description
#' [add_textbox()]: Create a textbox as graphical object. The dimensions and overall visual appearance
#' is flexible. Text wraps automatically, if the provided text is wider than the
#' textbox itself.
#'
#' @param text The text that should be displayed.
#' @param x_pos X starting position of the textbox in cm.
#' @param y_pos Y starting position of the textbox in cm. 0 position is at the top.
#' @param width Width of the textbox in cm.
#' @param alignment Horizontal text alignment.
#' @param font Name of the font to be used.
#' @param font_color Font color as hex code.
#' @param font_size Font size.
#' @param font_face Valid values are "plain", "bold", "italic", "oblique", and "bold.italic".
#' @param line_height The height of a single text line.
#' @param name The internal name of the textbox with which it can be identified.
#' @param draw FALSE by default. If TRUE, directly draws the textbox onto the canvas.
#'
#' @seealso
#' High level graphic functions: [design_graphic()]
#'
#' Mid level graphic functions:
#'
#' Add textboxes: [add_textbox()], [add_title()], [add_footnote()], [add_graphic_origin()]
#'
#' Viewport: [setup_main_canvas()], [is_viewport_pushed()]
#'
#' Dimensions: [get_available_width()], [get_available_height()]
#'
#' @return
#' Returns a grid::textGrob object.
#'
#' @rdname textboxes
#'
#' @export
add_textbox <- function(text,
                        x_pos       = .qol_options[["graphic_dimensions"]][["margins"]],
                        y_pos       = .qol_options[["graphic_dimensions"]][["margins"]],
                        width       = .qol_options[["graphic_dimensions"]][["graphic_width"]],
                        alignment   = .qol_options[["graphic_visuals"]][["other_alignment"]],
                        font        = .qol_options[["graphic_visuals"]][["font"]],
                        font_color  = .qol_options[["graphic_visuals"]][["other_font_color"]],
                        font_size   = .qol_options[["graphic_dimensions"]][["other_font_size"]],
                        font_face   = .qol_options[["graphic_visuals"]][["other_font_face"]],
                        line_height = .qol_options[["graphic_fine_tuning"]][["line_height"]],
                        name        = "textbox",
                        draw        = FALSE){
    # Return if there is no text
    if (is.null(text) || length(text) == 0 || text == ""){
        return(invisible(grid::nullGrob()))
    }

    # In case text is provided as vector with multiple elements, meaning the line
    # splitting already took place manually, combine elements with line breaks
    # and return graphical object.
    # In case text already contains manual line breaks assume that the user knows
    # what he is doing and keep text as is.
    if (length(text) > 1 || grepl("\n", text)){
        if (length(text) > 1){
            text <- paste(text, collapse = "\n")
        }

        textbox <- grid::textGrob(label = text,
                                  x     = grid::unit(x_pos, "native"),
                                  y     = grid::unit(y_pos, "native"),
                                  just  = c(alignment, "top"),
                                  name  = name,
                                  gp    = grid::gpar(col = font_color,
                                                     fontfamily = font,
                                                     fontsize   = font_size,
                                                     fontface   = font_face,
                                                     lineheight = line_height))
        if (draw){
            grid::grid.draw(textbox)
        }

        return(invisible(textbox))
    }

    # Get words and check if there are any
    words <- unlist(strsplit(text, " "))

    if (length(words) == 0){
        return(grid::nullGrob())
    }
    # If there is only one word, just return as graphical object
    else if (length(words) == 1){
        textbox <- grid::textGrob(label = words,
                                  x     = grid::unit(x_pos, "native"),
                                  y     = grid::unit(y_pos, "native"),
                                  just  = c(alignment, "top"),
                                  name  = name,
                                  gp    = grid::gpar(col = font_color,
                                                     fontfamily = font,
                                                     fontsize   = font_size,
                                                     fontface   = font_face,
                                                     lineheight = line_height))
        if (draw){
            grid::grid.draw(textbox)
        }

        return(invisible(textbox))
    }

    # Now put the words back into lines
    text_lines <- list()

    while (length(words) > 0){
        # Get the number of words fitting in one line of text
        number_of_words <- get_fitting_words(words, width, font, font_size, font_face)

        # Add the words as a separate text line and remove them afterwards from the
        # vector carrying all words.
        text_lines <- c(text_lines, paste(words[1:number_of_words], collapse = " "))
        words      <- words[-(1:number_of_words)]
    }

    # Combine line vector with line breaks
    wrapped_text <- paste(text_lines, collapse = "\n")

    # Return graphics element
    textbox <- grid::textGrob(label = wrapped_text,
                              x     = grid::unit(x_pos, "native"),
                              y     = grid::unit(y_pos, "native"),
                              just  = c(alignment, "top"),
                              name  = name,
                              gp    = grid::gpar(col = font_color,
                                                 fontfamily = font,
                                                 fontsize   = font_size,
                                                 fontface   = font_face,
                                                 lineheight = line_height))
    if (draw){
        grid::grid.draw(textbox)
    }

    invisible(textbox)
}


#' Quick Wrap A Single Line Of Text
#'
#' @description
#' Wrap text based on a provided textbox width. The function tries to find the optimal
#' number of words fitting in one line.
#'
#' @param words A vector of single words.
#' @param textbox_width Width of the textbox in cm.
#' @param font Name of the font to be used.
#' @param font_size Font size.
#' @param font_face Valid values are "plain", "bold", "italic", "oblique", and "bold.italic".
#'
#' @return
#' Returns the number of words fitting in one line based on the provided words vector.
#'
#' @noRd
get_fitting_words <- function(words,
                              textbox_width,
                              font,
                              font_size,
                              font_face){
    low_end  <- 1
    high_end <- length(words)
    fitting_words <- 1

    # Using a quick sort approach here. Basically jumping from mid point to mid point
    # to determine the number of words which fit in one line.
    while (low_end <= high_end){
        mid_point <- floor((low_end + high_end) / 2)

        # Test whether the text line from low_end to the mid_point fits in one line
        test_line <- paste(words[1:mid_point], collapse = " ")
        temp_grob <- grid::textGrob(test_line, gp = grid::gpar(fontfamily = font,
                                                               fontsize   = font_size,
                                                               fontface   = font_face))

        test_width <- grid::convertWidth(grid::grobWidth(temp_grob), "native", valueOnly = TRUE)

        # If the text fits, save the number of word up to this point and set
        # a new low end. The loop will go on and try to add more words.
        if (test_width <= textbox_width){
            fitting_words <- mid_point
            low_end       <- mid_point + 1
        }
        # If the text doesn't fit set a new high end and try to get a match with
        # fewer words.
        else{
            high_end <- mid_point - 1
        }
    }

    fitting_words
}


#' @description
#' [wrap_single_text()]: Wrap text based on a provided width.
#'
#' @param text_line A single line of text to wrap.
#' @param textbox_width Width of the textbox in cm.
#'
#' @return
#' Returns wrapped character.
#'
#' @rdname textboxes
#'
#' @export
wrap_single_text <- function(text_line,
                             textbox_width,
                             font,
                             font_size,
                             font_face){
    # Get words and check if there are any
    words <- unlist(strsplit(text_line, " "))

    if (length(words) == 0){
        return("")
    }

    # Now put the words back into lines
    text_lines <- list()

    while (length(words) > 0){
        # Get the number of words fitting in one line of text
        number_of_words <- get_fitting_words(words, textbox_width, font, font_size, font_face)

        # Add the words as a separate text line and remove them afterwards from the
        # vector carrying all words.
        text_lines <- c(text_lines, paste(words[1:number_of_words], collapse = " "))
        words      <- words[-(1:number_of_words)]
    }

    # Combine line vector with line breaks
    paste(text_lines, collapse = "\n")
}


#' @description
#' [wrap_text_vector()]: Wrap every single element of a character vector individually.
#'
#' @param text_lines A vector of texts to wrap.
#'
#' @return
#' Returns wrapped character vector.
#'
#' @rdname textboxes
#'
#' @export
wrap_text_vector <- function(text_lines,
                             textbox_width,
                             font,
                             font_size,
                             font_face){
    vapply(text_lines,
           wrap_single_text,
           textbox_width = textbox_width,
           font          = font,
           font_size     = font_size,
           font_face     = font_face,
           FUN.VALUE     = character(1),
           USE.NAMES     = FALSE)
}


#' @description
#' Set the textbox anchor point according to selected alignment.
#'
#' @param dimensions qol package dimensions options.
#' @param visuals qol package visuals options.
#' @param which Can be "title" or "footnote".
#'
#' @noRd
fix_alignment <- function(dimensions = .qol_options[["graphic_dimensions"]],
                          visuals    = .qol_options[["graphic_visuals"]],
                          which      = "title"){
    # Adjust x position according to alignment. This is necessary for center and
    # right alignment because not only this changes but also the anchor point.
    if (tolower(visuals[[paste0(which, "_alignment")]]) == "left"){
        dimensions[["margins"]]
    }
    if (tolower(visuals[[paste0(which, "_alignment")]]) == "right"){
        dimensions[["graphic_width"]] - dimensions[["margins"]]
    }
    else if (tolower(visuals[[paste0(which, "_alignment")]]) == "center"){
        dimensions[["graphic_width"]] / 2
    }
    else{
        dimensions[["margins"]]
    }
}


#' @description
#' [add_title()]: Create a textbox as graphical object. A wrapper to easily create
#' a textbox at the top of the graphic as title.
#'
#' @param dimensions qol package dimensions options.
#' @param visuals qol package visuals options.
#' @param fine_tuning qol package fine tuning options.
#'
#' @rdname textboxes
#'
#' @export
add_title <- function(text,
                      dimensions  = .qol_options[["graphic_dimensions"]],
                      visuals     = .qol_options[["graphic_visuals"]],
                      fine_tuning = .qol_options[["graphic_fine_tuning"]],
                      draw        = FALSE){
    if (length(text) == 0){
        return(invisible(grid::nullGrob()))
    }

    x_pos <- fix_alignment(dimensions, visuals)

    invisible(
        add_textbox(text        = text,
                    x_pos       = x_pos,
                    y_pos       = dimensions[["graphic_height"]] - dimensions[["margins"]],
                    width       = get_available_width(dimensions),
                    alignment   = visuals[["title_alignment"]],
                    font        = visuals[["font"]],
                    font_color  = visuals[["title_font_color"]],
                    font_size   = dimensions[["title_font_size"]],
                    font_face   = visuals[["title_font_face"]],
                    line_height = fine_tuning[["line_height"]],
                    name        = "title",
                    draw        = draw))
}


#' @description
#' [add_footnote()]: Create a textbox as graphical object. A wrapper to easily create
#' a textbox at the bottom of the graphic as footnote.
#'
#' @rdname textboxes
#'
#' @export
add_footnote <- function(text,
                         dimensions  = .qol_options[["graphic_dimensions"]],
                         visuals     = .qol_options[["graphic_visuals"]],
                         fine_tuning = .qol_options[["graphic_fine_tuning"]],
                         draw        = FALSE){
    if (length(text) == 0){
        return(invisible(grid::nullGrob()))
    }

    x_pos <- fix_alignment(dimensions, visuals, "footnote")

    # Add textbox as normal first
    footnote <- add_textbox(text        = text,
                            x_pos       = x_pos,
                            y_pos       = dimensions[["margins"]],
                            width       = get_available_width(dimensions),
                            alignment   = visuals[["footnote_alignment"]],
                            font        = visuals[["font"]],
                            font_color  = visuals[["footnote_font_color"]],
                            font_size   = dimensions[["footnote_font_size"]],
                            font_face   = visuals[["footnote_font_face"]],
                            line_height = fine_tuning[["line_height"]],
                            name        = "footnote")

    # Adjust position afterwards to the bottom
    footnote <- grid::editGrob(footnote,
                               y    = grid::unit(dimensions[["margins"]], "native"),
                               just = c(visuals[["footnote_alignment"]], "bottom"))

    if (draw){
        grid::grid.draw(footnote)
    }

    invisible(footnote)
}


#' @description
#' [add_graphic_origin()]: Create a textbox as graphical object. A wrapper to easily create
#' a textbox at the bottom right of the graphic as information who created the graphic.
#'
#' @rdname textboxes
#'
#' @export
add_graphic_origin <- function(dimensions = .qol_options[["graphic_dimensions"]],
                               visuals    = .qol_options[["graphic_visuals"]],
                               draw       = FALSE){
   # Add textbox as normal first
    origin <- add_textbox(text       = visuals[["origin"]],
                          x_pos      = dimensions[["graphic_width"]] - dimensions[["margins"]],
                          y_pos      = dimensions[["margins"]],
                          width      = get_available_width(dimensions),
                          alignment  = visuals[["footnote_alignment"]],
                          font       = visuals[["font"]],
                          font_color = visuals[["origin_font_color"]],
                          font_size  = dimensions[["origin_font_size"]],
                          font_face  = visuals[["origin_font_face"]],
                          name       = "origin")

    # Adjust position afterwards to the bottom
    origin <- grid::editGrob(origin,
                             y    = grid::unit(dimensions[["margins"]], "native"),
                             just = c("right", "bottom"))

    if (draw){
        grid::grid.draw(origin)
    }

    invisible(origin)
}


#' @description
#' [register_windows_font()]: Windows only. Registers the font to be used.
#'
#' @rdname textboxes
#'
#' @export
register_windows_font <- function(font){
    if (.Platform$OS.type == "windows"){
        registered_fonts <- names(grDevices::windowsFonts())

        # Only register if font isn't already registered
        if (!(font %in% registered_fonts)){
            font_args <- list(grDevices::windowsFont(font))
            names(font_args) <- font

            do.call(grDevices::windowsFonts, font_args)
        }
    }
}


###############################################################################
# Viewport
###############################################################################
#' Set Up Viewports To Draw On
#'
#' @name viewport
#'
#' @description
#' [setup_main_canvas()]: Setup the main graphic viewport and push it to the graphics
#' tree. Additionally creates a new page before.
#'
#' @param width Viewport width.
#' @param height Viewport height
#' @param line_height The height of a single text line.
#' @param name The internal name of the canvas with which it can be identified.
#'
#' @return
#' Returns a grid::viewport object.
#'
#' @rdname viewport
#'
#' @export
setup_main_canvas <- function(width         = .qol_options[["graphic_dimensions"]][["graphic_width"]],
                              height        = .qol_options[["graphic_dimensions"]][["graphic_height"]],
                              line_height   = .qol_options[["graphic_fine_tuning"]][["line_height"]],
                              layout_row    = NULL,
                              layout_column = NULL,
                              name          = "main_canvas"){
    if (is.null(layout_row)){
        grid::grid.newpage()
    }

    # Set up the main viewport for the entire graphic
    vp <- grid::viewport(width  = grid::unit(width, "cm"),
                         height = grid::unit(height, "cm"),
                         xscale = c(0, width),
                         yscale = c(0, height),
                         gp     = grid::gpar(lineheight = line_height + 0.1),
                         layout.pos.row = layout_row,
                         layout.pos.col = layout_column,
                         name   = name)

    grid::pushViewport(vp)

    invisible(vp)
}


#' @description
#' [setup_nested_viewport()]: Setup a graphic viewport and push it to the graphics
#' tree. The function assumes that it is called after setting up a main canvas.
#'
#' @param x_pos Viewport starting x position.
#' @param y_pos Viewport starting y position.
#' @param y_scale The viewports vertical scaling.
#'
#' @rdname viewport
#'
#' @export
setup_nested_viewport <- function(x_pos       = 0,
                                  y_pos       = 0,
                                  y_scale     = c(0, 1),
                                  width       = .qol_options[["graphic_dimensions"]][["graphic_width"]],
                                  height      = .qol_options[["graphic_dimensions"]][["graphic_height"]],
                                  line_height = .qol_options[["graphic_fine_tuning"]][["line_height"]],
                                  name        = "nested_viewport"){
    # Set up a new nested viewport.
    vp <- grid::viewport(x      = grid::unit(x_pos, "native"),
                         y      = grid::unit(y_pos, "native"),
                         width  = grid::unit(width, "native"),
                         height = grid::unit(height, "native"),
                         yscale = y_scale,
                         just   = c("left", "top"),
                         gp     = grid::gpar(lineheight = line_height + 0.1),
                         name   = name)

    grid::pushViewport(vp)

    invisible(vp)
}


#' @description
#' [setup_diagram_viewport()]: Setup the main diagram viewport based on the list
#' of arguments passed on by [design_graphic()].
#'
#' @param arguments Argument list passed passed on by [design_graphic()].
#'
#' @rdname viewport
#'
#' @export
setup_diagram_viewport <- function(arguments){
    dimensions <- arguments[["dimensions"]]

    # Set up a new viewport for the whole diagram area to be able to safely work
    # in this area.
    setup_nested_viewport(x_pos   = dimensions[["margins"]],
                          y_pos   = dimensions[["diagram_start"]],
                          y_scale = c(0, 1),
                          width   = dimensions[["diagram_width"]],
                          height  = dimensions[["diagram_height"]],
                          line_height = arguments[["fine_tuning"]][["line_height"]],
                          name = "diagram_area")
}


#' @description
#' [setup_diagram_viewport()]: Setup the main diagram viewport based on the list
#' of arguments passed on by [design_graphic()].
#'
#' @param arguments Argument list passed passed on by [design_graphic()].
#'
#' @rdname viewport
#'
#' @export
setup_nested_diagram_viewport <- function(arguments){
    # Set up a new viewport for the whole diagram area to be able to safely work
    # in this area.
    outer_viewport <- setup_diagram_viewport(arguments)

    # Measure segment, group and axes dimensions
    diagram_info <- get_diagram_dimensions(arguments)

    diagram_info[["outer_viewport"]] <- outer_viewport

    # If all values are negative, the group label positions are vertically inverted
    # later. Which means normally they are drawn below the variable axes (vbars),
    # but if the values are all negative they are drawn at the top. This means
    # the viewport has to be set up accordingly and needs to be shifted down by
    # the size of the group labels.
    if (diagram_info[["zero_pos"]] != 1){
        # In case of just one group layer, the viewport will be drawn at the top
        if (length(diagram_info[["group_label_heights"]]) == 1){
            y_pos <- 1
        }
        # If there are more group layers, subtract the first layer height from the
        # top, because the first layer will be drawn at the top, alls others below
        # the axes.
        else{
            y_pos <- 1 - diagram_info[["group_label_heights"]][1]
        }
    }
    else{
        # In case of just one group layer, the viewport will be shifted down by
        # the height of this layer.
        if (length(diagram_info[["group_label_heights"]]) == 1){
            y_pos <- 1 - diagram_info[["group_label_heights"]][1]
        }
        # If there are more group layers, subtract the first layer height from the
        # overall height, because the first layer will be drawn at the bottom, not
        # at the top like the others.
        else{
            y_pos <- 1 - (diagram_info[["entire_group_label_height"]] - diagram_info[["group_label_heights"]][1])
        }
    }

    # Setup the main inner diagram viewport
    diagram_info[["inner_viewport"]] <-
        setup_nested_viewport(x_pos   = diagram_info[["primary_y_axes_width"]],
                              y_pos   = grid::unit(y_pos, "native"),
                              y_scale = c(diagram_info[["primary_y_min"]], diagram_info[["primary_y_max"]]),
                              width   = grid::convertUnit(grid::unit(1.0, "npc"), "native", valueOnly = TRUE)
                                      - diagram_info[["primary_y_axes_width"]],
                              height  = 1 - diagram_info[["entire_group_label_height"]],
                              line_height = arguments[["fine_tuning"]][["line_height"]],
                              name    = "main_diagram")

    invisible(diagram_info)
}


#' @description
#' [back_to_the_root()]: Pops out of all viewports.
#'
#' @param to_main TRUE by default. Pops out of all viewports but the main canvas.
#'
#' @return
#' Returns the grid::current.vpPath.
#'
#' @rdname viewport
#'
#' @export
back_to_the_root <- function(to_main = TRUE){
    # Go back to the root
    if (!to_main){
        grid::popViewport(n = 0)
    }
    # Go back to the main canvas
    else{
        # Get the current depth of the viewport stack to know how many viewports
        # were pushed.
        number_of_viewports <- length(grid::current.vpPath())

        # If there is more than one viewport pop out of all but the first one.
        # Otherwise the graphic already is at the root viewport.
        if (number_of_viewports > 1){
            grid::popViewport(n = number_of_viewports - 1)
        }
    }

    invisible(grid::current.vpPath())
}


#' @description
#' [is_viewport_pushed()]: Check whether a viewport was pushed after setting up
#' a new canvas.
#'
#' @return
#' [is_viewport_pushed()]: Returns TRUE/FALSE.
#'
#' @rdname viewport
#'
#' @export
is_viewport_pushed <- function(){
    current_vp <- grid::current.vpPath()

    !is.null(current_vp)
}


###############################################################################
# Dimensions
###############################################################################
#' Get The Available Width Or Height Without Margins
#'
#' @name dimensions
#'
#' @description
#' [get_available_width()]: Calculate the available width from the total graphic
#' width minus the margins on both sides. The function can either be used with the
#' dimensions options from this package or with manually set width and margins.
#' This is an either/or option.
#'
#' @param dimensions qol package dimensions options.
#' @param total_width The width of the entire graphic.
#' @param margins The margins used on both ends of the graphic.
#'
#' @return
#' [get_available_width()]: Returns a numeric value for the available width.
#'
#' @rdname dimensions
#'
#' @export
get_available_width <- function(dimensions  = .qol_options[["graphic_dimensions"]],
                                total_width = NULL,
                                margins     = 0){
    # If a specific total width is provided use it for the calculation
    if (!is.null(total_width)){
        total_width - (margins * 2)
    }
    # Otherwise use the passed dimensions element
    else{
        dimensions[["graphic_width"]] - (dimensions[["margins"]] * 2)
    }
}


#' @description
#' [get_available_height()]: Calculate the available height from the total graphic
#' height minus the margins on both sides. The function can either be used with the
#' dimensions options from this package or with manually set height and margins.
#' This is an either/or option.
#'
#' @param total_height The height of the entire graphic.
#'
#' @return
#' [get_available_height()]: Returns a numeric value for the available height.
#'
#' @rdname dimensions
#'
#' @export
get_available_height <- function(dimensions   = .qol_options[["graphic_dimensions"]],
                                 total_height = NULL,
                                 margins      = 0){
    # If a specific total height is provided use it for the calculation
    if (!is.null(total_height)){
        total_height - (margins * 2)
    }
    # Otherwise use the passed dimensions element
    else{
        dimensions[["graphic_height"]] - (dimensions[["margins"]] * 2)
    }
}

###############################################################################
# Diagram
###############################################################################
#' Get The Dimensions And Positions Of Groups And Segments
#'
#' @name diagram
#'
#' @description
#' [get_diagram_dimensions()]: Get various parameters concerning the division and
#' position of groups and segments. These are e.g. number of groups/segments, width of
#' groups/segments, ids or positions among other things.
#'
#' @param arguments Argument list passed passed on by [design_graphic()].
#'
#' @return
#' Returns a list of parameters for groups, segments and axes.
#'
#' @rdname diagram
#'
#' @export
get_diagram_dimensions <- function(arguments){
    # Grab necessary values
    graphic_tab  <- arguments[["graphic_tab"]]
    axes_vars    <- arguments[["axes_vars"]]
    segment_vars <- arguments[["segment_vars"]]
    values       <- arguments[["values"]]
    axes         <- arguments[["axes"]]
    dimensions   <- arguments[["dimensions"]]
    visuals      <- arguments[["visuals"]]
    fine_tuning  <- arguments[["fine_tuning"]]
    stat_labels  <- arguments[["stat_labels"]]

    # Get actual values and statistic types
    value_types <- sub(".*_", "", values)
    values      <- graphic_tab[[values]]

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Basic horizontal positioning for vbars
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Get the unique values of the axes and segment variables to determine their
    # actual numbers. This is needed to measure the space each group and each
    # segment takes.
    unique_groups      <- do.call(paste, c(graphic_tab[axes_vars], sep = "_")) |> collapse::funique()
    number_of_groups   <- length(unique_groups)
    unique_segments    <- graphic_tab[[segment_vars]] |> as.character() |> collapse::funique()
    number_of_segments <- length(unique_segments)
    number_of_elements <- number_of_groups * number_of_segments

    # Now get the group elements per axes variable to draw them in their individual
    # space later.
    individual_groups <- lapply(graphic_tab[axes_vars], function(variable){
        variable |> collapse::funique()
    })

    individual_number_of_groups <- sapply(individual_groups, length)

    for (i in seq_along(individual_groups)){
        if (i == 1){
            next
        }

        individual_number_of_groups[i] <- length(individual_groups[[i]]) * individual_number_of_groups[i - 1]
    }

    # Actual space calculation. Add margins for the whole graphic and the individual
    # groups to give everything a bit air to breathe.
    margin        <- fine_tuning[["diagram_margin"]]
    group_width   <- 1 / number_of_groups
    segment_width <- (group_width - (margin * 2)) / number_of_segments

    individual_group_widths <- vapply(individual_number_of_groups, function(no_of_groups){
        1 / no_of_groups
    }, numeric(1))

    # First get the group numbers zero based, then the segment numbers within
    # each group also zero based. Finally calculate all starting x positions of
    # each segment.
    running_nr  <- seq_len(number_of_elements) - 1
    group_ids   <- floor(running_nr / number_of_segments)
    segment_ids <- running_nr %% number_of_segments
    segment_pos <- margin + (group_ids * group_width) + (segment_ids * segment_width)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # y axes calculations
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Get the values and tick positions for the y axes, which is basically an even
    # distribution.
    primary_y_values   <- get_y_axes_values(values, axes, fine_tuning)
    #secondary_y_values <- get_y_axes_values(values, axes, fine_tuning, "secondary")

    primary_y_tick_width <- 1 / (length(primary_y_values) - 1)
    #secondary_y_tick_width <- 1 / length(primary_y_values)
    primary_y_tick_pos   <- c(0:(length(primary_y_values) - 1)) * primary_y_tick_width
    #secondary_y_tick_pos <- c(1:length(secondary_y_values)) * secondary_y_tick_width

    # Get extreme axes values and distance
    primary_y_max <- collapse::fmax(primary_y_values)
    primary_y_min <- collapse::fmin(primary_y_values)
    primary_y_distance <- abs(primary_y_max - primary_y_min)
    #secondary_y_max <- collapse::fmax(secondary_y_values)
    #secondary_y_min <- collapse::fmin(secondary_y_values)
    #secondary_y_distance <- abs(secondary_y_max - secondary_y_min)

    # Determine the 0 position of the y axes. Normally at the bottom or where ever
    # 0 is.
    zero_pos <- 0

    if (0 %in% primary_y_values){
        zero_pos <- primary_y_tick_pos[primary_y_values == 0]
    }
    # If 0 value isn't displayed on the y axes and all values are negative,
    # draw the x axes at the top
    else if (collapse::fmin(primary_y_values) < 0){
        zero_pos <- 1
    }

    # Calculate the actual drawing heights of the segments, which is needed in case
    # of the bottom line of the viewport not being the zero line of the axes. Or if
    # the zero line starts at a higher value than zero.
    if (primary_y_min <= 0){
        if (primary_y_max < 0){
            actual_drawing_height <- values - primary_y_max
        }
        else{
            actual_drawing_height <- values
        }
    }
    else{
        actual_drawing_height <- values - primary_y_min
    }

    # Get width of the highest value for the axes to measure the actual space the
    # whole axes needs in the diagram.
    if (abs(primary_y_max) > abs(primary_y_min)){
        primary_y_axes_width <- graphic_tab |>
            get_value_axes_width(primary_y_max, axes, dimensions, visuals, fine_tuning)
    }
    # In case of negative value having more digits
    else{
        primary_y_axes_width <- graphic_tab |>
            get_value_axes_width(primary_y_min, axes, dimensions, visuals, fine_tuning)
    }
    # TODO: CONDITIONALLY DECIDE WHEN TO DO THE SECONDARY AXES.

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Calculations concerning the segment value positioning
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Setup adjustment for vbar values, depending on whether the values are positive
    # or negative and whether they are rotated by 90 degrees or not.
    if (!visuals[["rotate_values"]]){
        values_inner_vjust <- data.table::fifelse(values >= 0,
                                                  fine_tuning[["values_vjust_positive"]],
                                                  fine_tuning[["values_vjust_negative"]])
        values_outer_vjust <- data.table::fifelse(values >= 0,
                                                  fine_tuning[["values_vjust_negative"]],
                                                  fine_tuning[["values_vjust_positive"]])
    }
    else{
        values_inner_vjust <- data.table::fifelse(values >= 0,
                                                  fine_tuning[["values_vjust_90_positive"]],
                                                  fine_tuning[["values_vjust_90_negative"]])
        values_outer_vjust <- data.table::fifelse(values >= 0,
                                                  fine_tuning[["values_vjust_90_negative"]],
                                                  fine_tuning[["values_vjust_90_positive"]])
    }

    # Get center vbar position
    values_x_pos <- segment_pos + (segment_width * 0.5)

    # Format values to final form
    formatted_values <- format_diagram_values(list(values = values, value_types = value_types),
                                              list(axes        = axes,
                                                   visuals     = visuals,
                                                   fine_tuning = fine_tuning,
                                                   stat_labels = stat_labels))

    # Check whether values fit in vbar segments. If values don't fit, mark the exact
    # positions so that later functions can decide what to do with these values.
    values_width <- get_values_width(formatted_values,
                                     dimensions,
                                     visuals)

    values_fit_vertical <- TRUE

    if (visuals[["bar_values_inside"]]){
        if (!visuals[["rotate_values"]]){
            # Determine whether a value should be drawn inside or outside the segment
            values_fit_vertical <- (get_values_height(list(values            = values,
                                                          primary_y_distance = primary_y_distance),
                                                      dimensions, visuals, fine_tuning) * fine_tuning[["value_overlap_factor"]] < abs(values))
        }
        else{
            # Determine whether a value should be drawn inside or outside the segment
            values_fit_vertical <- swap_xy_scaling(values_width, dimensions) *
                                   primary_y_distance *
                                   fine_tuning[["values_vjust_positive"]] < abs(values)
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Label positioning
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Calculate the positions of the group labels. This is done for each individual
    # group layer.
    group_label_x_pos    <- list()
    group_label_y_pos    <- c()
    wrapped_group_labels <- list()
    group_label_heights  <- c()

    for (i in seq_along(individual_number_of_groups)){
        # Get the centered positions where to draw the group labels
        group_label_x_pos[[i]] <- ((seq_len(individual_number_of_groups[i]) - 1) * individual_group_widths[i]) + (individual_group_widths[i] * 0.5)

        # Format group labels on variable x axes
        wrapped_group_labels[[i]] <- wrap_text_vector(individual_groups[[i]],
                                                      individual_group_widths[i] - (margin * 2),
                                                      visuals[["font"]],
                                                      dimensions[["axes_font_size"]],
                                                      visuals[["axes_font_face"]])

        # Get dimensions of group labels
        group_label_heights[i] <- get_variable_axes_height(wrapped_group_labels[[i]], dimensions, visuals, fine_tuning)

        # Calculate y positions for multi layered group labels
        if (i <= 2){
            group_label_y_pos[i] <- 0
        }
        else{
            group_label_y_pos[i] <- group_label_heights[i] + (fine_tuning[["variable_axes_margin"]] * (i - 1)) + group_label_y_pos[i - 1]
        }
    }

    # In case there are more than three group layers, the order of the layers below
    # the axes has to be reversed so that they are drawn in the correct order,
    # meaning the broadest at the bottom and the finest at the top.
    if (length(group_label_x_pos) > 2){
        group_label_x_pos    <- c(group_label_x_pos[1],    rev(group_label_x_pos[-1]))
        wrapped_group_labels <- c(wrapped_group_labels[1], rev(wrapped_group_labels[-1]))
        group_label_heights  <- c(group_label_heights[1],  rev(group_label_heights[-1]))
    }

    # Change symbol of y positions because group label layers are drawn from top to bottom
    group_label_y_pos <- -group_label_y_pos

    # Marker whether we deal with multi layered grouping labels
    is_multi_group_label <- length(group_label_x_pos) > 1

    # Determine the height of the entire grouping labels
    entire_group_label_height <- collapse::fsum(group_label_heights) + (margin * (length(group_label_heights) - 1))

    # Get tick positions
    group_ticks_pos_x <- get_group_tick_positions_x(number_of_groups, number_of_segments,
                                                    segment_width, segment_pos)

    # Format segment labels
    segment_label_textbox_width <- dimensions[["textbox_width"]] / dimensions[["diagram_width"]]
    wrapped_segment_labels <- wrap_text_vector(unique_segments,
                                               segment_label_textbox_width,
                                               visuals[["font"]],
                                               dimensions[["axes_font_size"]],
                                               visuals[["axes_font_face"]])

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Separation lines
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Get the separation line positions by selecting the tick positions which are
    # centered between two groups
    number_of_ticks <- length(group_ticks_pos_x) - 1

    group_separation_lines_x <- lapply(individual_number_of_groups[-length(individual_number_of_groups)], function(number){
        # Build a stepped sequence which captures the number of ticks a group spans over
        tick_positions <- seq(from = 1, to = number_of_ticks, by = number_of_ticks / number)
        tick_positions <- tick_positions[-1]

        group_ticks_pos_x[tick_positions]
    })

    # Some entries are now the same throughout the list vectors. These identical entries
    # are now cleared from the vectors, so that each layer of separation lines doesn't
    # intersect any other separation line.
    if (length(group_separation_lines_x) > 1){
        for (i in seq(length(group_separation_lines_x), 2)){
            group_separation_lines_x[[i]] <- setdiff(group_separation_lines_x[[i]], group_separation_lines_x[[i - 1]])
        }
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Return information
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Return information as list
    list(values                      = values,
         value_types                 = value_types,
         unique_groups               = unique_groups,
         individual_groups           = individual_groups,
         number_of_groups            = number_of_groups,
         individual_number_of_groups = individual_number_of_groups,
         unique_segments             = unique_segments,
         number_of_segments          = number_of_segments,
         number_of_elements          = number_of_elements,
         group_width                 = group_width,
         individual_group_widths     = individual_group_widths,
         segment_width               = segment_width,
         running_nr                  = running_nr,
         group_ids                   = group_ids,
         segment_ids                 = segment_ids,
         segment_pos                 = segment_pos,
         primary_y_values            = primary_y_values,
         primary_y_tick_width        = primary_y_tick_width,
         primary_y_tick_pos          = primary_y_tick_pos,
         primary_y_max               = primary_y_max,
         primary_y_min               = primary_y_min,
         primary_y_distance          = primary_y_distance,
         zero_pos                    = zero_pos,
         actual_drawing_height       = actual_drawing_height,
         primary_y_axes_width        = primary_y_axes_width,
         values_inner_vjust          = values_inner_vjust,
         values_outer_vjust          = values_outer_vjust,
         values_x_pos                = values_x_pos,
         formatted_values            = formatted_values,
         values_width                = values_width,
         values_fit_vertical         = values_fit_vertical,
         group_label_x_pos           = group_label_x_pos,
         group_label_y_pos           = group_label_y_pos,
         wrapped_group_labels        = wrapped_group_labels,
         group_label_heights         = group_label_heights,
         entire_group_label_height   = entire_group_label_height,
         group_ticks_pos_x           = group_ticks_pos_x,
         segment_label_textbox_width = segment_label_textbox_width,
         wrapped_segment_labels      = wrapped_segment_labels,
         is_multi_group_label        = is_multi_group_label,
         number_of_ticks             = number_of_ticks,
         group_separation_lines_x    = group_separation_lines_x)
}


#' @description
#' [vbar_grob()]: Set up the main segments for vertical bars.
#'
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#' @param theme Color theme used for the segments.
#'
#' @return
#' Returns a grid::rectGrob object.
#'
#' @rdname diagram
#'
#' @export
#'
vbar_grob <- function(diagram_info,
                      arguments,
                      theme){
    visuals     <- arguments[["visuals"]]
    dimensions  <- arguments[["dimensions"]]
    fine_tuning <- arguments[["fine_tuning"]]
    value_y_pos <- diagram_info[["values"]]

    border_color <- visuals[["segment_border_color"]]
    shrink_width <- grid::unit(dimensions[["space_between_bars_pct"]], "pt")

    # Prevent color usage overflow, when there are more segments than color usage
    # patterns.
    color_usage        <- arguments[["color_usage"]]
    number_of_segments <- diagram_info[["number_of_segments"]]

    if (number_of_segments > length(color_usage)){
        print_message("WARNING", c("More segments than available colors. Maximum number",
                                   "of colors will be used."))

        color_usage <- color_usage[[length(color_usage)]]
    }
    else{
        color_usage <- color_usage[[number_of_segments]]
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Generate rectangles
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Check whether the provided border color is a single hex code. If it is not
    # and is a theme name instead, then get the theme colors to make it visually
    # as if there where no borders. This way the borders are colored like the inner
    # space.
    if (!grepl("^#([A-Fa-f0-9]{6})$", border_color)){
        border_color <- get_theme_base_colors(border_color)

        # Get color usage to determine which colors to pick for the specific number
        # of segments
        border_color <- border_color[color_usage]

        # If borders are colored, it becomes obvious that the segments actually overlap
        # by one pixel. To conceal this the segment width will be reduced by a bit.
        if (dimensions[["space_between_bars_pct"]] == 0){
            shrink_width <- grid::unit(fine_tuning[["shrink_segment_width"]], "pt")
        }
    }

    # Get the colors from the theme which should be used. Reverse them if option
    # is set accordingly.
    colors_to_use <- theme[["base"]][color_usage]

    if (visuals[["reverse_colors"]]){
        colors_to_use <- rev(colors_to_use)
        border_color  <- rev(border_color)
    }

    # NOTE: For the width a tiny bit is subtracted because the bars would otherwise
    #       overlap by one pixel, in case colored outlines are used.
    rects <- grid::rectGrob(x      = grid::unit(diagram_info[["segment_pos"]], "native") + (shrink_width / 2),
                            y      = diagram_info[["zero_pos"]],
                            width  = grid::unit(diagram_info[["segment_width"]], "native") - shrink_width,
                            height = grid::unit(diagram_info[["actual_drawing_height"]], "native"),
                            just   = c("left", "bottom"),
                            name   = "segments",
                            gp     = grid::gpar(fill = colors_to_use,
                                                col  = border_color,
                                                lwd  = dimensions[["line_thickness"]]))

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Generate value texts
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    if (visuals[["display_values"]]){
        number_of_values <- length(value_y_pos)

        hjust      <- numeric(number_of_values)
        vjust      <- numeric(number_of_values)
        rotate     <- numeric(number_of_values)
        font_color <- character(number_of_values)

        # If a small scale is used it can happen that a negative symbol appears in front
        # of 0 values. This gets removed here.
        formatted_values                   <- diagram_info[["formatted_values"]]
        formatted_values[value_y_pos == 0] <- sub("–", "", formatted_values[value_y_pos == 0])

        # Get the specific font colors for the respective set up of segments
        colors_inside  <- rep(theme[["font_inside"]][color_usage], diagram_info[["number_of_groups"]])
        colors_outside <- rep(theme[["font_outside"]][color_usage], diagram_info[["number_of_groups"]])

        # Determine whether a value should be drawn inside or outside the segment
        values_fit_inside <- diagram_info[["values_fit_vertical"]]

        # Set font color according to whether values are drawn inside or outside the segments
        font_color[values_fit_inside]  <- colors_inside[values_fit_inside]
        font_color[!values_fit_inside] <- colors_outside[!values_fit_inside]

        y_offset <- grid::unit(0, "mm")

        if (!visuals[["rotate_values"]]){
            hjust  <- fine_tuning[["values_hjust"]]
            rotate <- 0

            if (visuals[["bar_values_inside"]]){
                # Set up values drawn inside and outside the segments
                vjust[values_fit_inside]  <- diagram_info[["values_inner_vjust"]][values_fit_inside]
                vjust[!values_fit_inside] <- diagram_info[["values_outer_vjust"]][!values_fit_inside]
            }
            # Set up values drawn outside segments
            else{
                vjust      <- diagram_info[["values_outer_vjust"]]
                font_color <- colors_outside
            }
        }
        # Set up with rotated values
        else{
            vjust_base  <- fine_tuning[["values_hjust_90"]]
            rotate      <- fine_tuning[["values_rotation"]]

            if (visuals[["bar_values_inside"]]){
                # Set up values drawn inside segments
                hjust[values_fit_inside] <- diagram_info[["values_inner_vjust"]][values_fit_inside]
                vjust[values_fit_inside] <- vjust_base + fine_tuning[["values_hjust_90_plus"]]

                # Set up justification for values that don't fit inside the bars
                hjust[!values_fit_inside] <- diagram_info[["values_outer_vjust"]][!values_fit_inside]
                vjust[!values_fit_inside] <- vjust_base

                # Justification is set to the extremes depending on whether values
                # are positive or negative. Instead of setting the value offset via
                # justification, a fixed offset is added to the y coordinate.
                y_offset <- data.table::fifelse(hjust <= 0,
                                                grid::unit(fine_tuning[["values_vjust_90_positive"]], "mm"),
                                                grid::unit(-fine_tuning[["values_vjust_90_positive"]] + fine_tuning[["values_vjust_90_negative"]], "mm"))
                hjust    <- data.table::fifelse(hjust <= 0, 0, 1)
            }
            # All values drawn outside the bars
            else{
                # Justification is set to the extremes depending on whether values
                # are positive or negative. Instead of setting the value offset via
                # justification, a fixed offset is added to the y coordinate.
                hjust      <- data.table::fifelse(value_y_pos >= 0, 0, 1)
                y_offset   <- data.table::fifelse(value_y_pos >= 0,
                                                  grid::unit(fine_tuning[["values_vjust_90_positive"]], "mm"),
                                                  grid::unit(-fine_tuning[["values_vjust_90_positive"]], "mm"))

                vjust      <- vjust_base
                font_color <- colors_outside
            }
        }

        # If all values are negative and the y axes is at the top of the diagram,
        # 0 values should be drawn below the y axes.
        if (collapse::fmax(value_y_pos) == 0){
            if (!visuals[["rotate_values"]]){
                vjust[value_y_pos == 0] <- fine_tuning[["values_below_axes_just"]]
            }
            else{
                hjust[value_y_pos == 0] <- fine_tuning[["values_below_axes_90_just"]]
            }
        }

        # Add offset to y axes for values equal to 0 if they are rotated
        if (visuals[["rotate_values"]]){
            value_y_pos[value_y_pos == 0] <- grid::convertUnit(grid::unit(fine_tuning[["values_zero_line_offset"]], "cm"),
                                                               "native", valueOnly = TRUE) * diagram_info[["primary_y_distance"]]
        }

        # Reverse font colors, if option is set accordingly.
        if (visuals[["reverse_colors"]]){
            font_color <- rev(font_color)
        }

        # Generate formatted values
        texts <- grid::textGrob(formatted_values,
                                x      = grid::unit(diagram_info[["values_x_pos"]], "native"),
                                y      = grid::unit(value_y_pos, "native") + y_offset,
                                vjust  = vjust,
                                hjust  = hjust,
                                rot    = rotate,
                                name   = "values",
                                gp     = grid::gpar(col        = font_color,
                                                    fontfamily = visuals[["font"]],
                                                    fontsize   = dimensions[["value_font_size"]],
                                                    fontface   = visuals[["value_font_face"]],
                                                    lineheight = fine_tuning[["line_height"]]))
    }
    else{
        texts <- grid::nullGrob()
    }

    # Return final segments
    grid::gList(rects, texts)
}

###############################################################################
# Axes
###############################################################################
#' Measure And Generate Custom Axes
#'
#' @name axes
#'
#' @description
#' [get_value_axes_width()]: Get the width of the primary or secondary value
#' axes. The highest value will be put into its final format first, after that
#' the width is measured.
#'
#' @param graphic_tab The base data frame for the graphic.
#' @param max_value The maximum value which takes the most space.
#' @param axes The list of axes parameters.
#' @param dimensions The list of dimensions parameters.
#' @param visuals The list of visual parameters.
#' @param fine_tuning The list of fine tuning parameters.
#' @param which Primary or secondary axes.
#'
#' @return
#' [get_value_axes_width()]: Returns a numeric width.
#'
#' @rdname axes
#'
#' @export
get_value_axes_width <- function(graphic_tab,
                                 max_value,
                                 axes,
                                 dimensions,
                                 visuals,
                                 fine_tuning,
                                 which = "primary"){
    which <- tolower(which)

    # Format the number to see, whether additional symbols are added according to
    # settings.
    axes_max <- format_values(max_value,
                              axes[[paste0(which, "_axes_decimals")]],
                              axes[[paste0(which, "_axes_big_mark")]],
                              axes[[paste0(which, "_axes_decimal_mark")]],
                              axes[[paste0(which, "_axes_prefix")]],
                              axes[[paste0(which, "_axes_suffix")]],
                              axes[[paste0(which, "_axes_scale")]])

    # Create test graphical object to measure the actual width
    temp_grob <- grid::textGrob(axes_max,
                                gp = grid::gpar(fontfamily = visuals[["font"]],
                                                fontsize   = dimensions[["axes_font_size"]],
                                                fontface   = visuals[[paste0(which, "_axes_font_face")]]))

    # Return width
    grid::convertWidth(grid::grobWidth(temp_grob), "native", valueOnly = TRUE) + fine_tuning[["value_axes_margin"]]
}


#' @description
#' [get_variable_axes_height()]: Get the height of the variable axes.
#'
#' @param wrapped_text The wrapped text for the variable axes to measure.
#'
#' @return
#' [get_variable_axes_height()]: Returns a numeric height.
#'
#' @rdname axes
#'
#' @export
get_variable_axes_height <- function(wrapped_text,
                                     dimensions,
                                     visuals,
                                     fine_tuning){
    # Create test graphical object to measure the actual height
    temp_grob <- grid::textGrob(wrapped_text,
                                gp = grid::gpar(fontfamily = visuals[["font"]],
                                                fontsize   = dimensions[["axes_font_size"]],
                                                fontface   = visuals[["variable_axes_font_face"]],
                                                lineheight = fine_tuning[["line_height"]] + 0.1))

    # Measure height by manual calculation because the grobHeight comes out way
    # too small.
    height_cm <- (grid::convertHeight(grid::stringHeight(temp_grob[["label"]]), "cm", valueOnly = TRUE)
                  / dimensions[["graphic_height"]])

    collapse::fmax(height_cm) + fine_tuning[["variable_axes_margin"]]
}


#' @description
#' [get_values_width()]: Get the width of the segment values.
#'
#' @param formatted_values The formatted values in their final form.
#'
#' @return
#' [get_values_width()]: Returns a numeric width
#'
#' @rdname axes
#'
#' @export
get_values_width <- function(formatted_values,
                             dimensions,
                             visuals){
    # The unit measuring functions are checking the font values from the current
    # viewport. Since there are multiple different font options used on the same
    # viewport they can't be set in general. Therefor a temporary viewport with the
    # specific options has to be set up.
    grid::pushViewport(grid::viewport(gp = grid::gpar(fontfamily = visuals[["font"]],
                                                      fontsize   = dimensions[["value_font_size"]],
                                                      fontface   = visuals[["value_font_face"]])))

    # Measure individual widths
    widths <- grid::convertWidth(grid::stringWidth(formatted_values),
                                 "native", valueOnly = TRUE)

    # Release viewport
    grid::popViewport()

    # Width seems to be measured too small. Adding a bit on top seems to help.
    widths
}


#' @description
#' [get_values_height()]: Get the height of the segment values.
#'
#' @return
#' [get_values_height()]: Returns a numeric height.
#'
#' @rdname axes
#'
#' @export
get_values_height <- function(diagram_info,
                              dimensions,
                              visuals,
                              fine_tuning){
    # The unit measuring functions are checking the font values from the current
    # viewport. Since there are multiple different font options used on the same
    # viewport they can't be set in general. Therefor a temporary viewport with the
    # specific options has to be set up.
    grid::pushViewport(grid::viewport(gp = grid::gpar(fontfamily = visuals[["font"]],
                                                      fontsize   = dimensions[["value_font_size"]],
                                                      fontface   = visuals[["value_font_face"]],
                                                      lineheight = fine_tuning[["line_height"]] + 0.1)))

    # Measure individual heights
    height <- grid::convertHeight(grid::stringHeight(diagram_info[["values"]]),
                                  "native", valueOnly = TRUE) * diagram_info[["primary_y_distance"]]

    # Release viewport
    grid::popViewport()

    # Height seems to be measured too small. Adding a bit on top seems to help.
    collapse::fmax(height) * fine_tuning[["values_vjust_positive"]]
}


#' @description
#' [swap_xy_scaling()]: Width and height are measured according to the individual scaling
#' for each dimension. This function converts the measured to the respective other dimension.
#'
#' @param measuring The values received from [get_values_width()] or [get_values_height()].
#' @param from Input measuring: Can be "width" or "height".
#' @param to Output measuring: Can be "width" or "height".
#' @param part Determine measuring on which part of the graphic. Can be "graphic", "diagram" or
#' "inner_canvas".
#'
#' @return
#' [swap_xy_scaling()]: Returns a vector of width or heights.
#'
#' @rdname axes
#'
#' @export
swap_xy_scaling <- function(measuring,
                            dimensions,
                            from = "width",
                            to   = "height",
                            part = "diagram"){
    measuring * dimensions[[paste0(part, "_", from)]] / dimensions[[paste0(part, "_", to)]]
}


#' @description
#' [get_group_tick_positions_x()]: Calculates the x axes tick positions. In this
#' calculation ticks are meant to enclose the group labels. Meaning ticks are drawn
#' between groups.
#'
#' @param number_of_groups The number of groups.
#' @param number_of_segments The number of segments per group.
#' @param segment_width The width of a single segment
#' @param segment_pos A vector containing all segment positions.
#'
#' @return
#' [get_group_tick_positions_x()]: Returns a vector of numeric tick positions.
#'
#' @rdname axes
#'
#' @export
get_group_tick_positions_x <- function(number_of_groups,
                                       number_of_segments,
                                       segment_width,
                                       segment_pos){
    # Get the ending positions of each segment
    segment_end_pos <- segment_pos + segment_width

    group_ticks <- sapply(1:(number_of_groups - 1), function(group){
        # Get the ending position of the last segment inside a group
        last_bar_end_pos <- segment_end_pos[group * number_of_segments]

        # Get the starting position of the first segment of the upcoming group
        next_bar_start_pos <- segment_pos[(group * number_of_segments) + 1]

        # Calculate the midpoint between these two segments
        (last_bar_end_pos + next_bar_start_pos) / 2
    })

    # Insert starting and ending tick
    c(0, group_ticks, 1)
}


#' @description
#' [get_y_axes_values()]: Generates a vector containing the values drawn on the
#' y axes. Values are equally spaced between the minimum and maximum value.
#'
#' @param values Value vector from the data frame.
#'
#' @return
#' [get_y_axes_values()]: Returns a numeric vector of equally spaced round values.
#'
#' @rdname axes
#'
#' @export
get_y_axes_values <- function(values,
                              axes,
                              fine_tuning,
                              which = "primary"){
    which <- tolower(which)

    # Get global values first
    global_min <- axes[[paste0(which, "_axes_min")]]
    global_max <- axes[[paste0(which, "_axes_max")]]

    # Replace auto generated value by statically set global values
    if (global_min != "auto" && global_min != global_max){
        min_value <- global_min
    }
    # Get values from the actual data frame and add to the value, so that the highest
    # value doesn't go up to the end of the axes. Leave a little room to breathe.
    else{
        min_value <- collapse::fmin(c(0, values))
        min_value <- min_value * fine_tuning[["y_axes_scaling"]]
    }

    # Replace auto generated value by statically set global values
    if (global_max != "auto" && global_min != global_max){
        max_value <- global_max
    }
    # Get values from the actual data frame and add to the value, so that the highest
    # value doesn't go up to the end of the axes. Leave a little room to breathe.
    else{
        max_value <- collapse::fmax(c(0, values))
        max_value <- max_value * fine_tuning[["y_axes_scaling"]]
    }

    # In case minimum value is greater than the maximum value, just swap them
    if (min_value > max_value){
        temp_value <- max_value
        min_value  <- max_value
        max_value  <- temp_value
    }

    # Return equally spaced round values in given range
    even_prettier(min_value, max_value, axes[[paste0(which, "_axes_steps")]])
}


#' @description
#' [even_prettier()]: Generates a sequence of pretty steps for equal length ticks.
#' The function calculates more granular results than the base [pretty()] function.
#'
#' @param min_value Minimum starting value.
#' @param max_value Maximum end value.
#' @param number_of_steps The number of equal length steps to be used to head from
#' the minimum to the maximum value.
#'
#' @return
#' [even_prettier()]: Returns a pretty sequence of equal length numeric values.
#'
#' @rdname axes
#'
#' @export
even_prettier <- function(min_value,
                          max_value,
                          number_of_steps){
    # Define possible intervals
    standard_intervals <- c(0.1, 0.125, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9,
                              1,  1.25,  1.5,   2,  2.5,   3,   4,   5,   6,   7,  7.5,   8,   9,
                             10,  12.5,   15,  20,   25,  30,  40,  50,  60,  70,   75,  80,  90,
                            100)

    # In case the minimum value is negative and the maximum value is positive,
    # it has to be made sure, that 0 is always part of the distribution.
    if (min_value < 0 && max_value > 0){
        total_span <- max_value - min_value

        # Calculate step distribution and make sure it stays between the given bounds
        negative_steps <- round((abs(min_value) / total_span) * number_of_steps)
        negative_steps <- pmax(1, pmin(negative_steps, number_of_steps - 1))
        positive_steps <- number_of_steps - negative_steps

        # Calculate possible candidate steps
        needed_step_size <- max(abs(min_value) / negative_steps, max_value / positive_steps)
        candidates       <- 10^floor(log10(abs(needed_step_size))) * standard_intervals

        # Calculate score to determine which of the candidates fits best. The smaller
        # the score, the nearer the equal length step hits the boundaries.
        scores <- abs((-negative_steps * candidates) - min_value) +
                  abs((positive_steps  * candidates) - max_value)

        # Penalize candidates that undershoot the provided boundaries to make sure
        # that the provided boundaries are basically the minimum edge case values.
        undershoots         <- (-negative_steps * candidates > min_value) | (positive_steps * candidates < max_value)
        scores[undershoots] <- scores[undershoots] + (total_span * 10)

        # Select the step with the smallest score which is used as the equal length step
        pretty_step <- candidates[which.min(scores)]

        # Return the pretty sequence
        return(seq(-negative_steps * pretty_step, positive_steps * pretty_step, by = pretty_step))
    }

    # In case there are only positive or negative values
    # Calculate possible candidate steps
    needed_step_size <- (max_value - min_value) / number_of_steps
    candidates       <- 10^floor(log10(abs(needed_step_size))) * standard_intervals

    # Calculate possible candidate boundaries
    lowers <- floor(min_value   / candidates) * candidates
    uppers <- ceiling(max_value / candidates) * candidates

    # Calculate step sizes and the corresponding score to determine which step size
    # is nearest to the provided number of steps.
    step_sizes <- (uppers - lowers) / candidates
    scores     <- abs(step_sizes - number_of_steps)

    # Select the step with the smallest score which is used as the equal length step
    score_id    <- which.min(scores)
    pretty_step <- candidates[which.min(scores)]

    # Return the pretty sequence
    seq(lowers[score_id], uppers[score_id], by = pretty_step)
}


#' @description
#' [setup_y_axes()]: Uses the currently active viewport to set up the y axes. The
#' primary axes is drawn on the left, the secondary axes on the right side of the
#' currently active viewport.
#'
#' @param tick_positions The position of the axes ticks.
#' @param tick_values The values drawn at the tick positions.
#' @param arguments Argument list passed passed on by [design_graphic()].
#' @param secondary FALSE by default. If TRUE draws the primary axes on the left side,
#' otherwise the secondary axes on the right side.
#'
#' @return
#' [setup_y_axes()], [setup_x_axes()], [setup_xy_axes()]: Returns a grid::gTree
#' object containing the full axes.
#'
#' @rdname axes
#'
#' @export
setup_y_axes <- function(tick_positions,
                         tick_values,
                         arguments,
                         secondary = FALSE){
    visuals    <- arguments[["visuals"]]
    dimensions <- arguments[["dimensions"]]

    which <- "primary"

    if (secondary){
        which <- "secondary"
    }

    # Vertical axes line over the whole viewport
    zero_pos <- sum(secondary)

    line <- grid::linesGrob(x = c(zero_pos, zero_pos), y = c(0, 1),
                            gp = grid::gpar(col = visuals[[paste0(which, "_axes_color")]],
                                            lwd = dimensions[["axes_line_thickness"]]))

    # Setup the ticks left/right
    #tick_length <- 0.1 + (-0.2 * zero_pos)
    tick_length_cm <- arguments[["fine_tuning"]][["tick_length"]] * 5
    tick_length    <- tick_length_cm + ((-2 * tick_length_cm) * zero_pos)

    ticks <- grid::segmentsGrob(x0   = zero_pos,
                                x1   = grid::unit(zero_pos, "native") - grid::unit(tick_length, "cm"),
                                y0   = grid::unit(tick_positions, "npc"),
                                y1   = grid::unit(tick_positions, "npc"),
                                name = paste0("y_ticks"),
                                gp   = grid::gpar(col = visuals[[paste0(which, "_axes_color")]],
                                                  lwd = dimensions[["axes_line_thickness"]]))

    # Draw guiding lines
    if (visuals[["guiding_lines_y"]]){
        guiding_lines <- grid::segmentsGrob(x0   = 0,
                                            x1   = 1,
                                            y0   = grid::unit(tick_positions, "npc"),
                                            y1   = grid::unit(tick_positions, "npc"),
                                            name = paste0("y_guiding_lines"),
                                            gp   = grid::gpar(col = visuals[["guiding_line_color"]],
                                                              lty = visuals[["guiding_line_type"]],
                                                              lwd = dimensions[["guiding_line_thickness"]]))
    }
    else{
        guiding_lines <- grid::nullGrob()
    }

    # Format values according to options
    axes <- arguments[["axes"]]

    formatted_tick_values <- format_values(tick_values,
                                           axes[[paste0(which, "_axes_decimals")]],
                                           axes[[paste0(which, "_axes_big_mark")]],
                                           axes[[paste0(which, "_axes_decimal_mark")]],
                                           axes[[paste0(which, "_axes_prefix")]],
                                           axes[[paste0(which, "_axes_suffix")]],
                                           axes[[paste0(which, "_axes_scale")]])

    # When values are placed at the tick positions, they are drawn slightly of to
    # the bottom. So the positions need to be manually adjusted.
    value_positions <- grid::convertUnit(grid::unit(tick_positions, "native")
                                       + grid::unit(0.1, "char"),   "native", valueOnly = TRUE)

    # Insert the formatted values for the value axes
    axes_margin  <- arguments[["fine_tuning"]][["value_axes_margin"]]
    label_offset <- -axes_margin + ((2 * axes_margin) * zero_pos)

    axes_values <- grid::textGrob(label = formatted_tick_values,
                                  x     = label_offset,
                                  y     = value_positions,
                                  just  = c("right", "center"),
                                  name  = paste0("y_values"),
                                  gp    = grid::gpar(col        = visuals[[paste0(which, "_axes_font_color")]],
                                                     fontfamily = visuals[["font"]],
                                                     fontsize   = dimensions[["axes_font_size"]],
                                                     fontface   = visuals[["axes_font_face"]]))

    # Return the whole axes as one graphical object
    grid::gTree(children = grid::gList(guiding_lines, line, ticks, axes_values),
                name     = "y_axes")
}


#' @description
#' [setup_x_axes()]: Uses the currently active viewport to set up the x axes. The
#' height at which it is drawn is determined by the primary y axes 0 point. If y0
#' is not on the y axes then the x axes will either be drawn at the bottom, if only
#' positive values are present, or at the top, if only negative values are present.
#'
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#'
#' @rdname axes
#'
#' @export
setup_x_axes <- function(diagram_info,
                         arguments){
    fine_tuning          <- arguments[["fine_tuning"]]
    visuals              <- arguments[["visuals"]]
    dimensions           <- arguments[["dimensions"]]

    tick_length          <- fine_tuning[["tick_length"]]
    tick_positions       <- diagram_info[["group_ticks_pos_x"]]
    label_x_positions    <- diagram_info[["group_label_x_pos"]]
    label_y_positions    <- diagram_info[["group_label_y_pos"]]
    labels               <- diagram_info[["wrapped_group_labels"]]
    zero_pos             <- diagram_info[["zero_pos"]]
    is_multi_group_label <- diagram_info[["is_multi_group_label"]]
    group_label_heights  <- diagram_info[["group_label_heights"]]
    group_separation_lines_x <- diagram_info[["group_separation_lines_x"]]

    # If all values are negative, the group label positions are vertically inverted.
    # Which means normally they are drawn below the variable axes (vbars),
    # but if the values are all negative they are drawn at the top.
    if (zero_pos != 1){
        label_y    <- label_y_positions - fine_tuning[["variable_axes_margin"]]
        label_just <- c("center", "top")

        # The first layer of multi layered group labels is drawn at the top of the
        # diagram, not below the axes.
        if (is_multi_group_label){
            label_y[[1]] <- 1 + group_label_heights[1] + fine_tuning[["variable_axes_margin"]]

            # Get the separation lines y coordinates
            separation_lines_y <- list()

            for (i in seq_len(length(group_separation_lines_x))){
                # The first separation lines are special in the way that they start higher,
                # at the top of the upper grouping labels.
                if (i == 1){
                    separation_lines_y[[i]] <- c(label_y[[1]], label_y[[length(label_y)]] - group_label_heights[[length(label_y)]])
                }
                # All other lines start at the top of the diagram and are drawn to the
                # bottom of nested super group.
                else{
                    invers_element <- length(label_y) - (i - 2)

                    separation_lines_y[[i]] <- c(1, label_y[[invers_element]] - group_label_heights[[invers_element]])
                }
            }
        }
    }
    else{
        # Ticks point up if the x axes is drawn at the top.
        tick_length <- -tick_length
        label_y     <- 1 - label_y_positions + fine_tuning[["variable_axes_margin"]]
        label_just  <- c("center", "bottom")

        # The first layer of multi layered group labels is drawn at the bottom of the
        # diagram, not on top of the axes.
        if (is_multi_group_label){
            label_y[[1]] <- -diagram_info[["group_label_heights"]][1] - fine_tuning[["variable_axes_margin"]]

            # Get the separation lines y coordinates
            separation_lines_y <- list()

            for (i in seq_len(length(group_separation_lines_x))){
                # The first separation lines are special in the way that they start higher,
                # at the top of the upper grouping labels.
                if (i == 1){
                    separation_lines_y[[i]] <- c(label_y[[1]], label_y[[length(label_y)]] + group_label_heights[[length(label_y)]])
                }
                # All other lines start at the top of the diagram and are drawn to the
                # bottom of nested super group.
                else{
                    invers_element <- length(label_y) - (i - 2)

                    separation_lines_y[[i]] <- c(0, label_y[[invers_element]] + group_label_heights[[invers_element]])
                }
            }
        }
    }

    # Horizontal axes line over the whole viewport. The axes will be drawn at the 0
    # position of the primary y axes if it is there.
    line <- grid::linesGrob(x    = c(0, 1), y = c(zero_pos, zero_pos),
                            name = paste0("x_axes"),
                            gp   = grid::gpar(col = visuals[["variable_axes_color"]],
                                              lwd = dimensions[["axes_line_thickness"]]))

    # Setup the ticks pointing down
    ticks <- grid::segmentsGrob(x0   = grid::unit(tick_positions, "native"),
                                x1   = grid::unit(tick_positions, "native"),
                                y0   = zero_pos,
                                y1   = zero_pos - tick_length,
                                name = paste0("x_ticks"),
                                gp   = grid::gpar(col = visuals[["variable_axes_color"]],
                                                  lwd = dimensions[["axes_line_thickness"]]))

    # Draw guiding lines
    if (visuals[["guiding_lines_x"]]){
        guiding_lines <- grid::segmentsGrob(x0   = grid::unit(tick_positions, "npc"),
                                            x1   = grid::unit(tick_positions, "npc"),
                                            y0   = 0,
                                            y1   = 1,
                                            name = paste0("x_guiding_lines"),
                                            gp   = grid::gpar(col = visuals[["guiding_line_color"]],
                                                              lty = visuals[["guiding_line_type"]],
                                                              lwd = dimensions[["guiding_line_thickness"]]))
    }
    else{
        guiding_lines <- grid::nullGrob()
    }

    # Insert the group labels for the variable axes
    if (!is_multi_group_label){
        group_labels <- grid::textGrob(label = labels[[1]],
                                       x     = label_x_positions[[1]],
                                       y     = label_y,
                                       just  = label_just,
                                       name  = paste0("group_labels"),
                                       gp    = grid::gpar(col        = visuals[["variable_axes_font_color"]],
                                                          fontfamily = visuals[["font"]],
                                                          fontsize   = dimensions[["axes_font_size"]],
                                                          fontface   = visuals[["axes_font_face"]],
                                                          lineheight = fine_tuning[["line_height"]]))

        # Return the whole axes as one graphical object
        grid::gTree(children = grid::gList(line, ticks, group_labels), name = "x_axes")
    }
    # In case of multi layer group labels capture the single individual graphic objects
    # in a glist.
    else{
        # Draw the layered label texts
        group_labels <- do.call(grid::gList, mapply(function(labels, x_positions, y_positions, i){
            grid::textGrob(label = labels,
                           x     = x_positions,
                           y     = y_positions,
                           just  = label_just,
                           name  = paste0("group_labels_", i),
                           gp    = grid::gpar(col        = visuals[["variable_axes_font_color"]],
                                              fontfamily = visuals[["font"]],
                                              fontsize   = dimensions[["axes_font_size"]],
                                              fontface   = visuals[["axes_font_face"]],
                                              lineheight = fine_tuning[["line_height"]]))
        }, labels, label_x_positions, label_y, seq_along(labels), SIMPLIFY = FALSE))

        # Draw the separation lines
        separation_lines <- do.call(grid::gList, mapply(function(x_positions, y_positions, i){
            # Determine if this is the first (major) or subsequent (minor) pass
            type_prefix <- "minor_"

            if (i == 1){
                type_prefix <- "major_"
            }

            # Construct the actual separation lines
            grid::segmentsGrob(x0 = x_positions,
                               x1 = x_positions,
                               y0 = y_positions[1],
                               y1 = y_positions[2],
                               name  = paste0("separation_line_", i),
                               gp = grid::gpar(col = visuals[[paste0(type_prefix,"separation_line_color")]],
                                               lty = visuals[[paste0(type_prefix,"separation_line_type")]],
                                               lwd = dimensions[["separation_line_thickness"]]))
            }, group_separation_lines_x, separation_lines_y, seq_along(group_separation_lines_x), SIMPLIFY = FALSE))

        # Return the whole axes as one graphical object
        grid::gTree(children = grid::gList(guiding_lines, line, ticks, group_labels, separation_lines),
                    name     = "x_axes")
    }
}


#' @description
#' [setup_xy_axes()]: Uses the currently active viewport to set up both the x and
#' y axes.
#'
#' @rdname axes
#'
#' @export
setup_xy_axes <- function(diagram_info,
                          arguments){
    axes_x <- setup_x_axes(diagram_info, arguments)

    axes_y_primary <- setup_y_axes(diagram_info[["primary_y_tick_pos"]],
                                   diagram_info[["primary_y_values"]],
                                   arguments)

    grid::gTree(children = grid::gList(axes_x, axes_y_primary), name = "xy_axes")
}


#' @description
#' [inject_inner_canvas_size()]: Injects the inner canvas size, meaning diagram size
#' minus axes size, into the dimensions parameter.
#'
#' @param axes The axes gTree generated by the setup axes functions.
#'
#' @return
#' [inject_inner_canvas_size()]: Returns the argument list with injected inner canvas
#' sizes.
#'
#' @rdname axes
#'
#' @export
inject_inner_canvas_size <- function(axes, arguments){
    dimensions <- arguments[["dimensions"]]

    # Measure the y axes width of each children element and get the sum of all elements
    # as well as the smaller inner canvas width
    y_widths <- vapply(axes[["children"]][["y_axes"]][["children"]], function(child_grob){
            grid::convertWidth(grid::grobWidth(child_grob), "cm", valueOnly = TRUE)
        },
        numeric(1))

    dimensions[["y_axes_width"]]       <- collapse::fsum(y_widths)
    dimensions[["inner_canvas_width"]] <- dimensions[["diagram_width"]] - dimensions[["y_axes_width"]]

    # Measure the x axes height of each children element and get the sum of all elements
    # as well as the smaller inner canvas height
    x_heights <- vapply(axes[["children"]][["x_axes"]][["children"]], function(child_grob){
            if (grepl("^(x_|group_labels)", child_grob[["name"]])) {
                grid::convertHeight(grid::grobHeight(child_grob), "cm", valueOnly = TRUE)
            }
            else{
                0
            }
        },
        numeric(1))

    dimensions[["x_axes_height"]]       <- collapse::fsum(x_heights)
    dimensions[["inner_canvas_height"]] <- dimensions[["diagram_height"]] - dimensions[["x_axes_height"]]

    # Return arguments with injected values
    arguments[["dimensions"]] <- dimensions

    arguments
}


###############################################################################
# Segment labels and legend
###############################################################################
#' Generate Segment Labels And Legends
#'
#' @name segment_labels
#'
#' @description
#' [direct_vertical_labels()]: Set up segment labels which are connected directly
#' to the segments by a line.
#'
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#' @param arguments Argument list passed passed on by [design_graphic()].
#'
#' @return
#' [direct_vertical_labels()]: Returns a grid::gList.
#'
#' @rdname segment_labels
#'
#' @export
direct_vertical_labels <- function(diagram_info,
                                   arguments){
    visuals     <- arguments[["visuals"]]
    dimensions  <- arguments[["dimensions"]]
    fine_tuning <- arguments[["fine_tuning"]]

    if (visuals[["label_type"]] != "lines"){
        return(grid::nullGrob())
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Get base group measurements
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Look up which group of segments gets the labels. If desired group is out of
    # bounds, set the group to one of the extreme points.
    label_group  <- visuals[["label_group"]]
    group_ids_up <- diagram_info[["group_ids"]] + 1

    if (label_group == "auto"){
        # Get middle group
        label_group <- ceiling(collapse::fmax(group_ids_up) / 2)
    }
    else if (label_group < 1){
        label_group <- 1
    }
    else if (label_group > collapse::fmax(group_ids_up)){
        label_group <- collapse::fmax(group_ids_up)
    }

    # Setup empty graphics list
    label_grobs <- grid::gList()

    # Get all the group elements on which to draw the lines with labels
    label_group_selection <- data.table::fifelse(group_ids_up == label_group, TRUE, FALSE)

    # Get the middle points of the vertical bars
    segment_centers_x <- (diagram_info[["segment_pos"]][label_group_selection]
                       + (diagram_info[["segment_width"]] * 0.5))

    # Set a factor for the drawing direction. Normally everything is drawn up, but
    # if there are negative values and the zero axes line is far up, the drawing
    # direction will be turned around. Therefor a reverse factor is needed.
    drawing_direction  <- 1
    vertical_alignment <- 0 # bottom

    if (diagram_info[["zero_pos"]] > fine_tuning[["swap_direction_threshold"]]){
        drawing_direction  <- -1
        vertical_alignment <- 1 # top
    }

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Set up segment lines
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Get starting y, which is the height of the segments measured from the y axes
    # zero position.
    line_length <- convert_to_native_height(dimensions[["segment_line_length"]],
                                            diagram_info,
                                            dimensions)

    values_in_group <- diagram_info[["values"]][label_group_selection]
    segment_start_y <- data.table::fifelse(values_in_group * drawing_direction < 0, 0, values_in_group)

    # Generate a static offset for the lines so that they are a bit apart from the
    # segments and labels.
    offset_y <- convert_to_native_height(fine_tuning[["segment_line_offset"]],
                                         diagram_info,
                                         dimensions) * drawing_direction

    # Generate a variable offset according to the text size of the displayed values.
    # If no values are displayed, the offset is 0 and only the static offset counts.
    offset_value <- 0

    if (visuals[["display_values"]]){
        # Without rotation the offset is basically static because values are always
        # drawn on one line and therefor have the same height.
        if (!visuals[["rotate_values"]]){
            offset_value <- get_values_height(diagram_info,
                                              dimensions,
                                              visuals,
                                              fine_tuning)

            offset_value <- rep(offset_value, diagram_info[["number_of_segments"]])
        }
        # With rotation the values can be unequaly high. The offset therefor must
        # be calculated individually.
        else{
            offset_value <- diagram_info[["values_width"]]

            # The width measuring needs to be swapped to the height dimension.
            offset_value <- swap_xy_scaling(offset_value, dimensions, part = "inner_canvas")

            # Select label group and scale offset to y axes distance
            offset_value <- offset_value[group_ids_up == label_group] * diagram_info[["primary_y_distance"]]

            # Add additional offset for the space between segment and value
            #offset_value <- offset_value + (fine_tuning[["values_zero_line_offset"]] * offset_value)
        }

        # Reset offset, if values fit inside the segments, because then only the
        # static offset counts.
        values_fit_inside <- diagram_info[["values_fit_vertical"]][group_ids_up == label_group]
        offset_value[values_fit_inside] <- 0
    }

    # In case segment lines start at the y axes, the value offset has to be reset
    if (drawing_direction > 0){
        offset_value <- data.table::fifelse(values_in_group < 0, 0, offset_value)
        max_value    <- diagram_info[["primary_y_max"]]
    }
    else{
        offset_value <- data.table::fifelse(values_in_group > 0, 0, offset_value * drawing_direction)
        max_value    <- diagram_info[["primary_y_min"]]
    }

    # Apply offsets to segment start
    segment_start_y <- segment_start_y + (offset_y * 1.4) + offset_value

    # Get the ending y, which is a fixed height from the segments. The maximum height
    # is right below the highest axes value.
    segment_end_y <- collapse::fmin(c(collapse::fmax(abs(segment_start_y) + line_length),
                                      abs(max_value) * fine_tuning[["segment_line_treshhold"]])) * drawing_direction

    # Draw lines in stairs
    line_stairs   <- seq(0, by = dimensions[["segment_line_offset"]], length.out = length(segment_start_y))
    segment_end_y <- rep(segment_end_y, length(segment_start_y)) - line_stairs

    # Put together the vector containing start and end points for the lines.
    # In addition double up the x segment centers to match the length of the y vector.
    line_vector   <- as.vector(rbind(segment_start_y, segment_end_y))
    center_vector <- rep(segment_centers_x, each = 2)

    # To be able to draw each line as a separate line, the points inside the line_vector
    # need to receive an id. Meaning each point pair gets the same id to be identified
    # as two points of the same line by the grob function.
    line_ids <- rep(seq_along(segment_centers_x), each = 2)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Generate graphical objects
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Set the horizontal label alignment according to whether the segment lines are
    # drawn on equal heights or in stairs.
    horizontal_alignment <- 0.5 # center

    if (dimensions[["segment_line_offset"]] > 0){
        horizontal_alignment <- 0 # left
        segment_centers_x    <- segment_centers_x * (1 - fine_tuning[["segment_label_hjust"]])
    }
    else if (dimensions[["segment_line_offset"]] < 0){
        horizontal_alignment <- 1 # right
        segment_centers_x    <- segment_centers_x * (1 + fine_tuning[["segment_label_hjust"]])
    }
    else{
        segment_centers_x <- decollide_labels(diagram_info[["wrapped_segment_labels"]],
                                              segment_centers_x,
                                              dimensions,
                                              fine_tuning)
    }

    # Generate the lines
    lines <- grid::polylineGrob(x    = grid::unit(center_vector, "npc"),
                                y    = grid::unit(line_vector, "native"),
                                id   = line_ids,
                                name = "segment_lines",
                                gp   = grid::gpar(col = visuals[["segment_line_color"]],
                                                  lty = visuals[["segment_line_type"]],
                                                  lwd = dimensions[["segment_line_thickness"]]))

    # Generate the labels on top of the lines
    segment_labels <- grid::textGrob(label = diagram_info[["wrapped_segment_labels"]],
                                     x     = grid::unit(segment_centers_x, "npc"),
                                     y     = grid::unit(segment_end_y + offset_y, "native"),
                                     hjust = horizontal_alignment,
                                     vjust = vertical_alignment,
                                     name  = "segment_labels",
                                     gp    = grid::gpar(col        = visuals[["label_font_color"]],
                                                        fontfamily = visuals[["font"]],
                                                        fontsize   = dimensions[["label_font_size"]],
                                                        fontface   = visuals[["label_font_face"]],
                                                        lineheight = fine_tuning[["line_height"]]))

    # Return whole label object
    grid::gList(lines, segment_labels)
}


###############################################################################
# Conversion
###############################################################################
#' Convert Values Into Native Units
#'
#' @name unit_conversion
#'
#' @description
#' Native units are scaled from 0 to 1 and take to individual viewport and axes
#' scaling into account.
#'
#' [convert_to_native_width()]: Convert a value into a native width unit.
#'
#' @param value The value to convert into native units.
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#' @param dimensions qol package dimensions options.
#' @param base "diagram" by default. Can be either "diagram" or "graphic", depending on
#' what part of the graphic the value should be measured.
#'
#' @return
#' Returns numeric value scaled to native units.
#'
#' @rdname unit_conversion
#'
#' @export
convert_to_native_width <- function(value,
                                    diagram_info,
                                    dimensions,
                                    base       = "diagram"){
    # Get measuring base
    base <- tolower(base)

    if (!tolower(base) %in% c("diagram", "graphic")){
        base <- "diagram"
    }

    # Get the total axes distance to get the scaling right, if scaling is applied
    distance <- 1

    if ("primary_x_distance" %in% names(diagram_info)){
        distance <- diagram_info[["primary_x_distance"]]
    }

    # Convert value into native units
    value * distance / dimensions[[paste0(base, "_width")]]
}


#' @description
#' [convert_to_native_height()]: Convert a value into a native height unit.
#'
#' @rdname unit_conversion
#'
#' @export
convert_to_native_height <- function(value,
                                     diagram_info,
                                     dimensions,
                                     base       = "diagram"){
    # Get measuring base
    base <- tolower(base)

    if (!tolower(base) %in% c("diagram", "graphic")){
        base <- "diagram"
    }

    # Get the total axes distance to get the scaling right, if scaling is applied
    distance <- 1

    if ("primary_y_distance" %in% names(diagram_info)){
        distance <- diagram_info[["primary_y_distance"]]
    }

    # Convert value into native units
    value * distance / dimensions[[paste0(base, "_height")]]
}


###############################################################################
# Other
###############################################################################
# These are the parameters passed to the main diagram functions by design_graphic.
# If they are not present inside a main diagram function, this function should not
# execute.
expected_parameters <- c("graphic_tab",
                         "axes_vars",
                         "segment_vars",
                         "values",
                         "statistics",
                         "var_labels",
                         "stat_labels",
                         "color_theme",
                         "color_usage",
                         "visuals",
                         "axes",
                         "dimensions",
                         "title_height",
                         "footnote_height",
                         "origin_height")


#' Format Values
#'
#' @name format_values
#'
#' @description
#' [format_values()]: Format values to output numbers in a certain way.
#'
#' @param values Single value or vector of values.
#' @param decimals Number of decimals to be displayed.
#' @param big_mark Character used as the thousand separator.
#' @param decimal_mark Character used as the decimal separator.
#' @param prefix What to put in front of the value.
#' @param suffix What to put after the value.
#' @param scale Multiplier for values.
#'
#' @return
#' Returns a formatted value as character.
#'
#' @rdname format_values
#'
#' @export
format_values <- function(values,
                          decimals     = .qol_options[["graphic_axes"]][["primary_axes_decimals"]],
                          big_mark     = .qol_options[["graphic_axes"]][["primary_axes_big_mark"]],
                          decimal_mark = .qol_options[["graphic_axes"]][["primary_axes_decimal_mark"]],
                          prefix       = .qol_options[["graphic_axes"]][["primary_axes_prefix"]],
                          suffix       = .qol_options[["graphic_axes"]][["primary_axes_suffix"]],
                          scale        = .qol_options[["graphic_axes"]][["primary_axes_scale"]]){
    paste0(prefix,
           formatC(values * scale,
                   format       = "f",
                   digits       = decimals,
                   big.mark     = big_mark,
                   decimal.mark = decimal_mark),
           suffix)
}


#' Format Values
#'
#' @description
#' [format_diagram_values()]: A wrapper to make the code for formatting segment
#' values shorter.
#'
#' @param diagram_info The list of measurements generated by [get_diagram_dimensions()].
#' @param arguments Argument list passed passed on by [design_graphic()].
#' @param which Primary or secondary axes.
#'
#' @return
#' Returns a formatted value as character.
#'
#' @export
format_diagram_values <- function(diagram_info,
                                  arguments,
                                  which = "primary"){
    values <- diagram_info[["values"]]
    axes   <- arguments[["axes"]]

    # Look up whether there a statistic labels provided. If this is the case then
    # insert the label behind the values. In this case the suffix specified in the
    # axes parameter is overwritten. This version is needed for multiple value variables
    # in combined diagrams.
    suffix <- axes[[paste0(which, "_values_suffix")]]

    for (value_type in diagram_info[["value_types"]]){
        if (value_type %in% names(arguments[["stat_labels"]])){
            # TODO: MULTIPLE VALUE VARIABLE HANDLING NOT INCLUDED AT THE MOMENT
            suffix <- arguments[["stat_labels"]][[value_type]]
        }
    }

    # Format numeric values first
    values <- format_values(values       = diagram_info[["values"]],
                            decimals     = axes[[paste0(which, "_values_decimals")]],
                            big_mark     = axes[[paste0(which, "_values_big_mark")]],
                            decimal_mark = axes[[paste0(which, "_values_decimal_mark")]],
                            prefix       = axes[[paste0(which, "_values_prefix")]],
                            suffix       = suffix,
                            scale        = axes[[paste0(which, "_axes_scale")]])

    # Convert short minus symbol with long one for better readability
    values <- sub("-", "–", values)

    # Display the + symbol in front of positive values, if specified
    if (arguments[["visuals"]][["display_plus_symbol"]]){
        values[values > 0] <- paste0("+", values[values > 0])
    }

    values
}
