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
                        x_pos      = .qol_options[["graphic_dimensions"]][["margins"]],
                        y_pos      = .qol_options[["graphic_dimensions"]][["margins"]],
                        width      = .qol_options[["graphic_dimensions"]][["graphic_width"]],
                        alignment  = .qol_options[["graphic_visuals"]][["other_alignment"]],
                        font       = .qol_options[["graphic_visuals"]][["font"]],
                        font_color = .qol_options[["graphic_visuals"]][["other_font_color"]],
                        font_size  = .qol_options[["graphic_dimensions"]][["other_font_size"]],
                        font_face  = .qol_options[["graphic_visuals"]][["other_font_face"]],
                        name       = "textbox",
                        draw       = FALSE){
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
                                                     lineheight = 1.1))
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
                                                     lineheight = 1.1))
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
                                                 lineheight = 1.1))
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
#'
#' @rdname textboxes
#'
#' @export
add_title <- function(text,
                      dimensions = .qol_options[["graphic_dimensions"]],
                      visuals    = .qol_options[["graphic_visuals"]],
                      draw       = FALSE){
    x_pos <- fix_alignment(dimensions, visuals)

    invisible(
        add_textbox(text       = text,
                    x_pos      = x_pos,
                    y_pos      = dimensions[["graphic_height"]] - dimensions[["margins"]],
                    width      = get_available_width(dimensions),
                    alignment  = visuals[["title_alignment"]],
                    font       = visuals[["font"]],
                    font_color = visuals[["title_font_color"]],
                    font_size  = dimensions[["title_font_size"]],
                    font_face  = visuals[["title_font_face"]],
                    name       = "title",
                    draw       = draw))
}


#' @description
#' [add_footnote()]: Create a textbox as graphical object. A wrapper to easily create
#' a textbox at the bottom of the graphic as footnote.
#'
#' @rdname textboxes
#'
#' @export
add_footnote <- function(text,
                         dimensions = .qol_options[["graphic_dimensions"]],
                         visuals    = .qol_options[["graphic_visuals"]],
                         draw       = FALSE){
    x_pos <- fix_alignment(dimensions, visuals, "footnote")

    # Add textbox as normal first
    footnote <- add_textbox(text       = text,
                            x_pos      = x_pos,
                            y_pos      = dimensions[["margins"]],
                            width      = get_available_width(dimensions),
                            alignment  = visuals[["footnote_alignment"]],
                            font       = visuals[["font"]],
                            font_color = visuals[["footnote_font_color"]],
                            font_size  = dimensions[["footnote_font_size"]],
                            font_face  = visuals[["footnote_font_face"]],
                            name       = "footnote")

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
#' @param background_color Hex color code of background the rectangle.
#' @param border_color Hex color code of border around the background rectangle.
#' @param name The internal name of the canvas with which it can be identified.
#'
#' @return
#' Returns a grid::viewport object.
#'
#' @rdname viewport
#'
#' @export
setup_main_canvas <- function(width  = .qol_options[["graphic_dimensions"]][["graphic_width"]],
                              height = .qol_options[["graphic_dimensions"]][["graphic_height"]],
                              background_color = .qol_options[["graphic_visuals"]][["graphic_background_color"]],
                              border_color     = .qol_options[["graphic_visuals"]][["graphic_border_color"]],
                              name   = "main_canvas"){
    grid::grid.newpage()

    # Set up the main viewport for the entire graphic
    vp <- grid::viewport(width  = grid::unit(width, "cm"),
                         height = grid::unit(height, "cm"),
                         xscale = c(0, width),
                         yscale = c(0, height),
                         gp     = grid::gpar(lineheight = 1.2),
                         name   = name)

    grid::pushViewport(vp)

    # Draw the graphics background
    grid::grid.rect(gp = grid::gpar(fill = background_color,
                                    col  = border_color))

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
setup_nested_viewport <- function(x_pos   = 0,
                                  y_pos   = 0,
                                  y_scale = c(0, 1),
                                  width   = .qol_options[["graphic_dimensions"]][["graphic_width"]],
                                  height  = .qol_options[["graphic_dimensions"]][["graphic_height"]],
                                  background_color = .qol_options[["graphic_visuals"]][["diagram_background_color"]],
                                  border_color     = .qol_options[["graphic_visuals"]][["diagram_border_color"]],
                                  name = "nested_viewport"){
    # Set up a new nested viewport.
    vp <- grid::viewport(x      = grid::unit(x_pos, "native"),
                         y      = grid::unit(y_pos, "native"),
                         width  = grid::unit(width, "native"),
                         height = grid::unit(height, "native"),
                         yscale = y_scale,
                         just   = c("left", "top"),
                         gp     = grid::gpar(lineheight = 1.2),
                         name   = name)

    grid::pushViewport(vp)

    # Draw the graphics background
    grid::grid.rect(gp = grid::gpar(fill = background_color,
                                    col  = border_color))

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
    # Calculate new diagram viewport y starting position
    # HACK: Using one additional margin here because somehow the height measuring
    #       of multiline text doesn't output the correct values.
    valid_heights <- sum(arguments[["title_height"]] > 0)
    y_pos         <- (arguments[["dimensions"]][["graphic_height"]]
                   - (arguments[["title_height"]]
                   + (arguments[["dimensions"]][["margins"]] * (2 + valid_heights))))

    # Calculate new diagram viewport height
    valid_heights <- sum(arguments[["title_height"]]    > 0,
                         arguments[["footnote_height"]] > 0)

    # HACK: Using two additional margins here because somehow the height measuring
    #       of multiline text doesn't output the correct values.
    height <- (arguments[["dimensions"]][["graphic_height"]]
             - arguments[["title_height"]]
             - arguments[["footnote_height"]]
             - (arguments[["dimensions"]][["margins"]] * (4 + valid_heights)))

    # Set up a new viewport for the whole diagram area to be able to safely work
    # in this area.
    setup_nested_viewport(x_pos   = arguments[["dimensions"]][["margins"]],
                          y_pos   = y_pos,
                          y_scale = c(0, 1),
                          width   = arguments[["dimensions"]][["graphic_width"]]
                                 - (arguments[["dimensions"]][["margins"]] * 2),
                          height  = height,
                          background_color = arguments[["visuals"]][["diagram_background_color"]],
                          border_color     = arguments[["visuals"]][["diagram_border_color"]],
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
    segment_info <- arguments[["graphic_tab"]] |>
        get_diagram_dimensions(arguments[["axes_vars"]],
                               arguments[["segment_vars"]],
                               arguments[["values"]],
                               arguments[["axes"]],
                               arguments[["dimensions"]],
                               arguments[["visuals"]])

    segment_info[["outer_viewport"]] <- outer_viewport

    # Setup the main inner diagram viewport
    segment_info[["inner_viewport"]] <-
        setup_nested_viewport(x_pos   = segment_info[["primary_y_axes_width"]],
                              y_pos   = grid::unit(1, "native"),
                              y_scale = c(segment_info[["primary_y_min"]], segment_info[["primary_y_max"]]),
                              width   = grid::convertUnit(grid::unit(1.0, "npc"), "native", valueOnly = TRUE)
                                      - segment_info[["primary_y_axes_width"]],
                              height           = 1 - segment_info[["group_label_height"]],
                              background_color = arguments[["visuals"]][["diagram_background_color"]],
                              name             = "main_diagram")

    invisible(segment_info)
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
#' @param graphic_tab The base data frame for the graphic.
#' @param axes_vars The names of the axes variables.
#' @param segment_vars The names of the segment variables.
#' @param values Value vector from the data frame.
#' @param axes The list of axes parameters.
#' @param dimensions The list of dimensions parameters.
#' @param visuals The list of visual parameters.
#'
#' @return
#' Returns a list of parameters for groups, segments and axes.
#'
#' @rdname diagram
#'
#' @export
get_diagram_dimensions <- function(graphic_tab,
                                   axes_vars,
                                   segment_vars,
                                   values,
                                   axes       = .qol_options[["graphic_axes"]],
                                   dimensions = .qol_options[["graphic_dimensions"]],
                                   visuals    = .qol_options[["graphic_visuals"]]){
    # TODO: WHAT IF MULTIPLE VARIABLES ARE PASSED OR "age + sex" COMBINATIONS?

    # Get the unique values of the axes and segment variables to determine their
    # actual numbers. This is needed to measure the space each group and each
    # segment takes.
    unique_groups      <- graphic_tab[[axes_vars]] |> as.character() |> collapse::funique()
    number_of_groups   <- length(unique_groups)
    unique_segments    <- graphic_tab[[segment_vars]] |> as.character() |> collapse::funique()
    number_of_segments <- length(unique_segments)
    number_of_elements <- number_of_groups * number_of_segments

    # Actual space calculation. Add margins for the whole graphic and the individual
    # groups to give everything a bit air to breathe.
    margin        <- 0.01
    group_width   <- 1 / number_of_groups
    segment_width <- (group_width - (margin * 2)) / number_of_segments

    # First get the group numbers zero based, then the segment numbers within
    # each group also zero based. Finally calculate all starting x positions of
    # each segment.
    running_nr  <- seq_len(number_of_elements) - 1
    group_ids   <- floor(running_nr / number_of_segments)
    segment_ids <- running_nr %% number_of_segments
    segment_pos <- margin + (group_ids * group_width) + (segment_ids * segment_width)

    # Get the values and tick positions for the y axes, which is basically an even
    # distribution.
    primary_y_values   <- get_y_axes_values(graphic_tab[[values]], axes)
    #secondary_y_values <- get_y_axes_values(values, axes, "secondary")

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
            actual_drawing_height <- graphic_tab[[values]] - primary_y_max
        }
        else{
            actual_drawing_height <- graphic_tab[[values]]
        }
    }
    else{
        actual_drawing_height <- graphic_tab[[values]] - primary_y_min
    }

    # Get width of the highest value for the axes to measure the actual space the
    # whole axes needs in the diagram.
    if (abs(primary_y_max) > abs(primary_y_min)){
        primary_y_axes_width <- graphic_tab |>
            get_value_axes_width(primary_y_max, axes, dimensions, visuals)
    }
    # In case of negative value having more digits
    else{
        primary_y_axes_width <- graphic_tab |>
            get_value_axes_width(primary_y_min, axes, dimensions, visuals)
    }
    # TODO: CONDITIONALLY DECIDE WHEN TO DO THE SECONDARY AXES.

    # Calculate the positions of the group labels
    group_label_pos <- (collapse::funique(group_ids) * group_width) + (group_width * 0.5)

    # Format group labels on variable x axes
    wrapped_group_labels <- wrap_text_vector(unique_groups,
                                             group_width - (margin * 2),
                                             visuals[["font"]],
                                             dimensions[["axes_font_size"]],
                                             visuals[["axes_font_face"]])

    # Get dimensions of group labels
    group_label_height <- get_variable_axes_dimension(wrapped_group_labels, dimensions, visuals)

    # Get tick positions
    group_ticks_pos_x <- get_group_tick_positions_x(number_of_groups, number_of_segments,
                                                    segment_width, segment_pos)

    # Format segment labels
    # TODO: USE VARIABLE LABEL WIDTH INSTEAD OF GROUP WIDTH
    wrapped_segment_labels <- wrap_text_vector(unique_segments,
                                               group_width - (margin * 2),
                                               visuals[["font"]],
                                               dimensions[["axes_font_size"]],
                                               visuals[["axes_font_face"]])
    # Return information as list
    list(unique_groups          = unique_groups,
         number_of_groups       = number_of_groups,
         unique_segments        = unique_segments,
         number_of_segments     = number_of_segments,
         number_of_elements     = number_of_elements,
         group_width            = group_width,
         segment_width          = segment_width,
         running_nr             = running_nr,
         group_ids              = group_ids,
         segment_ids            = segment_ids,
         segment_pos            = segment_pos,
         primary_y_values       = primary_y_values,
         primary_y_tick_width   = primary_y_tick_width,
         primary_y_tick_pos     = primary_y_tick_pos,
         primary_y_max          = primary_y_max,
         primary_y_min          = primary_y_min,
         primary_y_distance     = primary_y_distance,
         zero_pos               = zero_pos,
         actual_drawing_height  = actual_drawing_height,
         primary_y_axes_width   = primary_y_axes_width,
         group_label_pos        = group_label_pos,
         wrapped_group_labels   = wrapped_group_labels,
         group_label_height     = group_label_height,
         group_ticks_pos_x      = group_ticks_pos_x,
         wrapped_segment_labels = wrapped_segment_labels)
}


#' @description
#' [vbar_grob()]: Set up the main segments for vertical bars.
#'
#' @param segment_info The list of measurements generated by
#' [setup_nested_diagram_viewport()].
#' @param arguments Argument list passed passed on by [design_graphic()].
#' @param theme Color theme used for the segments.
#'
#' @return
#' Returns a grid::rectGrob object.
#'
#' @rdname diagram
#'
#' @export
#'
vbar_grob <- function(segment_info,
                      arguments,
                      theme){
    grid::rectGrob(x      = grid::unit(segment_info[["segment_pos"]], "native"),
                   y      = segment_info[["zero_pos"]],
                   width  = grid::unit(segment_info[["segment_width"]], "native"),
                   height = grid::unit(segment_info[["actual_drawing_height"]], "native"),
                   just   = c("left", "bottom"),
                   gp     = grid::gpar(fill = theme[segment_info[["segment_ids"]] + 1],
                                       col  = arguments[["visuals"]][["segment_border_color"]]))
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

    tick_width <- grid::convertWidth(grid::unit(0.5, "lines"), "native", valueOnly = TRUE)

    # Create test graphical object to measure the actual width taken
    temp_grob <- grid::textGrob(axes_max,
                                gp = grid::gpar(fontfamily = visuals[["font"]],
                                                fontsize   = dimensions[["axes_font_size"]],
                                                fontface   = visuals[[paste0(which, "_axes_font_face")]]))

    # Return width
    grid::convertWidth(grid::grobWidth(temp_grob), "native", valueOnly = TRUE) + tick_width
}


#' @description
#' [get_variable_axes_dimension()]: Get the height of the variable axes.
#'
#' @param wrapped_text The wrapped text for the variable axes to measure.
#'
#' @return
#' [get_variable_axes_dimension()]: Returns a numeric height.
#'
#' @rdname axes
#'
#' @export
get_variable_axes_dimension <- function(wrapped_text,
                                        dimensions,
                                        visuals){
    # Create test graphical object to measure the actual width taken
    temp_grob <- grid::textGrob(wrapped_text,
                                gp = grid::gpar(fontfamily = visuals[["font"]],
                                                fontsize   = dimensions[["axes_font_size"]],
                                                fontface   = visuals[["variable_axes_font_face"]],
                                                lineheight = 1.2))

    # Measure height by manual calculation because the grobHeight comes out way
    # too small.
    height_cm <- (grid::convertHeight(grid::stringHeight(temp_grob[["label"]]), "cm", valueOnly = TRUE)
                  / dimensions[["graphic_height"]])

    collapse::fmax(height_cm) + 0.02
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
                              which = "primary"){
    which <- tolower(which)

    # Get global values first
    global_min <- axes[[paste0(which, "_axes_min")]]
    global_max <- axes[[paste0(which, "_axes_max")]]

    # Replace auto generated value by statically set global values
    if (global_min != "auto"){
        min_value <- global_min
    }
    # Get values from the actual data frame and add ten percent to the value, so
    # that the highest value doesn't go up to the end of the axes. Leave a little
    # room to breathe.
    else{
        min_value <- collapse::fmin(c(0, values))
        min_value <- min_value + (min_value / 10)
    }

    # Replace auto generated value by statically set global values
    if (global_max != "auto"){
        max_value <- global_max
    }
    # Get values from the actual data frame and add ten percent to the value, so
    # that the highest value doesn't go up to the end of the axes. Leave a little
    # room to breathe.
    else{
        max_value <- collapse::fmax(c(0, values))
        max_value <- max_value + (max_value / 10)
    }

    # Return equally spaced round values in given range
    pretty(c(min_value, max_value), n = axes[[paste0(which, "_axes_steps")]])
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
    which <- "primary"

    if (secondary){
        which <- "secondary"
    }

    # Vertical axes line over the whole viewport
    zero_pos <- sum(secondary)

    line <- grid::linesGrob(x = c(zero_pos, zero_pos), y = c(0, 1))

    # Setup the ticks left/right
    tick_length <- 0.1 + (-0.2 * zero_pos)

    ticks <- grid::segmentsGrob(x0 = zero_pos,
                                x1 = grid::unit(zero_pos, "native") - grid::unit(tick_length, "cm"),
                                y0 = grid::unit(tick_positions, "npc"),
                                y1 = grid::unit(tick_positions, "npc"))

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

    # Insert the group labels for the variable axes
    label_offset <- -0.02 + (0.04 * zero_pos)

    axes_values <- grid::textGrob(label = formatted_tick_values,
                                  x     = label_offset,
                                  y     = value_positions,
                                  just  = c("right", "center"),
                                  gp    = grid::gpar(fontfamily = arguments[["visuals"]][["font"]],
                                                     fontsize   = arguments[["dimensions"]][["axes_font_size"]],
                                                     fontface   = arguments[["visuals"]][["axes_font_face"]]))

    # Return the whole axes as one graphical object
    grid::gTree(children = grid::gList(line, ticks, axes_values))
}


#' @description
#' [setup_x_axes()]: Uses the currently active viewport to set up the x axes. The
#' height at which it is drawn is determined by the primary y axes 0 point. If y0
#' is not on the y axes then the x axes will either be drawn at the bottom, if only
#' positive values are present, or at the top, if only negative values are present.
#'
#' @param label_positions The positions the labels will be drawn at.
#' @param labels The labels per tick or per segment group.
#' @param zero_pos The y axes 0 position at which the x axes is drawn.
#'
#' @rdname axes
#'
#' @export
setup_x_axes <- function(tick_positions,
                         label_positions,
                         labels,
                         zero_pos,
                         arguments){
    tick_length <- 0.02

    # Ticks point up if the x axes is drawn at the top.
    # TODO: IN THIS CASE THE VIEWPORT SHOULD BE SET UP DIFFERENTLY BEFOREHAND.
    #       LABELS SHOULD BE DRAWN AT THE TOP.
    if (zero_pos == 1){
        tick_length <- -0.02
    }

    # Horizontal axes line over the whole viewport. The axes will be drawn at the 0
    # position of the primary y axes if it is there.
    line <- grid::linesGrob(x = c(0, 1), y = c(zero_pos, zero_pos))

    # Setup the ticks pointing down
    ticks <- grid::segmentsGrob(x0 = grid::unit(tick_positions, "native"),
                                x1 = grid::unit(tick_positions, "native"),
                                y0 = zero_pos,
                                y1 = zero_pos - tick_length)

    # Insert the group labels for the variable axes
    group_labels <- grid::textGrob(label = labels,
                                   x     = label_positions,
                                   y     = -0.02,
                                   just  = c("center", "top"),
                                   gp    = grid::gpar(fontfamily = arguments[["visuals"]][["font"]],
                                                      fontsize   = arguments[["dimensions"]][["axes_font_size"]],
                                                      fontface   = arguments[["visuals"]][["axes_font_face"]],
                                                      lineheight = 1.1))

    # Return the whole axes as one graphical object
    grid::gTree(children = grid::gList(line, ticks, group_labels))
}


#' @description
#' [setup_xy_axes()]: Uses the currently active viewport to set up both the x and
#' y axes.
#'
#' @param segment_info The list of measurements generated by
#' [setup_nested_diagram_viewport()].
#'
#' @rdname axes
#'
#' @export
setup_xy_axes <- function(segment_info,
                          arguments){
    axes_x <- setup_x_axes(segment_info[["group_ticks_pos_x"]],
                           segment_info[["group_label_pos"]],
                           segment_info[["wrapped_group_labels"]],
                           segment_info[["zero_pos"]],
                           arguments)

    axes_y_primary <- setup_y_axes(segment_info[["primary_y_tick_pos"]],
                                   segment_info[["primary_y_values"]],
                                   arguments)

    grid::gTree(children = grid::gList(axes_x, axes_y_primary))
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
#' @param segment_info The list of measurements generated by
#' [setup_nested_diagram_viewport()].
#' @param arguments Argument list passed passed on by [design_graphic()].
#'
#' @return
#' [direct_vertical_labels()]: Returns a grid::gList.
#'
#' @rdname segment_labels
#'
#' @export
direct_vertical_labels <- function(segment_info,
                                   arguments){
    if (arguments[["visuals"]][["label_type"]] != "lines"){
        return(grid::nullGrob())
    }

    # Look up which group of segments gets the labels. If desired group is out of
    # bounds, set the group to one of the extreme points.
    label_group  <- arguments[["visuals"]][["label_group"]]
    group_ids_up <- segment_info[["group_ids"]] + 1

    if (label_group < 1){
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
    segment_centers_x <- (segment_info[["segment_pos"]][label_group_selection]
                       + (segment_info[["segment_width"]] * 0.5))

    # Set a factor for the drawing direction. Normally everything is drawn up, but
    # if there are negative values and the zero axes line is far up, the drawing
    # direction will be turned around. Therefor a reverse factor is needed.
    drawing_direction <- 1
    alignment         <- "bottom"

    if (segment_info[["zero_pos"]] > 0.75){
        drawing_direction <- -1
        alignment         <- "top"
    }

    # Get starting y, which is the height of the segments measured from the y axes
    # zero position, and the ending y which is a fixed height from the segments.
    # The maximum height is right below the highest axes value.
    max_value   <- segment_info[["primary_y_distance"]]
    line_length <- grid::convertUnit(grid::unit(arguments[["dimensions"]][["segment_line_length"]], "cm"),
                                     "native", valueOnly = TRUE) * max_value

    segment_start_y <- arguments[["graphic_tab"]][[arguments[["values"]]]][label_group_selection]
    segment_start_y <- data.table::fifelse(segment_start_y * drawing_direction < 0, 0, segment_start_y)
    segment_end_y   <- collapse::fmin(c(collapse::fmax(abs(segment_start_y) + line_length) * drawing_direction,
                                        max_value * 0.95))

    # TODO: VARIABLE HEIGHT + DRAWING IN STEPS
    # TODO: SEPARATE LABELS

    # Generate a static offset for the lines so that they are a bit apart from the
    # segments and labels.
    offset_y <- grid::convertUnit(grid::unit(0.5, "cm"), "native", valueOnly = TRUE) * max_value * drawing_direction

    # Put together the vector containing start and end points for the lines.
    # In addition double up the x segment centers to match the length of the y vector.
    line_vector <- as.vector(rbind(segment_start_y + offset_y,
                                   segment_end_y   - offset_y))
    center_vector <- rep(segment_centers_x, each = 2)

    # To be able to draw each line as a separate line, the points inside the line_vector
    # need to receive an id. Meaning each point pair gets the same id to be identified
    # as two points of the same line by the grob function.
    line_ids <- rep(seq_along(segment_centers_x), each = 2)

    # Generate the lines
    lines <- grid::polylineGrob(x  = grid::unit(center_vector, "npc"),
                                y  = grid::unit(line_vector, "native"),
                                id = line_ids,
                                gp = grid::gpar(col = arguments[["visuals"]][["segment_line_color"]],
                                                lty = arguments[["visuals"]][["segment_line_type"]]))

    # Generate the labels on top of the lines
    segment_labels <- grid::textGrob(label = segment_info[["wrapped_segment_labels"]],
                                     x     = grid::unit(segment_centers_x, "npc"),
                                     y     = grid::unit(segment_end_y, "native"),
                                     just  = c("center", alignment),
                                     gp    = grid::gpar(fontfamily = arguments[["visuals"]][["font"]],
                                                        fontsize   = arguments[["dimensions"]][["label_font_size"]],
                                                        fontface   = arguments[["visuals"]][["label_font_face"]],
                                                        lineheight = 1.1))

    # Return whole label object
    grid::gList(lines, segment_labels)
}


###############################################################################
# Graphic output
###############################################################################
#' Output Graphic To Device
#'
#' @description
#' Draw or output a graphic to a desired destination.
#'
#' @param graphic_object The complete graphic to be drawn or exported.
#' @param dimensions qol package dimensions options.
#' @param output qol package output options.
#'
#' @return
#' Returns the input graphic object.
#'
#' @export
output_graphic <- function(graphic_object,
                           dimensions,
                           output){
    # First check if full file path is provided. If not only draw graphic in plot window.
    if (is.null(output[["save_path"]]) || is.null(output[["file"]])){
        if (interactive()){
            grid::grid.draw(graphic_object)
        }
    }
    else{
        # If save path doesn't exist, just draw graphic in plot window.
        if (!file.exists(output[["save_path"]])){
            message(" ! WARNING: Path does not exist: ", output[["save_path"]])

            if (interactive()){
                grid::grid.draw(graphic_object)
            }
        }
        # Save file
        else{
            # Get file extension to determine output format
            extension <- tolower(tools::file_ext(output[["file"]]))

            # Output into desired format
            if (extension == "png"){
                grDevices::png(paste0(output[["save_path"]], "/", output[["file"]]),
                               width  = dimensions[["width"]]  / 2.54,
                               height = dimensions[["height"]] / 2.54,
                               units  = "in",
                               res    = output[["resolution"]])

                grid::grid.draw(graphic_object)

                grDevices::dev.off()
            }
        }
    }

    invisible(graphic_object)
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
                         "statistics",
                         "var_labels",
                         "stat_labels",
                         "colors",
                         "structure",
                         "visuals",
                         "axes",
                         "dimensions")


#' Format Values
#'
#' @description
#' Format values to output numbers in a certain way.
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
