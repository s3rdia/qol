###############################################################################
# Vertical bars
###############################################################################
#' Generate The Main Graphic Area For VBars
#'
#' @description
#' Generate the main graphic area for vbars
#'
#' @param ... Arguments passed on by [design_graphic()].
#'
#' @return
#' Returns a grid::gTree object.
#'
#' @export
vbars <- function(...){
    # Evaluate the passed arguments first. If this function is not called from inside
    # design_graphic() it will error.
    arguments        <- list(...)
    names(arguments) <- dots_to_char(...)

    if (is.null(arguments) || !all(expected_parameters %in% names(arguments))){
        print_message("ERROR", c("Diagram function doesn't work on it's own. It can only be used",
                                 "as the <diagram> parameter in <design_graphic>."))

        return(invisible(grid::nullGrob()))
    }

    # Set up the whole diagram as well as the inner diagram viewport. Additionally
    # retrieve the calculated measurements.
    diagram_info <- setup_nested_diagram_viewport(arguments)

    # Draw the diagram background
    diagram_background <- grid::rectGrob(gp  = grid::gpar(fill = arguments[["visuals"]][["diagram_background_color"]],
                                                          col  = arguments[["visuals"]][["diagram_border_color"]],
                                                          lwd  = arguments[["dimensions"]][["diagram_outline_thickness"]]),
                                         name = "diagram_background")

    interactive_elements <- setup_interactive_elements(arguments, diagram_info)

    # Generate axes
    axes          <- setup_xy_axes(diagram_info, arguments)
    guiding_lines <- setup_guiding_lines(diagram_info, arguments)
    arguments     <- inject_inner_canvas_size(axes, arguments)

    # Generate segments
    segments <- vbar_grob(diagram_info, arguments)


    segment_labels <- direct_vertical_labels(diagram_info, arguments)

    # Combine all elements into one graphical object
    list(graphic = grid::gTree(children = grid::gList(diagram_background,
                                                      interactive_elements,
                                                      guiding_lines,
                                                      segments,
                                                      axes,
                                                      segment_labels), name = "diagram"),
         meta = diagram_info)
}
