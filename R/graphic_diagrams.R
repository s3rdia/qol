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
        message(" X ERROR: Diagram function doesn't work on it's own. It can only be used\n",
                "          as the <diagram> parameter in <design_graphic>.")

        return(invisible(grid::nullGrob()))
    }

    # Get the colors to be used
    theme <- get_theme_colors(arguments[["colors"]])

    # Set up the whole diagram as well as the inner diagram viewport. Additionally
    # retrieve the calculated measurements.
    segment_info <- setup_nested_diagram_viewport(arguments)

    # Generate segments
    segments <- vbar_grob(segment_info, arguments, theme)

    # Generate axes
    axes <- setup_xy_axes(segment_info, arguments)

    segment_labels <- direct_vertical_labels(segment_info, arguments)

    # Combine all elements into one graphical object
    list(graphic = grid::gTree(children = grid::gList(segments, axes, segment_labels)),
         meta    = segment_info)
}
