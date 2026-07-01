###############################################################################
# Vertical bars
###############################################################################
#' Generate The Main Diagram Area
#'
#' @name main_diagram
#'
#' @description
#' The listed functions are the built in diagram types which ship with this package.
#' These functions don't work on their own and have to be plugged into the "diagram"
#' parameter of [design_graphic()].
#'
#' [dg_vbars()]: Grouped vertical bars.
#'
#' @param ... Arguments passed on by [design_graphic()].
#'
#' @return
#' Returns a grid::gTree object.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' # Formats
#' age. <- discrete_format(
#'     "Total"          = 0:100,
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = 25:54,
#'     "55 to under 65" = 55:64,
#'     "65 and older"   = 65:100)
#'
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Design grouped vertical bar chart
#' qol_graphic <- my_data |>
#'      design_graphic(axes_variables = "sex",
#'                     segments       = "age",
#'                     values         = weight,
#'                     diagram        = dg_vbars,
#'                     formats        = list(sex = sex.,
#'                                           age = age.))
#'
#' @rdname main_diagram
#'
#' @export
dg_vbars <- function(...){
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

    # Set up segment labels
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
