#' Monitor Time Consumption
#'
#' @description
#' The monitor functions offer a simple way to keep track of timings and visualize them in charts.
#' If used throughout a longer syntax it is useful to identify bottlenecks or just get a better feeling
#' which passages take more time than others.
#'
#' @name monitor
#'
#' @param monitor_df A data table in which the delta times with their respective section names are stored.
#' @param section A named section for which to store delta times.
#' @param group Optionally pass a broader group name to be able to plot summarised delta times in addition to the detailed ones.
#' "Total" as default value.
#'
#' @return
#' Returns a small data table with section-, group-names and corresponding delta times.
#'
#' @examples
#' # Example data frame
#' monitor_df <- NULL |> monitor_start("Generate data frame", "Preparation")
#'
#' my_data <- dummy_data(1000)
#'
#' # Formats
#' monitor_df <- monitor_df |> monitor_next("Create formats", "Preparation")
#'
#' age. <- discrete_format(
#'     "Total"          = 0:100,
#'     "under 18"       = 0:17,
#'     "18 to under 25" = 18:24,
#'     "25 to under 55" = 25:54,
#'     "55 to under 65" = 55:65,
#'     "65 and older"   = 65:100)
#'
#' sex. <- discrete_format(
#'     "Total"  = 1:2,
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Evaluations
#' monitor_df <- monitor_df |> monitor_next("Nested summarise", "Summarise")
#'
#' all_nested <- my_data |>
#'     summarise_plus(class      = c(year, sex, age),
#'                    values     = income,
#'                    statistics = c("sum", "pct_group", "pct_total", "sum_wgt", "freq"),
#'                    formats    = list(sex = "sex.", age = "age."),
#'                    weight     = weight,
#'                    nesting    = "deepest",
#'                    na.rm      = TRUE)
#'
#' monitor_df <- monitor_df |> monitor_next("All summarise", "Summarise")
#'
#' all_possible <- my_data |>
#'     summarise_plus(class      = c(year, sex, age),
#'                    values     = c(probability),
#'                    statistics = c("sum", "p1", "p99", "min", "max", "freq", "freq_g0"),
#'                    formats    = list(sex    = "sex.",
#'                                      age    = "age."),
#'                    weight     = weight,
#'                    nesting    = "all",
#'                    na.rm      = TRUE)
#'
#' monitor_df <- monitor_df |> monitor_end()
#'
#' # For detailed plot
#' monitor_df |> monitor_plot()
#'
#' # For summarised plot
#' monitor_df |> monitor_plot(by = "group")
#'
#' # NOTE: The more complex functions in this package have a detailed monitoring
#' #       integrated which can be viewed by setting the argument 'monitor' to TRUE.
#'
#' @rdname monitor
#'
#' @keywords internal
NULL


#' @description
#' [monitor_start()] starts a new timing and adds this as an observation to the monitoring
#' data table. Pass NULL as monitor_df if you call the function for the first time to create a new
#' monitoring data table.
#'
#' @rdname monitor
#'
#' @export
monitor_start <- function(monitor_df, section, group = "Total"){
    if (is.null(monitor_df)){
        monitor_df <- data.table::data.table()
    }

    # Set up starting time in a new row of monitoring data frame
    monitor_df[collapse::fnrow(monitor_df) + 1, "group"]   <- as.character(group)
    monitor_df[collapse::fnrow(monitor_df),     "section"] <- as.character(section)
    monitor_df[collapse::fnrow(monitor_df),     "start"]   <- as.numeric(Sys.time())

    monitor_df
}


#' @description
#' [monitor_end()] ends the current timing and calculates corresponding delta.
#'
#' @rdname monitor
#'
#' @export
monitor_end <- function(monitor_df){
    # Set end time in last timed row
    monitor_df[collapse::fnrow(monitor_df), "end"]   <- as.numeric(Sys.time())
    monitor_df[collapse::fnrow(monitor_df), "delta"] <- monitor_df[collapse::fnrow(monitor_df), "end"] - monitor_df[collapse::fnrow(monitor_df), "start"]

    monitor_df
}


#' @description
#' [monitor_next()] ends the current timing and calculates corresponding delta. In addition directly starts
#' a new timing for a new section.
#'
#' @rdname monitor
#'
#' @export
monitor_next <- function(monitor_df, section, group = "Total"){
    # End current section and directly start new one
    monitor_df <- monitor_df |> monitor_end()
    monitor_df <- monitor_df |> monitor_start(section, group)

    monitor_df
}


#' @description
#' [monitor_plot()] outputs two charts to visualize the saved delta times.
#'
#' @param by Use "section" for a detailed plot and "group" for summarised categories.
#' @param draw_plot Conditionally draw plots. TRUE by default.
#'
#' @rdname monitor
#'
#' @export
monitor_plot <- function(monitor_df, by = "section", draw_plot = TRUE){
    if (!draw_plot){
        return(FALSE)
    }

    old_par <-graphics:: par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))

    monitor_df[monitor_df[["group"]] == "Calc(pseudo_group)", "group"] <- "Calc(total)"

    if (by == "group"){
        label_levels <- monitor_df[["group"]] |>
            unlist(use.names = FALSE) |>
            collapse::funique() |>
            collapse::na_omit()

        monitor_df[["group"]] <- factor(
            monitor_df[["group"]],
            levels = label_levels,
            ordered = TRUE)

        monitor_df <- monitor_df |>
            collapse::fgroup_by("group") |>
            collapse::fsummarise(delta = collapse::fsum(delta))
    }
    else{
        by <- "section"

        label_levels <- monitor_df[["section"]] |>
            unlist(use.names = FALSE) |>
            collapse::funique() |>
            collapse::na_omit()

        monitor_df[["section"]] <- factor(
            monitor_df[["section"]],
            levels = label_levels,
            ordered = TRUE)

        monitor_df <- monitor_df |>
            collapse::fgroup_by("section") |>
            collapse::fsummarise(delta = collapse::fsum(delta))
    }

    # Calculate total time consumption
    total_time <- round(collapse::fsum(monitor_df[["delta"]]), 3)

    # Setup plot dimensions
    graphics::par(las      = 2,             # Vertical labels
                  mfrow    = c(2, 1),       # Two rows, one column on page
                  cex.main = 0.8,           # Smaller font size for title
                  cex.lab  = 0.8,           # Smaller font size for labels
                  cex.axis = 0.6,           # Smaller font size for axis
                  mar      = c(6, 4, 2, 1)) # Margins: bottom, left, top, right

    # Line chart
    plot(monitor_df[["delta"]],                            # Values
         main = paste0("Time Consumption - ", total_time), # Main title
         type = "l",                                       # Line without markers
         xlab = "", ylab = "Seconds",                      # Axis texts
         ylim = c(0, max(monitor_df[["delta"]]) * 1.2),    # y-axis starts from zero up to the highest value
         axes = FALSE,                                     # Removes all lines around diagram
         col  = 4,                                         # Blue color for line
         lwd  = 2)                                         # Line thickness

    # Set up axis
    graphics::axis(side   = 1,                         # x-Axis
                   at     = seq_len(collapse::fnrow(monitor_df)), # How many ticks are shown
                   labels = monitor_df[[by]])          # Labels
    graphics::axis(side   = 2)                         # Show y-Axis as stated in plot

    # Stacked bar chart 100 %
    percentages <- monitor_df[["delta"]] / sum(monitor_df[["delta"]])

    # Setup plot dimensions
    color_palette <- grDevices::rainbow(collapse::fnrow(monitor_df))

    graphics::par(mar = c(5, 1, 2, 1))              # Margins: bottom, left, top, right

    graphics::barplot(as.matrix(percentages),       # Convert to matrix for stacked bars
                      xlab  = "", ylab = "",        # Axis texts
                      axes  = FALSE,                # Removes all lines around diagram
                      horiz = TRUE,                 # Bars
                      col   = color_palette,        # Rainbow colors
                      main = "Time Distribution in Percent") # Different colors

    # Set up legend
    graphics::legend("bottomleft",              # General legend position
                     legend = monitor_df[[by]], # Values
                     fill   = color_palette,    # Different colors
                     cex    = 0.7,              # Smaller font size
                     ncol   = 3,                # Number of horizontal categories
                     inset  = c(0, -0.4),       # Move legend slightly down
                     xpd    = TRUE)             # Legend can be drawn outside th chart area

}
