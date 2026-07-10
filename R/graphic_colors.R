###############################################################################
# Colors
###############################################################################
#' Set Up Basic Color Themes
#'
#' @description
#' Set up basic color themes for global options.
#'
#' @return
#' Returns a named list.
#'
#' @noRd
base_color_themes <- function(){
    greys <- c("#2B2B2B", "#5A5A5A", "#9A9A9A", "#DADADA")

    list(ocean = c(
            "#12324A", "#1F4E6E", "#3E6F8E", "#7EB1C8", "#A7C9D8", "#E5F0F6", greys),

         sunset = c(
            "#5A1E22", "#7F2F2E", "#9C5A2F", "#D38A45", "#F0BF78", "#F6D49B", greys),

         ember = c(
            "#4A1423", "#6B2335", "#7A3F57", "#AF8692", "#D7B8BE", "#E9D3D6", greys),

         forest = c(
            "#15342A", "#2C5A42", "#4F6F4A", "#8EA872", "#B1C48E", "#E6EFD8", greys),

         lagoon = c(
            "#0C3A42", "#1E5F68", "#2F7F86", "#75B9BA", "#9ACCCD", "#E3F2F2", greys),

         dusk = c(
            "#2D1F4A", "#45346F", "#5E4E7A", "#9388B1", "#B8ACCA", "#ECE8F2", greys),

         clay = c(
            "#4A2A1E", "#6E4432", "#7E5E46", "#B6947A", "#DED0C1", "#EFE6DD", greys),

         olive_gold = c(
            "#3E4415", "#646B2A", "#7F8538", "#BFC27F", "#E6E0B3", "#F4F1D6", greys),

         steel = c(
            "#273A4F", "#3B546E", "#5E6F82", "#9FB1C2", "#C1CCD7", "#EDF1F5", greys),

         graphite = c(
            "#0F0F10", "#1E1E20", "#2F2F32", "#444448", "#5E5E63",
            "#7A7A80", "#9A9AA0", "#BEBEC3", "#E0E0E3", "#F2F2F4"),

         aurora = c(
            "#1A2438", "#1F3A52", "#275C6E", "#2E7F86", "#4EA18F",
            "#7BBE87", "#A8D68A", "#D1E39C", "#EAF0C4", "#F6F8E6"),

         forest_sun = c(
            "#1F2B1C", "#2E4225", "#456633", "#6E8C3F", "#9FB552",
            "#C7D56A", "#E2EA8E", "#F0F4B5", "#F8F9D9", "#FCFDEE"),

         clay_mint = c(
            "#3A2A1F", "#5A3E2A", "#7A5734", "#9A7A42", "#8FA15B",
            "#6EB07A", "#7FC9A1", "#A8DEC2", "#D4F0E3", "#F0FAF6"),

         violet_gold = c(
            "#2A1F3D", "#3E2A5C", "#56387A", "#70509A", "#8E75B5",
            "#AFA0C8", "#CFC3C5", "#E6D8A8", "#F3E8C7", "#FBF6E8"),

         night_lime = c(
            "#182624", "#1F3F3A", "#2C6257", "#3E8771", "#5AAD7E",
            "#7FCC7A", "#A6E37B", "#D0F09A", "#ECF8C9", "#F8FCE9"),

         plum_citrus = c(
            "#321826", "#4A2135", "#663047", "#83485D", "#9E6E75",
            "#B89A8A", "#D0C28F", "#E5DEA7", "#F3F0CE", "#FBF9EC"),

         violet_fire = c(
             "#280959", "#4b1d91", "#731498", "#a1119d", "#d23194",
             "#ef6375", "#f28066", "#f2a65f", "#f3cb86", "#f6e7b6"))
}


#' Create The Default Global Theme List
#'
#' @description
#' Creates the default global list of themes consisting of the base and corresponding
#' font colors.
#'
#' @return
#' Returns a named list.
#'
#' @noRd
create_global_themes <- function(){
    global_list <- list()
    base_colors <- base_color_themes()

    # Loop through base color themes and create full color themes with font colors
    for (i in seq_along(base_colors)){
        name <- names(base_colors)[i]

        # For these themes the base colors are also the font outside colors
        if (name %in% c("aurora", "forest_sun", "clay_mint", "violet_gold",
                        "night_lime", "plum_citrus", "violet_fire")){
            theme <- add_color_theme(name, base_colors[[i]], NULL, "base", FALSE)
        }
        # For all the other themes the outside color is just black
        else{
            theme <- add_color_theme(name, base_colors[[i]], print = FALSE)
        }

        # Add to global list
        global_list <- c(global_list, stats::setNames(list(theme), name))
    }

    global_list
}


#' Managing Global Color Themes
#'
#' @name color_themes
#'
#' @description
#' [add_color_theme()]: Adds a new color theme containing base and font colors to the
#' global theme list.
#'
#' @param theme_name The name of the theme.
#' @param base_colors The base colors for the segments.
#' @param font_inside_colors The individual font colors used when drawing values inside segments
#' on the corresponding base color. By default the colors black and white will be automatically
#' generated based on the relative luminance of the base colors.
#' @param font_outside_colors The individual font colors used when drawing values outside segments
#' on the corresponding base color.
#' @param print TRUE by default. Whether to display the theme afterwards or not.
#'
#' @return
#' [add_color_theme()]: Returns newly created color theme.
#'
#' @seealso
#' Main graphic function: [design_graphic()]
#'
#' Graphic options: [graphic_visuals()], [modify_graphic_visuals()], [graphic_axes()],
#' [modify_graphic_axes()], [graphic_dimensions()], [modify_graphic_dimensions()],
#' [graphic_output()], [modify_graphic_output()], [graphic_fine_tuning()], [modify_graphic_fine_tuning()]
#'
#' Global graphic options: [set_graphic_options()], [get_graphic_options()], [reset_graphic_options()]
#'
#' @examples
#' # Add a color theme where font colors are set automatically
#' add_color_theme("rainbow", rainbow(10))
#'
#' # Add a color theme with individual font colors. Use "base" keyword to copy
#' # base colors.
#' add_color_theme(theme_name  = "tropic",
#'                 base_colors = c("#1B3A2E", "#1F5A3F", "#257F54", "#2FA36A", "#56C07E",
#'                                 "#85D49A", "#B2E4B8", "#D7F0D5", "#ECF8EB", "#F7FCF7"),
#'                 font_inside_colors = c("#F7FCF7", "#ECF8EB", "#D7F0D5", "#B2E4B8", "#85D49A",
#'                                        "#56C07E", "#2FA36A", "#257F54", "#1F5A3F", "#1B3A2E"),
#'                 font_outside_colors = "base")
#'
#' @rdname color_themes
#'
#' @export
add_color_theme <- function(theme_name,
                            base_colors,
                            font_inside_colors  = NULL,
                            font_outside_colors = rep("#000000", 10),
                            print               = TRUE){
    theme_name <- get_origin_as_char(theme_name, substitute(theme_name))
    number_of_base_colors <- length(base_colors)

    if (number_of_base_colors == 0){
        print_message("ERROR", "No base colors provided. Color theme won't be added.")
        return(invisible(.qol_options[["graphic_themes"]]))
    }

    # By default determine the font inside color by the relative luminance of
    # the provided base colors.
    if (is.null(font_inside_colors)){
        # Convert hex colors in RGB matrix
        rgb_matrix <- col2rgb(base_colors)

        # Calculate relative luminance
        luminance <- (0.2126 * rgb_matrix["red",   ] +
                      0.7152 * rgb_matrix["green", ] +
                      0.0722 * rgb_matrix["blue",  ]) / 255

        # pick text color based on relative luminance threshold
        font_inside_colors <- ifelse(luminance > 0.5, "#000000", "#FFFFFF")
    }
    # If keyword "base" was passed, then set base colors as font colors
    else if (length(font_inside_colors) == 1 && font_inside_colors == "base"){
        font_inside_colors <- base_colors
    }

    # If keyword "base" was passed, then set base colors as font colors
    if (length(font_outside_colors) == 1 && font_outside_colors == "base"){
        font_outside_colors <- base_colors
    }

    # Loop through provided base colors and check if they are valid
    for (single_color in base_colors){
        if (!grepl("^#?[A-Fa-f0-9]{6}$", single_color)){
            print_message("ERROR", "Base color '[single_color]' must be a 6 character <hex code>. Color theme won't be added.",
                          single_color = single_color)
            return(invisible(.qol_options[["graphic_themes"]]))
        }
    }

    # Loop through provided font colors and check if they are valid
    for (single_color in font_inside_colors){
        if (!grepl("^#?[A-Fa-f0-9]{6}$", single_color)){
            print_message("ERROR", "Font inside color '[single_color]' must be a 6 character <hex code>. Color theme won't be added.",
                          single_color = single_color)
            return(invisible(.qol_options[["graphic_themes"]]))
        }
    }

    # Loop through provided font colors and check if they are valid
    for (single_color in font_outside_colors){
        if (!grepl("^#?[A-Fa-f0-9]{6}$", single_color)){
            print_message("ERROR", "Font outside color '[single_color]' must be a 6 character <hex code>. Color theme won't be added.",
                          single_color = single_color)
            return(invisible(.qol_options[["graphic_themes"]]))
        }
    }

    # Update the internal global theme list
    .qol_options[["graphic_themes"]][[theme_name]] <- structure(list(base         = base_colors,
                                                                     font_inside  = font_inside_colors,
                                                                     font_outside = font_outside_colors),
                                                                class = "qol_color_theme")

    # Display added theme
    if (print){
        display_colors(theme_name)
        print_message("NOTE", "Theme '[theme]' was succesfully added to global theme list.", theme = theme_name)
    }

    invisible(.qol_options[["graphic_themes"]][[theme_name]])
}


#' @description
#' [get_theme_colors()]: Retrieve a list with three vectors of hex colors from the
#' globally set up themes containing the base colors for the segments and the
#' corresponding font colors.
#'
#' @param theme_name The theme name to look up.
#'
#' @return
#' [get_theme_colors()]: A list with three vectors of hex color codes.
#'
#' @examples
#' # Get a color theme from the global theme list
#' full_theme <- get_theme_colors("ocean")
#'
#' base_colors  <- full_theme[[1]]
#' font_inside  <- full_theme[[2]]
#' font_outside <- full_theme[[3]]
#'
#' @rdname color_themes
#'
#' @export
get_theme_colors <- function(theme_name){
    if (length(theme_name) > 1){
        print_message("WARNING", "Only a single theme can be retrieved. First vector element will be used.")

        theme_name <- theme_name[[1]]
    }

    if (!theme_name %in% names(.qol_options[["graphic_themes"]])){
        print_message("ERROR", "Theme '[theme_name]' doesn't exist. Default theme will be used.", theme_name = theme_name)
        return(invisible(.qol_options[["graphic_themes"]][[1]]))
    }

    .qol_options[["graphic_themes"]][[theme_name]]
}


#' @description
#' [reset_color_themes()]: Resets the global color themes back to default.
#'
#' @param clear_themes FALSE by default. If TRUE clears the global theme list.
#'
#' @return
#' [reset_color_themes()]: Default global color list or empty list.
#'
#' @examples
#' # Clear all globally stored themes in case only self created themes should be stored
#' reset_color_themes(clear_themes = TRUE)
#'
#' # Reset global themes to default
#' reset_color_themes()
#'
#' @rdname color_themes
#'
#' @export
reset_color_themes <- function(clear_themes = FALSE){
    if (clear_themes){
        .qol_options[["graphic_themes"]] <- list()
    }
    else{
        .qol_options[["graphic_themes"]] <- create_global_themes()
    }

    invisible(.qol_options[["graphic_themes"]])
}


#' @description
#' [display_colors()]: Displays all colors of a theme with the corresponding hex codes.
#'
#' @param theme_name The name of a globally stored theme.
#'
#' @return
#' [display_colors()]: Returns a list of base and font colors.
#'
#' @examples
#' # Displaying colors and themes
#' display_colors("ocean")
#'
#' @rdname color_themes
#'
#' @export
display_colors <- function(theme_name){
    if (length(theme_name) > 1 || !is.character(theme_name)){
        print_message("ERROR", c("Only a single theme can be displayed. Use 'display_themes()' to display all",
                                "currently stored themes with their respective colors."))
        return(invisible(NULL))
    }

    if (!theme_name %in% names(.qol_options[["graphic_themes"]])){
        print_message("ERROR", "Theme '[theme_name]' doesn't exist. Default theme will be used.", theme_name = theme_name)

        theme_name <- "ocean"
    }

    base_colors  <- get_theme_colors(theme_name)[[1]]
    font_inside  <- get_theme_colors(theme_name)[[2]]
    font_outside <- get_theme_colors(theme_name)[[3]]

    # Set up a new graphic
    grid::grid.newpage()

    # To evenly draw rectangles side by side, the space each rectangle takes has to be
    # calculated and at what position it has to be drawn. Do do that, each position is
    # divided by the total number of colors. This basically gives the positions of the
    # right edges of the rectangles. Since the draw function draws rectangles from the
    # center, the positions have to be reduced by a half, to get the center positions.
    number_of_colors <- length(base_colors)
    rect_centers     <- (seq_len(number_of_colors) - 0.5) / number_of_colors

    # Draw rectangles side by side
    grid::grid.rect(x      = rect_centers,
                    y      = 0.4,
                    width  = 1 / number_of_colors,
                    height = 0.4,
                    gp     = grid::gpar(fill = base_colors, col = NA))

    # Draw hex codes in rectangles for font inside colors
    grid::grid.text(base_colors,
                    x   = rect_centers,
                    y   = 0.4,
                    rot = 90,
                    gp  = grid::gpar(col = font_inside))

    # Draw hex codes above rectangles for font outside colors
    grid::grid.text(font_outside,
                    x   = rect_centers,
                    y   = 0.75,
                    rot = 90,
                    gp  = grid::gpar(col = font_outside))

    invisible(list(base = base_colors,
                   font_inside  = font_inside,
                   font_outside = font_outside))
}


#' @description
#' [display_themes()]: Display all colors of all themes currently present in the
#' global environment.
#'
#' @return
#' [display_themes()]: A list containing all globally stored themes with their color
#' codew vectors.
#'
#' @examples
#' display_themes()
#'
#' @rdname color_themes
#'
#' @export
display_themes <- function(){
    global_themes    <- .qol_options[["graphic_themes"]]
    number_of_themes <- length(global_themes)

    if (number_of_themes == 0){
        print_message("NOTE", c("No themes stored. Add themes by calling 'set_color_theme()' or use",
                                "'reset_color_theme()' to get back default themes."))
        return(invisible(global_themes))
    }

    # Set up a new graphic
    grid::grid.newpage()

    # Set up a viewport with a basic structural layout which contains a label column
    # for the theme names and a column for the color ramps. Theme names receive a smaller
    # space. Afterwards add the viewport to the stack to work within it.
    label_width <- 0.2

    grid_layout <- grid::grid.layout(nrow   = number_of_themes,
                                     ncol   = 2,
                                     widths = grid::unit.c(grid::unit(label_width, "npc"),
                                                           grid::unit(1 - label_width, "npc")))

    grid::viewport(layout = grid_layout) |> grid::pushViewport()

    # Draw all theme names in the first column and spread them evenly among the rows
    grid::grid.text(label = names(global_themes),
                    x     = 0.95,
                    y     = grid::unit(seq(number_of_themes, 1) - 0.5, "native"),
                    just  = "right",
                    vp    = grid::viewport(layout.pos.col = 1, yscale = c(0, number_of_themes)))

    # Draw the individual color ramps one after another and evenly spread the colors based
    # on the individual number of colors in a theme.
    for (i in seq_len(number_of_themes)){
        base_colors      <- global_themes[[i]][["base"]]
        font_colors      <- global_themes[[i]][["font_inside"]]
        number_of_colors <- length(base_colors)
        rect_centers     <- (seq_len(number_of_colors) - 0.5) / number_of_colors

        # Draw rectangles with base colors
        grid::grid.rect(x = rect_centers,
                        width  = 1 / number_of_colors,
                        height = 0.8,
                        gp     = grid::gpar(fill = base_colors, col = NA),
                        vp     = grid::viewport(layout.pos.row = i, layout.pos.col = 2))

        # Draw numbers in rectangles
        grid::grid.text(seq_len(number_of_colors),
                        x   = rect_centers,
                        y   = 0.5,
                        gp  = grid::gpar(col = font_colors),
                        vp  = grid::viewport(layout.pos.row = i, layout.pos.col = 2))
    }

    # Remove viewport from the stack
    grid::popViewport()

    invisible(global_themes)
}


#' @description
#' [override_theme()]: Is used to override certain elements of an individual segment.
#' Can be used to e.g. give certain segments a special coloring.
#'
#' @param number The number of the segment which should be manipulated.
#' @param color The new main segment color.
#' @param border_color The new border color of the segment.
#' @param font_color The new font color of the segment value.
#'
#' @return
#' [override_theme()]: A list containing the override parameters.
#'
#' @examples
#' # Example data frame
#' my_data <- dummy_data(100)
#'
#' # Formats
#' age. <- discrete_format(
#'     "Total"          = 0:100,
#'     "under 18"       = 0:17,
#'     "18 to under 65" = 18:64,
#'     "65 and older"   = 65:100)
#'
#' sex. <- discrete_format(
#'     "Male"   = 1,
#'     "Female" = 2)
#'
#' # Override specific segment visuals to make them stand out
#' my_data |>
#'      design_graphic(axes_variables = "sex",
#'                     segments       = "age",
#'                     values         = weight,
#'                     diagram        = dg_vbars,
#'                     formats        = list(sex = sex.,
#'                                           age = age.),
#'                     visuals        = graphic_visuals(
#'                         color_theme    = "violet_fire",
#'                         theme_override = list(override_theme(4, "#FF0000", "#00FF00", "#000000"),
#'                                               override_theme(7, "#00FFFF", "#FFFF00", "#0000FF"))))
#'
#' @rdname color_themes
#'
#' @export
override_theme <- function(number       = NULL,
                           color        = NULL,
                           border_color = NULL,
                           font_color   = NULL){
    if (!is.null(number) && length(number) > 1){
        print_message("WARNING", c("Only one segment number in override_theme allowed. Override will be omitted."))
        return(list())
    }

    if (is.null(number)){
        print_message("WARNING", c("No segment number provided. Override will be omitted."))
        return(list())
    }

    # Check colors
    if (!grepl("^#?[A-Fa-f0-9]{6}$", color)){
        print_message("ERROR", "<Color> '[color]' must be a 6 character hex code. Override parameter will be omitted.",
                      color = color)
        color <- NULL
    }

    if (!grepl("^#?[A-Fa-f0-9]{6}$", border_color)){
        print_message("ERROR", "<Border color> '[color]' must be a 6 character hex code. Override parameter will be omitted.",
                      color = border_color)
        border_color <- NULL
    }

    if (!grepl("^#?[A-Fa-f0-9]{6}$", font_color)){
        print_message("ERROR", "<Border color> '[color]' must be a 6 character hex code. Override parameter will be omitted.",
                      color = font_color)
        font_color <- NULL
    }

    # Return as list
    as.list(environment())
}


#' @description
#' [sequential_usage()]: Creates a basic numeric sequence based on the number of
#' colors and segments.
#'
#' @examples
#' # sequential_usage sequences
#' c(1),
#' c(1, 2),
#' c(1, 2, 3),
#' c(1, 2, 3, 4),
#' c(1, 2, 3, 4, 5),
#' c(1, 2, 3, 4, 5, 6),
#' c(1, 2, 3, 4, 5, 6, 7),
#' c(1, 2, 3, 4, 5, 6, 7, 8),
#' c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#' c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' @rdname color_themes
#'
#' @export
sequential_usage <- function(number_of_colors, number_of_segments){
    seq_len(min(number_of_colors, number_of_segments))
}


#' @description
#' [contrast_usage()]: Creates a numeric sequence, using odd numbers first, then
#' adding even numbers, so that adjacent segments have more contrast.
#'
#' @examples
#' # contrast_usage sequences
#' c(1),
#' c(1, 3),
#' c(1, 3, 5),
#' c(1, 3, 5, 7),
#' c(1, 3, 5, 7, 9),
#' c(1, 2, 3, 5, 7, 9),
#' c(1, 2, 3, 4, 5, 7, 9),
#' c(1, 2, 3, 4, 5, 6, 7, 9),
#' c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#' c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' @rdname color_themes
#'
#' @export
contrast_usage <- function(number_of_colors, number_of_segments){
    # Contrast usage always skips one color. First take the colors at odd
    # number positions, than add the even positions in between.
    color_usage_pool <- c(seq(1, number_of_colors, by = 2),
                          seq(2, number_of_colors, by = 2))

    # Build the color usage list
    sort(color_usage_pool[seq_len(min(number_of_colors, number_of_segments))])
}


#' @description
#' [high_contrast_usage()]: Creates a numeric sequence, using extreme points first
#' and then filling up with the mid points. Adjacent segments will always have the
#' highest possible contrast.
#'
#' @examples
#' # high_contrast_usage sequences
#' c(1),
#' c(1, 10),
#' c(1, 5, 10),
#' c(1, 3, 5, 10),
#' c(1, 3, 5, 7, 10),
#' c(1, 2, 3, 5, 7, 10),
#' c(1, 2, 3, 4, 5, 7, 10),
#' c(1, 2, 3, 4, 5, 6, 7, 10),
#' c(1, 2, 3, 4, 5, 6, 7, 8, 10),
#' c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' @rdname color_themes
#'
#' @export
high_contrast_usage <- function(number_of_colors, number_of_segments){
    if (number_of_segments >= number_of_colors){
        return(seq_len(min(number_of_colors, number_of_segments)))
    }

    # Get the extreme points
    color_usage_pool      <- integer(number_of_colors)
    color_usage_pool[1:2] <- c(1, number_of_colors)

    # Set up loop to determine the high contrast sequence. Starts from the extreme
    # points and then always looks for the respective midpoints.
    left_side     <- 1
    right_side    <- number_of_colors
    last_element  <- 1
    first_element <- 1
    colors_used   <- 2

    while (last_element <= first_element && colors_used < number_of_segments){
        # Determine current lower and upper boundaries
        lower_bound  <- left_side[last_element]
        upper_bound  <- right_side[last_element]
        last_element <- last_element + 1

        if (upper_bound - lower_bound <= 1){
            next
        }

        # Get the midpoint between the boundaries
        midpoint <- (lower_bound + upper_bound) %/% 2

        # Add midpoint to used colors
        colors_used                   <- colors_used + 1
        color_usage_pool[colors_used] <- midpoint

        # If the midpoint is to the right of the lower boundary and not directly adjacent
        if (midpoint - lower_bound > 1){
            # Set up the next interval to: lower_bound - midpoint
            first_element             <- first_element + 1
            left_side[first_element]  <- lower_bound
            right_side[first_element] <- midpoint
        }

        # If the midpoint is to the left of the upper boundary and not directly adjacent
        if (upper_bound - midpoint > 1){
            # Set up the next interval to: midpoint - upper_bound
            first_element             <- first_element + 1
            left_side[first_element]  <- midpoint
            right_side[first_element] <- upper_bound
        }
    }

    # Return sequence
    sort(color_usage_pool[seq_len(min(number_of_colors, number_of_segments))])
}
