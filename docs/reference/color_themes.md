# Managing Global Color Themes

`add_color_theme()`: Adds a new color theme containing base and font
colors to the global theme list.

`get_theme_colors()`: Retrieve a list with three vectors of hex colors
from the globally set up themes containing the base colors for the
segments and the corresponding font colors.

`reset_color_themes()`: Resets the global color themes back to default.

`display_colors()`: Displays all colors of a theme with the
corresponding hex codes.

`display_themes()`: Display all colors of all themes currently present
in the global environment.

`override_theme()`: Is used to override certain elements of an
individual segment. Can be used to e.g. give certain segments a special
coloring.

`sequential_usage()`: Creates a basic numeric sequence based on the
number of colors and segments.

`contrast_usage()`: Creates a numeric sequence, using odd numbers first,
then adding even numbers, so that adjacent segments have more contrast.

`high_contrast_usage()`: Creates a numeric sequence, using extreme
points first and then filling up with the mid points. Adjacent segments
will always have the highest possible contrast.

## Usage

``` r
add_color_theme(
  theme_name,
  base_colors,
  font_inside_colors = NULL,
  font_outside_colors = rep("#000000", 10),
  print = TRUE
)

get_theme_colors(theme_name)

reset_color_themes(clear_themes = FALSE)

display_colors(theme_name)

display_themes()

override_theme(
  number = NULL,
  color = NULL,
  border_color = NULL,
  font_color = NULL
)

sequential_usage(number_of_colors, number_of_segments)

contrast_usage(number_of_colors, number_of_segments)

high_contrast_usage(number_of_colors, number_of_segments)
```

## Arguments

- theme_name:

  The name of a globally stored theme.

- base_colors:

  The base colors for the segments.

- font_inside_colors:

  The individual font colors used when drawing values inside segments on
  the corresponding base color. By default the colors black and white
  will be automatically generated based on the relative luminance of the
  base colors.

- font_outside_colors:

  The individual font colors used when drawing values outside segments
  on the corresponding base color.

- print:

  TRUE by default. Whether to display the theme afterwards or not.

- clear_themes:

  FALSE by default. If TRUE clears the global theme list.

- number:

  The number of the segment which should be manipulated.

- color:

  The new main segment color.

- border_color:

  The new border color of the segment.

- font_color:

  The new font color of the segment value.

- number_of_colors:

  The number of colors stored in a theme.

- number_of_segments:

  The number of segments inside the graphic.

## Value

`add_color_theme()`: Returns newly created color theme.

`get_theme_colors()`: A list with three vectors of hex color codes.

`reset_color_themes()`: Default global color list or empty list.

`display_colors()`: Returns a list of base and font colors.

`display_themes()`: A list containing all globally stored themes with
their color codew vectors.

`override_theme()`: A list containing the override parameters.

## See also

Main graphic function:
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md)

Graphic options:
[`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
[`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
[`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md),
[`graphic_output()`](https://s3rdia.github.io/qol/reference/graphic_output.md),
[`graphic_fine_tuning()`](https://s3rdia.github.io/qol/reference/graphic_fine_tuning.md),
[`modify_graphic_options()`](https://s3rdia.github.io/qol/reference/modify_graphic_options.md)

Global graphic options:
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md),
[`get_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md),
[`reset_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md)

## Examples

``` r
# Add a color theme where font colors are set automatically
add_color_theme("rainbow", rainbow(10))

# Add a color theme with individual font colors. Use "base" keyword to copy
# base colors.
add_color_theme(theme_name  = "tropic",
                base_colors = c("#1B3A2E", "#1F5A3F", "#257F54", "#2FA36A", "#56C07E",
                                "#85D49A", "#B2E4B8", "#D7F0D5", "#ECF8EB", "#F7FCF7"),
                font_inside_colors = c("#F7FCF7", "#ECF8EB", "#D7F0D5", "#B2E4B8", "#85D49A",
                                       "#56C07E", "#2FA36A", "#257F54", "#1F5A3F", "#1B3A2E"),
                font_outside_colors = "base")

# Get a color theme from the global theme list
full_theme <- get_theme_colors("ocean")

base_colors  <- full_theme[[1]]
font_inside  <- full_theme[[2]]
font_outside <- full_theme[[3]]

# Clear all globally stored themes in case only self created themes should be stored
reset_color_themes(clear_themes = TRUE)

# Reset global themes to default
reset_color_themes()

# Displaying colors and themes
display_colors("ocean")

display_themes()

# Example data frame
my_data <- dummy_data(100)

# Formats
age. <- discrete_format(
    "Total"          = 0:100,
    "under 18"       = 0:17,
    "18 to under 65" = 18:64,
    "65 and older"   = 65:100)

sex. <- discrete_format(
    "Male"   = 1,
    "Female" = 2)

# Override specific segment visuals to make them stand out
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "age",
                    values         = weight,
                    diagram        = dg_vbars,
                    formats        = list(sex = sex.,
                                          age = age.),
                    visuals        = graphic_visuals(
                        color_theme    = "violet_fire",
                        theme_override = list(override_theme(4, "#FF0000", "#00FF00", "#000000"),
                                              override_theme(7, "#00FFFF", "#FFFF00", "#0000FF"))))

# sequential_usage sequences
c(1)
c(1, 2)
c(1, 2, 3)
c(1, 2, 3, 4)
c(1, 2, 3, 4, 5)
c(1, 2, 3, 4, 5, 6)
c(1, 2, 3, 4, 5, 6, 7)
c(1, 2, 3, 4, 5, 6, 7, 8)
c(1, 2, 3, 4, 5, 6, 7, 8, 9)
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# contrast_usage sequences
c(1)
c(1, 3)
c(1, 3, 5)
c(1, 3, 5, 7)
c(1, 3, 5, 7, 9)
c(1, 2, 3, 5, 7, 9)
c(1, 2, 3, 4, 5, 7, 9)
c(1, 2, 3, 4, 5, 6, 7, 9)
c(1, 2, 3, 4, 5, 6, 7, 8, 9)
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# high_contrast_usage sequences
c(1)
c(1, 10)
c(1, 5, 10)
c(1, 3, 5, 10)
c(1, 3, 5, 7, 10)
c(1, 2, 3, 5, 7, 10)
c(1, 2, 3, 4, 5, 7, 10)
c(1, 2, 3, 4, 5, 6, 7, 10)
c(1, 2, 3, 4, 5, 6, 7, 8, 10)
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
```
