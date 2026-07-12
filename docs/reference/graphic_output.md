# Graphic Output

Set different options regarding the dimensions of a graphic produced by
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md).
All parameters can also be set globally via
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md).

## Usage

``` r
graphic_output(
  save_path = NULL,
  file = NULL,
  resolution = 300,
  by_as_grid = FALSE,
  grid_columns = "auto",
  interactive = FALSE
)
```

## Arguments

- save_path:

  If NULL, only draws the graphic in the plot window. Otherwise specify
  an output path.

- file:

  If NULL, only draws the graphic in the plot window. Otherwise specify
  a filename with extension.

- resolution:

  DPI resolution.

- by_as_grid:

  FALSE by default. If TRUE exports one single graphic containing the
  whole grid when using by variables. If used with interactive parameter
  all graphics in the grid will be put into one file, where the graphics
  can be switched with a drop down list.

- grid_columns:

  By default tries to create a square shape. If a numeric value is
  passed the number of grid columns is fixed and the number of rows
  adjusts accordingly.

- interactive:

  FALSE by default. If TRUE exports an interactive html file containing
  mouse over tooltips and hover effects.

## Value

Returns a list of named graphic options.

## Details

    Interactive graphics can be embedded in an <iframe> like this:

    <iframe src="my_graphic.html" style="width: 100%; aspect-ratio: 16 / 9; border: none;"
    scrolling="no"></iframe>

Set the aspect ratio according to your graphic dimensions.

## See also

The main graphic function:
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md)

Graphic options:
[`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
[`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
[`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md),
`graphic_output()`,
[`graphic_fine_tuning()`](https://s3rdia.github.io/qol/reference/graphic_fine_tuning.md),
[`modify_graphic_options()`](https://s3rdia.github.io/qol/reference/modify_graphic_options.md)

Global graphic options:
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md),
[`get_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md),
[`reset_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md)

Color themes:
[`display_colors()`](https://s3rdia.github.io/qol/reference/color_themes.md),
[`display_themes()`](https://s3rdia.github.io/qol/reference/color_themes.md),
[`add_color_theme()`](https://s3rdia.github.io/qol/reference/color_themes.md),
[`get_theme_colors()`](https://s3rdia.github.io/qol/reference/color_themes.md),
[`reset_color_themes()`](https://s3rdia.github.io/qol/reference/color_themes.md),
[`override_theme()`](https://s3rdia.github.io/qol/reference/color_themes.md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(100)

# For default values
default_output <- graphic_output()

# Set specific options, the rest will be set to default values
custom_output <- graphic_output(save_path = "C:/MyFolder/",
                                file      = "MyGraphic.png")

# Apply options locally
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    output         = custom_output)

# Or direct
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    output         = graphic_output(save_path = "C:/MyFolder/",
                                                    file      = "MyGraphic.png"))

# Set options globally
set_graphic_options(save_path = "C:/MyFolder/",
                    file      = "MyGraphic.png")

my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars)
```
