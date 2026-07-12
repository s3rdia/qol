# Graphic Axes

Set different options regarding the axes of a graphic produced by
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md).
All parameters can also be set globally via
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md).

## Usage

``` r
graphic_axes(
  primary_axes_max = "auto",
  primary_axes_min = "auto",
  primary_axes_steps = 5,
  primary_axes_decimals = 0,
  primary_axes_big_mark = ".",
  primary_axes_decimal_mark = ",",
  primary_axes_prefix = "",
  primary_axes_suffix = "",
  primary_axes_scale = 1,
  primary_values_decimals = 1,
  primary_values_big_mark = ".",
  primary_values_decimal_mark = ",",
  primary_values_prefix = "",
  primary_values_suffix = "",
  secondary_axes_max = "auto",
  secondary_axes_min = "auto",
  secondary_axes_steps = 5,
  secondary_axes_decimals = 0,
  secondary_axes_big_mark = ".",
  secondary_axes_decimal_mark = ",",
  secondary_axes_prefix = "",
  secondary_axes_suffix = "",
  secondary_axes_scale = 1,
  secondary_values_decimals = 0,
  secondary_values_big_mark = ".",
  secondary_values_decimal_mark = ",",
  secondary_values_prefix = "",
  secondary_values_suffix = "",
  variable_axes_interval = 1
)
```

## Arguments

- primary_axes_max:

  Maximum value for the primary axes. If "auto", the maximum value is
  determined by the maximum value present in the graphic.

- primary_axes_min:

  Minimum value for the primary axes. If "auto", the minimum value is
  determined by the minimum value present in the graphic. If there is no
  negative value it will always be set to 0.

- primary_axes_steps:

  The number of steps from minimum to maximum value for the primary
  axes.

- primary_axes_decimals:

  Number of decimal points the values have on the primary axes.

- primary_axes_big_mark:

  Character used as the thousand separator on the primary axes.

- primary_axes_decimal_mark:

  Character used as the decimal separator on the primary axes.

- primary_axes_prefix:

  What to put in front of the value on the primary axes.

- primary_axes_suffix:

  What to put after the value on the primary axes.

- primary_axes_scale:

  Multiplier for the values on the primary axes.

- primary_values_decimals:

  Number of decimal points the values plotted on the primary axes have.

- primary_values_big_mark:

  Character used as the thousand separator for the values plotted on the
  primary axes have.

- primary_values_decimal_mark:

  Character used as the decimal separator for the values plotted on the
  primary axes have.

- primary_values_prefix:

  What to put in front of the values plotted on the primary axes have.

- primary_values_suffix:

  What to put after the values plotted on the primary axes have.

- secondary_axes_max:

  Maximum value for the secondary axes. If "auto", the maximum value is
  determined by the maximum value present in the graphic.

- secondary_axes_min:

  Minimum value for the secondary axes. If "auto", the minimum value is
  determined by the minimum value present in the graphic. If there is no
  negative value it will always be set to 0.

- secondary_axes_steps:

  The number of steps from minimum to maximum value for the secondary
  axes.

- secondary_axes_decimals:

  Number of decimal points the values have on the secondary axes.

- secondary_axes_big_mark:

  Character used as the thousand separator on the secondary axes.

- secondary_axes_decimal_mark:

  Character used as the decimal separator on the secondary axes.

- secondary_axes_prefix:

  What to put in front of the value on the secondary axes.

- secondary_axes_suffix:

  What to put after the value on the secondary axes.

- secondary_axes_scale:

  Multiplier for values on the secondary axes.

- secondary_values_decimals:

  Number of decimal points the values plotted on the secondary axes
  have.

- secondary_values_big_mark:

  Character used as the thousand separator for the values plotted on the
  secondary axes.

- secondary_values_decimal_mark:

  Character used as the decimal separator for the values plotted on the
  secondary axes.

- secondary_values_prefix:

  What to put in front of the values plotted on the secondary axes.

- secondary_values_suffix:

  What to put after the values plotted on the secondary axes.

- variable_axes_interval:

  The number of steps in which to display labels on the variable axes.

## Value

Returns a list of named graphic options.

## See also

The main graphic function:
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md)

Graphic options:
[`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
`graphic_axes()`,
[`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md),
[`graphic_output()`](https://s3rdia.github.io/qol/reference/graphic_output.md),
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
default_axes <- graphic_axes()

# Set specific options, the rest will be set to default values
custom_axes <- graphic_axes(primary_axes_max      = 100,
                            primary_axes_decimals = 1)

# Apply options locally
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    axes           = custom_axes)

# Or direct
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    axes           = graphic_axes(primary_axes_max      = 100,
                                                  primary_axes_decimals = 1))

# Set options globally
set_graphic_options(primary_axes_max      = 100,
                    primary_axes_decimals = 1)

my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars)
```
