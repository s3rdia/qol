# Graphic Fine Tuning

Throughout the graphic generation some fixed values are used. These can
be adjusted to make detailed graphical changes in certain areas. These
values normally should only be altered in edge cases. All parameters can
also be set globally via
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md).

## Usage

``` r
graphic_fine_tuning(
  diagram_margin = 0.01,
  values_vjust_positive = 1.7,
  values_vjust_negative = -0.7,
  values_vjust_90_positive = 1.2,
  values_vjust_90_negative = -0.2,
  value_overlap_factor = 1.35,
  shrink_segment_width = 0.55,
  values_hjust = 0.5,
  values_hjust_90 = 0.35,
  values_hjust_90_plus = 0.05,
  values_zero_line_offset = 0.2,
  values_below_axes_just = 1.5,
  values_below_axes_90_just = 1.4,
  tick_length = 0.02,
  value_axes_margin = 0.02,
  y_axes_scaling = 1.1,
  swap_direction_threshold = 0.75,
  segment_line_offset = 0.3,
  segment_line_treshhold = 0.95,
  segment_label_hjust = 0.05,
  max_segment_label_shift = 0.25,
  cm_to_inch_factor = 2.54,
  svg_anchor_adjust = 0.6,
  svg_line_height_adjust = 0.65
)
```

## Arguments

- diagram_margin:

  Margin used within the diagram area.

- values_vjust_positive:

  Positive vertical adjustment for values in vbars.

- values_vjust_negative:

  Negative vertical adjustment for values in vbars.

- values_vjust_90_positive:

  Positive vertical adjustment for rotated values in vbars.

- values_vjust_90_negative:

  Negative vertical adjustment for rotated values in vbars.

- value_overlap_factor:

  Used as a multiplier for the value height. If the value height
  multiplied by this factor exceeds the segment height of a vbar, the
  value will be automatically drawn outside the segment.

- shrink_segment_width:

  Shrinks the segment width by this factor, if the segment borders are
  colored, to prevent the bars from overlapping

- values_hjust:

  Horizontal adjustment of segment values.

- values_hjust_90:

  Horizontal adjustment of rotated segment values.

- values_hjust_90_plus:

  Additional horizontal adjustment of rotated segment values, if drawn
  inside segments.

- values_zero_line_offset:

  Offset to the x axes for 0 values.

- values_below_axes_just:

  Adjustment for 0 values, if drawn below x axes.

- values_below_axes_90_just:

  Adjustment for rotated 0 values, if drawn below x axes.

- tick_length:

  Length of the axes ticks.

- value_axes_margin:

  Additional margin when measuring the value axes width

- y_axes_scaling:

  Scaling factor for the y axes, if maximum value is calculated
  automatically.

- swap_direction_threshold:

  Determines the threshold for the variable axes at which the drawing
  direction is swapped.

- segment_line_offset:

  A static offset at which segment lines are drawn.

- segment_line_treshhold:

  Segment lines won't be drawn further than the maximum value multiplied
  by this factor.

- segment_label_hjust:

  Horizontal adjustment of the segment labels if they are drawn in
  stairs.

- max_segment_label_shift:

  The maximum percentage of the textbox width the segment labels can be
  shifted to achieve a decollision.

- cm_to_inch_factor:

  cm to inc conversion factor.,

- svg_anchor_adjust:

  Text anchoring adjustment for interactive SVG graphics, so that the
  positions look like in the static version.

- svg_line_height_adjust:

  Text line height in interactive SVG graphics is bigger and needs to be
  turned down.

## Value

Returns a list of named graphic options.

## See also

The main graphic function:
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md)

Graphic options:
[`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
[`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
[`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md),
[`graphic_output()`](https://s3rdia.github.io/qol/reference/graphic_output.md),
`graphic_fine_tuning()`,
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
default_fine_tuning <- graphic_fine_tuning()

# Set specific options, the rest will be set to default values
custom_fine_tuning <- graphic_fine_tuning(values_vjust_positive = 2.5,
                                          tick_length           = 0.05)

# Apply options locally
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    fine_tuning    = custom_fine_tuning)

# Or direct
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    fine_tuning    = graphic_fine_tuning(values_vjust_positive = 2.5,
                                                         tick_length           = 0.05))

# Set options globally
set_graphic_options(values_vjust_positive = 2.5,
                    tick_length           = 0.05)

my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars)
```
