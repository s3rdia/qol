# Graphic Visuals

Set different options regarding the visual appearance of a graphic
produced by
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md).
All parameters can also be set globally via
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md).

## Usage

``` r
graphic_visuals(
  font = "Arial",
  color_theme = "ocean",
  color_usage = contrast_usage,
  theme_override = list(),
  title_font_color = "#000000",
  footnote_font_color = "#000000",
  primary_axes_font_color = "#000000",
  secondary_axes_font_color = "#000000",
  variable_axes_font_color = "#000000",
  label_font_color = "#000000",
  origin_font_color = "#2B2B2B",
  other_font_color = "#000000",
  title_font_face = "bold",
  footnote_font_face = "plain",
  primary_axes_font_face = "plain",
  secondary_axes_font_face = "plain",
  variable_axes_font_face = "plain",
  value_font_face = "bold",
  label_font_face = "plain",
  origin_font_face = "plain",
  other_font_face = "plain",
  title_alignment = "left",
  footnote_alignment = "left",
  hbar_alignment = "right",
  other_alignment = "left",
  reverse_colors = FALSE,
  primary_axes_color = "#9A9A9A",
  secondary_axes_color = "#9A9A9A",
  variable_axes_color = "#9A9A9A",
  graphic_background_color = "#FFFFFF",
  diagram_background_color = "#FFFFFF",
  graphic_border_color = "#000000",
  diagram_border_color = "#FFFFFF",
  segment_border_color = "#000000",
  line_markers = TRUE,
  line_markers_change = TRUE,
  guiding_lines_y = FALSE,
  guiding_lines_x = FALSE,
  guiding_line_type = "dotted",
  guiding_line_color = "#9A9A9A",
  major_separation_line_type = "solid",
  major_separation_line_color = "#9A9A9A",
  minor_separation_line_type = "dotted",
  minor_separation_line_color = "#9A9A9A",
  segment_line_type = "solid",
  segment_line_color = "#000000",
  rotate_segment_labels = FALSE,
  segment_label_rotation = 90,
  remove_small_values = TRUE,
  display_values = TRUE,
  bar_values_inside = TRUE,
  rotate_values = FALSE,
  value_rotation = 90,
  display_plus_symbol = FALSE,
  segment_label_type = "lines",
  segment_label_group = "auto",
  legend_x_pos = "auto",
  legend_y_pos = "auto",
  legend_columns = 1,
  legend_symbol_size = 0.3,
  origin = "Graphic: qol",
  tooltip_font_color = "auto",
  tooltip_background_color = "auto",
  tooltip_border_color = "auto",
  tooltip_border_width = 2,
  tooltip_background_opacity = 0.95,
  tooltip_x_padding = 8,
  tooltip_y_padding = 4,
  tooltip_corner_radius = 2,
  segment_hover_opacity = 0.85,
  group_hover_color = "#6495ED",
  group_hover_opacity = 0.25
)
```

## Arguments

- font:

  Set the font to be used for the entire output.

- color_theme:

  The color theme to use. Can be the name of a globally stored color
  theme or the theme itself created with
  [`add_color_theme()`](https://s3rdia.github.io/qol/reference/color_themes.md).
  Existing color themes can be viewed with
  [`display_themes()`](https://s3rdia.github.io/qol/reference/color_themes.md).

- color_usage:

  A list of numerical vectors that specify which specific colors from
  the color scheme are used for which number of segments. The three
  built in ways are "sequential_usage", "contrast_usage" (default) or
  "high_contrast_usage". To see how they are set up look up the topic
  [color_themes](https://s3rdia.github.io/qol/reference/color_themes.md)
  and the examples.

- theme_override:

  The override parameter takes in a list consisting of
  [`override_theme()`](https://s3rdia.github.io/qol/reference/color_themes.md)
  functions.

- title_font_color:

  Font color of the title.

- footnote_font_color:

  Font color of the footnote.

- primary_axes_font_color:

  Font color of the primary axes.

- secondary_axes_font_color:

  Font color of the secondary axes.

- variable_axes_font_color:

  Font color of the variable axes.

- label_font_color:

  Font color of the segment labels.

- origin_font_color:

  Font color of the origin text.

- other_font_color:

  Font color of every other text element.

- title_font_face:

  Font face of the title.

- footnote_font_face:

  Font face of the footnote.

- primary_axes_font_face:

  Font face of the primary axes.

- secondary_axes_font_face:

  Font face of the secondary axes.

- variable_axes_font_face:

  Font face of the variable axes.

- value_font_face:

  Font face of the values.

- label_font_face:

  Font face of the segment labels.

- origin_font_face:

  Font face of the origin text.

- other_font_face:

  Font face of every other text element.

- title_alignment:

  The graphic titles alignment.

- footnote_alignment:

  The graphic footnote alignment.

- hbar_alignment:

  The alignment of the axes group labels in horizontal bars.

- other_alignment:

  Alignment of other elements, like freely positionable textboxes.

- reverse_colors:

  FALSE by default. If TRUE reverses the color order.

- primary_axes_color:

  The color of the primary axes.

- secondary_axes_color:

  The color of the secondary axes.

- variable_axes_color:

  The color of the variable axes.

- graphic_background_color:

  The background color of entire graphic.

- diagram_background_color:

  The background color of the diagram area.

- graphic_border_color:

  The border color of the entire graphic.

- diagram_border_color:

  The border color of the diagram area.

- segment_border_color:

  The border color of each segment.

- line_markers:

  TRUE by default. Draws markers in line charts. If FALSE, doesn't draw
  markers in line charts.

- line_markers_change:

  TRUE by default. Uses different markers per line. If FALSE, dots are
  used as markers for all lines.

- guiding_lines_y:

  FALSE by default. If TRUE draws guiding lines at the y axes tick
  positions.

- guiding_lines_x:

  FALSE by default. If TRUE draws guiding lines at the x axes tick
  positions.

- guiding_line_type:

  Sets the type of guiding lines drawn at the y axes tick positions. Can
  be "dashed", "dotted" or "solid".

- guiding_line_color:

  The color of the guiding lines drawn at the y axes tick positions.

- major_separation_line_type:

  Sets the type of separation lines drawn between top group categories.
  Can be "dashed", "dotted" or "solid".

- major_separation_line_color:

  The color of the separation lines drawn between top group categories.

- minor_separation_line_type:

  Sets the type of separation lines drawn between minor group
  categories. Can be "dashed", "dotted" or "solid".

- minor_separation_line_color:

  The color of the separation lines drawn between minor group
  categories.

- segment_line_type:

  Sets the type of leading lines from segments to labels. Can be
  "dashed", "dotted" or "solid".

- segment_line_color:

  Sets the color of the leading lines from segments to labels.

- rotate_segment_labels:

  FALSE by default. If TRUE rotates the direct vertical segment labels
  by a number of degrees.

- segment_label_rotation:

  Degrees of segment label rotation.

- remove_small_values:

  TRUE by default. Doesn't display values in stacked diagrams, if the
  corresponding segment is to small and value would go out of bounds. If
  FALSE, always displays values.

- display_values:

  TRUE by default. Displays the values with the segments. If FALSE,
  removes all values and just draws the segments.

- bar_values_inside:

  TRUE by default. In grouped bar charts the segment values will be
  drawn inside the bars. If FALSE, values will be drawn above/beside the
  bars.

- rotate_values:

  FALSE by default. If TRUE rotates values inside the segments by a
  number of degrees.

- value_rotation:

  Degrees of segment value rotation.

- display_plus_symbol:

  FALSE by default. If TRUE displays a + symbol in front of positive
  values.

- segment_label_type:

  Can be "lines", which connects labels and segments with leading lines,
  or "legend", which allows to position a legend separately.

- segment_label_group:

  If label_Type is "lines", then this parameter determines above which
  group of segments the labels will be drawn.

- legend_x_pos:

  Horizontal position of the legend. Also two presets "left" and "right"
  can be used which place the legend beside the diagram.

- legend_y_pos:

  Vertical position of the legend. Also two presets "top" and "bottom"
  can be used which place the legend above or below the diagram.

- legend_columns:

  The number of columns in which the labels should be arranged. If 0,
  all labels will be drawn below each other. If max number of segments,
  then all labels will be drawn beside each other.

- legend_symbol_size:

  The size of the symbol that is drawn to the left of the expression
  text.

- origin:

  A character value that will be written in the bottom right corner of
  the graphic.

- tooltip_font_color:

  Font color of the tooltips in interactive charts. If set to "auto" the
  font color will be black or white depending on the tooltip background
  color.

- tooltip_background_color:

  Background color of the tooltips in interactive charts. If set to
  "auto" the background color will match the color of the corresponding
  segment.

- tooltip_border_color:

  Border color of the tooltips in interactive charts. If set to "auto"
  the border color will match the color of the corresponding segment.

- tooltip_border_width:

  Width of the border of the tooltips in interactive charts.

- tooltip_background_opacity:

  Background opacity of the tooltips in interactive charts. 0 is fully
  transparent, while 1 is fully visible.

- tooltip_x_padding:

  The horizontal space of the tooltip texts in interactive charts to the
  borders.

- tooltip_y_padding:

  The vertical space of the tooltip texts in interactive charts to the
  borders.

- tooltip_corner_radius:

  Radius of the corners of the tooltips in interactive charts. 0 means
  sharp corners, tooltip displays as rectangle.

- segment_hover_opacity:

  When hovering with the mouse cursor over a segment of an interactive
  chart, this parameter determines at how much opacity the segment is
  displayed.

- group_hover_color:

  The background color of segment groups used in interactive charts.

- group_hover_opacity:

  When hovering with the mouse cursor over a segment group of an
  interactive chart, this parameter determines at how much opacity the
  segment group background is displayed.

## Value

Returns a list of named graphic options.

## See also

The main graphic function:
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md)

Graphic options: `graphic_visuals()`,
[`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
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
default_visuals <- graphic_visuals()

# Set specific options, the rest will be set to default values
custom_visuals <- graphic_visuals(title_font_color = "#00FF00",
                                  line_markers     = FALSE)

# Apply options locally
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    visuals        = custom_visuals)

# Or direct
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    visuals        = graphic_visuals(title_font_color = "#00FF00",
                                                     line_markers     = FALSE))

# Set options globally
set_graphic_options(title_font_color = "#00FF00",
                    line_markers     = FALSE)

my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars)
```
