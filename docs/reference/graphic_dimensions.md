# Graphic Dimensions

Set different options regarding the dimensions of a graphic produced by
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md).
All parameters can also be set globally via
[`set_graphic_options()`](https://s3rdia.github.io/qol/reference/graphic_options.md).

## Usage

``` r
graphic_dimensions(
  graphic_width = 16,
  graphic_height = 9,
  diagram_start_top = "auto",
  diagram_start_left = "auto",
  diagram_width = "auto",
  diagram_height = "auto",
  margins = 0.25,
  title_font_size = 9,
  footnote_font_size = 8,
  axes_font_size = 9,
  value_font_size = 9,
  label_font_size = 9,
  origin_font_size = 8,
  other_font_size = 9,
  tooltip_font_size = 12,
  line_height = 1.1,
  space_between_bars = 0,
  bar_overlap = 0,
  line_thickness = 1,
  segment_line_thickness = 1,
  major_separation_line_thickness = 1,
  minor_separation_line_thickness = 1,
  axes_line_thickness = 1,
  guiding_line_thickness = 1,
  graphic_outline_thickness = 1,
  diagram_outline_thickness = 1,
  segment_line_length = 1,
  segment_line_offset = 0,
  textbox_width = 4
)
```

## Arguments

- graphic_width:

  The width of the whole graphic.

- graphic_height:

  The height of the whole graphic.

- diagram_start_top:

  The starting position of the main diagram within the graphic from the
  top.

- diagram_start_left:

  The starting position of the main diagram within the graphic from the
  left.

- diagram_width:

  The width of the main diagram within the graphic.

- diagram_height:

  The height of the main diagram within the graphic.

- margins:

  Inner margins to the graphic borders in cm.

- title_font_size:

  Font size of the title

- footnote_font_size:

  Font size of the footnote

- axes_font_size:

  Font size of the axes

- value_font_size:

  Font size of the values.

- label_font_size:

  Font size of the origin text.

- origin_font_size:

  Font size of the origin text.

- other_font_size:

  Font size of every other text element.

- tooltip_font_size:

  Font size of the tooltips in interactive charts.

- line_height:

  The height of a single text line.

- space_between_bars:

  The space between adjacent bars.

- bar_overlap:

  The factor determines how much bars are overlapping each other.

- line_thickness:

  The thickness of lines in a line chart or outlines of segments.

- segment_line_thickness:

  The thickness of the lines connecting segments with labels.

- major_separation_line_thickness:

  The thickness of the lines drawn between top group categories.

- minor_separation_line_thickness:

  The thickness of the lines drawn between minor group categories.

- axes_line_thickness:

  The thickness of the axes lines and ticks.

- guiding_line_thickness:

  The thickness of the axes guiding lines.

- graphic_outline_thickness:

  The thickness of the graphic outline.

- diagram_outline_thickness:

  The thickness of the diagram outline.

- segment_line_length:

  The length of the lines leading from segments to labels in cm.

- segment_line_offset:

  Offset in the height of the leading lines in cm. If 0, all leading
  lines will end at the same height, meaning the labels will be drawn at
  the same height. If an offset is set, all lines after the first one
  will be shorter by this amount \* number of the line. Enables the
  labels to be drawn in steps, if e.g. the labels have a long text which
  would overlap, if they where on the same height.

- textbox_width:

  Determines the width of any freely placed textbox.

## Value

Returns a list of named graphic options.

## See also

The main graphic function:
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md)

Graphic options:
[`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
[`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
`graphic_dimensions()`,
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
default_dimensions <- graphic_dimensions()

# Set specific options, the rest will be set to default values
custom_dimensions <- graphic_dimensions(graphic_width  = 10,
                                        graphic_height = 10)

# Apply options locally
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    dimensions     = custom_dimensions)

# Or direct
my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars,
                    dimensions     = graphic_dimensions(graphic_width  = 10,
                                                        graphic_height = 10))

# Set options globally
set_graphic_options(graphic_width  = 10,
                    graphic_height = 10)

my_data |>
     design_graphic(axes_variables = "sex",
                    segments       = "education",
                    diagram        = dg_vbars)
```
