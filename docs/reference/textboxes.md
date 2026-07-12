# Add Textboxes As Graphical Object

`add_textbox()`: Create a textbox as graphical object. The dimensions
and overall visual appearance is flexible. Text wraps automatically, if
the provided text is wider than the textbox itself. Textboxes can be
plugged into the "add_texts" parameter of
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md).

`get_text_width()`: Get the width of a text while applying individual
styling.

`get_text_height()`: Get the height of a text while applying individual
styling.

## Usage

``` r
add_textbox(
  text,
  x_pos = .qol_options[["graphic_dimensions"]][["margins"]],
  y_pos = .qol_options[["graphic_dimensions"]][["graphic_height"]] * 0.97,
  width = .qol_options[["graphic_dimensions"]][["textbox_width"]],
  alignment = .qol_options[["graphic_visuals"]][["other_alignment"]],
  font = .qol_options[["graphic_visuals"]][["font"]],
  font_color = .qol_options[["graphic_visuals"]][["other_font_color"]],
  font_size = .qol_options[["graphic_dimensions"]][["other_font_size"]],
  font_face = .qol_options[["graphic_visuals"]][["other_font_face"]],
  line_height = .qol_options[["graphic_dimensions"]][["line_height"]],
  rotation = 0,
  add_box = FALSE,
  box_back_color = NULL,
  box_line_color = "#000000",
  box_line_type = "solid",
  box_thickness = 1,
  name = "textbox"
)

get_text_width(text, type, dimensions, visuals, unit = "native")

get_text_height(text, type, dimensions, visuals, unit = "native")
```

## Arguments

- text:

  The text that should be displayed.

- x_pos:

  X starting position of the textbox in cm.

- y_pos:

  Y starting position of the textbox in cm. 0 position is at the top.

- width:

  Width of the textbox in cm.

- alignment:

  Horizontal text alignment.

- font:

  Name of the font to be used.

- font_color:

  Font color as hex code.

- font_size:

  Font size.

- font_face:

  Valid values are "plain", "bold", "italic", "oblique", and
  "bold.italic".

- line_height:

  The height of a single text line.

- rotation:

  Rotates the text by a number of degrees.

- add_box:

  FALSE by default. If TRUE draws a rectangle below the text.

- box_back_color:

  Background color of the textbox.

- box_line_color:

  Outline color of the textbox.

- box_line_type:

  Outline type of the textbox.

- box_thickness:

  Outline thickness of the textbox.

- name:

  The internal name of the textbox with which it can be identified.

- type:

  The type of text to be measured. Can be: title, footnote,
  primary_axes, secondary_axes, variable_axes, value, label, origin or
  other.

- dimensions:

  Dimension parameters set with
  [`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md).

- visuals:

  Visual parameters set with
  [`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md).

- unit:

  The unit to measure widths and heights in. Used are "native" for
  measuring inside the inner canvas. Otherwise use "cm".

## Value

Returns a grid::textGrob object.

`get_text_width()`: Returns a numeric width.

`get_text_height()`: Returns a numeric height.

## Examples

``` r
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

# Add individual texts to a graphic
some_textbox <- add_textbox("This is a textbox with an actual colored box.",
                            x_pos   = 12,
                            y_pos   = 7,
                            add_box = TRUE,
                            box_back_color = "#B9B9B9")
my_data |>
     design_graphic(axes_variables = "age",
                    segments       = "sex",
                    values         = weight,
                    diagram        = dg_vbars,
                    formats        = list(sex = sex., age = age.),
                    add_texts      = list(add_textbox("Hello",  3.5, 3),
                                          add_textbox("World!", 6,   5),
                add_textbox("This is a textbox with an actual colored box.",
                            x_pos   = 11.5,
                            y_pos   = 8,
                            add_box = TRUE,
                            box_back_color = "#B9B9B9")))
```
