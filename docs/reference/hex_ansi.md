# Convert Color Codes

`hex_to_256()`: Generate a 256-color 6x6x6 color cube.

`hex_to_ansi()`: Applies hex color and font weight to a text as ansi
codes.

## Usage

``` r
hex_to_256(hex_color)

hex_to_ansi(text, hex_color = NULL, bold = FALSE)
```

## Arguments

- hex_color:

  The hex color code to convert.

- text:

  The text which contains hex color codes to be convert into ansi style
  formatting.

- bold:

  FALSE by default. If TRUE inserts ansi code for bold printing

## Value

`hex_to_256()`: Returns 6x6x6 color cube.

`hex_to_ansi()`: Returns formatted text.

## Examples

``` r
color_cube <- hex_to_ansi("#32CD32")

formatted_text <- hex_to_ansi("This is a text to test the coloring",
                              hex_color = "#32CD32", bold = TRUE)
```
