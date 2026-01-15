# Build a Theme From Scratch

Build your own theme by just setting up the colors for the different
parts of RStudio. A theme file will be exported which can be added by
going to:

Tools -\> Global Options -\> Appearance -\> Add

## Usage

``` r
build_rstheme(
  file_path,
  theme_name = "qol_green",
  dark_theme = TRUE,
  editor_background = "#062625",
  editor_headline = "#3B3B3B",
  editor_font = "#C3B79D",
  toolbar = "#2E2E2E",
  tab = "#3B3B3B",
  selected_tab = "#062625",
  line_number = "#C3B79D",
  print_margin = "#3B3B3B",
  cursor = "#CCCCCC",
  selection = "#1B436E",
  smart_highlight = "#3686dc",
  bracket_highlight = "#595959",
  active_line = "#202324",
  whitespace = "#CCCCCC",
  debug_line = "#F18889",
  scrollbar = "#3B3B3B",
  scrollbar_hover = "#595959",
  scrollbar_active = "#BFBFBF",
  class_name = "#BEDD1A",
  keyword = "#FFC90E",
  language_constant = "#FFC90E",
  function_name = "#C3B79D",
  numeric = "#C93F3F",
  string = "#63C2C9",
  regex = "#E8E6E3",
  variable = "#E8E6E3",
  comment = "#32CD32",
  symbol = "#C3B79D",
  console_code = "#C3B79D",
  markdown_code = "#083332"
)
```

## Arguments

- file_path:

  The path to which the theme file should be saved.

- theme_name:

  The themes name.

- dark_theme:

  Handles some elements not covered with the other parameters.

- editor_background:

  Base background color in the editor.

- editor_headline:

  Mostly used for the headlines of the environment panel.

- editor_font:

  Base font color of the editor.

- toolbar:

  Base toolbar and frame color.

- tab:

  Color of inactive tabs.

- selected_tab:

  Color of active tabs.

- line_number:

  The color of the line numbers on the left.

- print_margin:

  Color of the vertical line showing the print margin.

- cursor:

  Cursor color.

- selection:

  The background color of the current selection.

- smart_highlight:

  Background color of smart highlighted words.

- bracket_highlight:

  Background color of highlighted bracket pairs.

- active_line:

  Color for the active line the cursor is in.

- whitespace:

  Color for whitespace characters.

- debug_line:

  Color of the current debug line.

- scrollbar:

  Color of the scrollbars.

- scrollbar_hover:

  Highlight color when hovering over a scrollbar.

- scrollbar_active:

  Highlight color when clicking on a scrollbar.

- class_name:

  Code color for class names (like package names).

- keyword:

  Code color for fixed keywords (like function, if, else).

- language_constant:

  Code color for language constants (like the @ keywords).

- function_name:

  Code color for base and package functions.

- numeric:

  Code color for numeric values.

- string:

  Code color for string values.

- regex:

  Code color for regex expressions.

- variable:

  Code color for variables, parameters and arguments.

- comment:

  Code color for comments.

- symbol:

  Code Color of symbols (like \<-, brackets).

- console_code:

  Color of executed Code in the Console.

- markdown_code:

  Background color of code passages in a markdown file.

## Value

Saves a complete theme file.

## Details

In the 'SAS Enterprise Guide' the user is able to not only choose a
given theme, but to also pick the colors for the different parts of the
editor by themselves. Everyone has a different taste of what colors look
pleasing to the eyes, so you should be able to choose them by yourself.

## Examples

``` r
# Example export file paths
# NOTE: These tempfiles are only for the examples. In reality you just call the
# main function and put in your desired path and name directly.
temp_file <- tempfile(fileext = ".rstheme")
file_name <- basename(tools::file_path_sans_ext(temp_file))

# Example theme
build_rstheme(file_path         = dirname(temp_file),
              theme_name        = file_name,
              editor_background = "#417291",
              editor_headline   = "#602BCA",
              editor_font       = "#C75C48")

# Manual cleanup for example
unlink(temp_file)
```
