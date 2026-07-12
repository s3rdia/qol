# Set Global Graphic Options

Modify styling options used by
[`design_graphic()`](https://s3rdia.github.io/qol/reference/design_graphic.md).
Available parameters can be seen in
[`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
[`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
[`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md),
[`graphic_output()`](https://s3rdia.github.io/qol/reference/graphic_output.md)
and
[`graphic_fine_tuning()`](https://s3rdia.github.io/qol/reference/graphic_fine_tuning.md).

`set_graphic_options()` sets the graphic options globally. These options
are used by all functions involved in graphics production.

`get_graphic_options()` Prints out the currently set global graphic
options.

`reset_graphic_options()` Resets global graphic options to the default
parameters. This includes all options set with `set_graphic_options()`,
[`set_labels()`](https://s3rdia.github.io/qol/reference/style_options.md),
[`set_titles()`](https://s3rdia.github.io/qol/reference/qol_options.md)
and
[`set_footnotes()`](https://s3rdia.github.io/qol/reference/qol_options.md).

## Usage

``` r
set_graphic_options(..., save_file = NULL)

get_graphic_options(from_file = NULL)

reset_graphic_options()
```

## Arguments

- ...:

  Put in any graphic option from
  [`graphic_visuals()`](https://s3rdia.github.io/qol/reference/graphic_visuals.md),
  [`graphic_axes()`](https://s3rdia.github.io/qol/reference/graphic_axes.md),
  [`graphic_dimensions()`](https://s3rdia.github.io/qol/reference/graphic_dimensions.md),
  [`graphic_output()`](https://s3rdia.github.io/qol/reference/graphic_output.md)
  or
  [`graphic_fine_tuning()`](https://s3rdia.github.io/qol/reference/graphic_fine_tuning.md)
  with the new value.

- save_file:

  A full file path to an RDS file in which global graphic options should
  be stored.

- from_file:

  A full file path to an RDS file in which global graphic options are
  stored.

## Value

`set_graphic_options()`: Returns modified global graphic options.

`get_graphic_options()`: List of global graphic options.

`reset_graphic_options()`: Returns default global graphic options.

## Examples

``` r
# This function can process any parameter from graphic_visuals(), graphic_axes(),
# graphic_dimensions(), graphic_output() or graphic_fine_tuning() and sets the
# option globally.
set_graphic_options(title_font_size = 12,
                    label_font_size = 10)

get_graphic_options()

reset_graphic_options()
```
