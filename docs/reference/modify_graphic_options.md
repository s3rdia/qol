# Modify Graphic Options

If options are stored in an object, it can be modified afterwards. In
this case only the options to modify will be adjusted while the rest
stays as is.

## Usage

``` r
modify_graphic_options(graphic_option_to_modify, ...)
```

## Arguments

- graphic_option_to_modify:

  A pre created graphic option object where only certain elements should
  be modified while the rest is kept as is.

- ...:

  Pass in names and corresponding new values for existing graphic
  elements.

## Value

Returns a modified list of named graphic options.

## Examples

``` r
# Set specific options, the rest will be set to default values
custom_dimensions <- graphic_dimensions(graphic_width  = 10,
                                        graphic_height = 10)

# Modify the previously created graphic options
custom_dimensions <- custom_dimensions |> modify_graphic_options(title_font_size = 12)
```
