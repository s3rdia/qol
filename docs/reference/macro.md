# Resolve Macro Variables In A Text

Variables written with a leading "&" inside a text will be resolved into
the character or numeric expression the corresponding object is holding.

## Usage

``` r
macro(text)

apply_macro(character_vector)
```

## Arguments

- text:

  A text containing macro variables to resolve.

- character_vector:

  A vector containing multiple character expressions in which to resolve
  macro variables.

## Value

`macro()`: Returns a character.

`apply_macro()`: Returns a character vector.

## Details

Macro variables in 'SAS' can be set up with %Let and can act as global
accessible variables. This in itself is nothing special to 'R' because
basically every object created outside a function is a global variable.

To use these global objects within a text one has to use e.g.
paste0("The current year is: ", year). With the macro function one can
write it like this: macro("The current year is &year").

So where is the benefit? If implemented within a function, a parameter
like "title" or "footnote" in the tabulation functions, can resolve
these variables without the need of another function. You can just pass
the character expression "The current year is &year" and the
implementation inside the function resolves the macro variable directly
in place.

## See also

Functions that can make use of macro variables:
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md),
[`frequencies()`](https://s3rdia.github.io/qol/reference/frequencies.md),
[`crosstabs()`](https://s3rdia.github.io/qol/reference/crosstabs.md),
[`export_with_style()`](https://s3rdia.github.io/qol/reference/export_with_style.md),
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md),
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md)

Within the tabulation functions titles, footnotes, var_labels and
stat_labels can resolve macros. Additionally they can be used in the
[`any_table()`](https://s3rdia.github.io/qol/reference/any_table.md)
rows and columns parameter, in the
[`summarise_plus()`](https://s3rdia.github.io/qol/reference/summarise_plus.md)
types parameter and in the
[`transpose_plus()`](https://s3rdia.github.io/qol/reference/transpose_plus.md)
pivot parameter.

## Examples

``` r
# Resolving macro variable in single character
year <- 2026

text <- macro("The current year is &year")

# You can also combine multiple macro variables
some_variable <- "current"
current2026   <- "The current year is"

text_combi <- macro("&&some_variable&year &year")

# Resolving macro variable in character vector
char_vector <- c("The current year is &year",
                 "The &some_variable year is &year",
                 "&&some_variable&year &year")

text_vector <- apply_macro(char_vector)
```
