# Retrieve A Substring From A Character

`sub_string()` can extract parts of a character from the left side,
right side or from the middle. It is also able to start or end at
specific letter sequences instead of positions.

## Usage

``` r
sub_string(data_frame, variable, from = NULL, to = NULL, case_sensitive = TRUE)
```

## Arguments

- data_frame:

  A data frame which contains the variables to concatenate.

- variable:

  A character variable to extract parts from.

- from:

  The names of the variables to concatenate.

- to:

  A single character which will be used to fill up the empty places.

- case_sensitive:

  TRUE by default. When a character expression is passed as from or to
  it makes a difference whether a letter is written in upper or lower
  case. Pass FALSE to handle upper and lower case equaly.

## Value

Returns parts of a character vector.

## See also

Other character manipulating functions:
[`concat()`](https://s3rdia.github.io/qol/reference/concat.md),
[`remove_blanks()`](https://s3rdia.github.io/qol/reference/remove_blanks.md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(100)

# Extract text from the left
my_data[["left"]] <- my_data |> sub_string(education, to = 2)

# Extract text from the right
my_data[["right"]] <- my_data |> sub_string(education, from = 2)

# Extract text from the middle
my_data[["middle"]] <- my_data |> sub_string(education, from = 2, to = 3)

# Find text and extract from the left
my_data[["left2"]] <- my_data |> sub_string(education, to = "l")

# Find text and extract from the right
my_data[["right2"]] <- my_data |> sub_string(education, from = "l")

# Find text and extract from the middle
my_data[["middle2"]] <- my_data |> sub_string(education, from = "i", to = "l")
```
