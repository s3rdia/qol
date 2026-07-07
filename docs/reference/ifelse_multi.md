# Do Multiple ifelse At Once

`ifelse_multi()` handles multiple conditions and value assignments at
once by nesting multiple ifelse statements. It is special in the case
that it takes the conditions as unevaluated characters to be able to
parse them before evaluation. This enables a SAS like writing style of
the conditions.

## Usage

``` r
ifelse_multi(data_frame, ..., else. = NA, do_if = NULL, na.rm = TRUE)
```

## Arguments

- data_frame:

  A data frame on which to apply multiple ifelse statements.

- ...:

  The conditions as character and the assignments in the form:
  "condition" = value, "condition" = value, ...

- else.:

  The default value which is applied when no condition results in an
  assignment.

- do_if:

  Define an overarching condition that will be used on all other
  conditions.

- na.rm:

  TRUE by default. Sets the default value even though a condition
  results in NA. If FALSE leaves NA as missing value.

## Value

Returns a vector with a conditionally computed variable.

## Details

The function takes in conditions as characters to be able to parse the
conditions. The parsing allows to write conditions in a SAS like way.
For example the condition

"age \>= 15 & age \< 65" can be written as "15 \<= age \< 65"

Additionally and/or (case insensitive) are recognized keywords which
will be translated into &/\|.

"age \>= 15 and age \< 65 or sex == 1" becomes "age \>= 15 & age \< 65
\| sex == 1"

And [`macro()`](https://s3rdia.github.io/qol/reference/macro.md)
variables are allowed in any spot, e.g.:

YEAR \<- 2025\\ "year == &YEAR" translates to "year == 2025".

Single = get translated to ==, the SAS not ^= is translated to !=.

In SAS you can write "age in (1 4 7 15 21)" or "age not in (2, 4, 37,
82)" which gets translated into "age %in% (1, 4, 7, 15, 21)" and "!(age
%in% (2, 4, 37, 82))".

When using == or != operators you can check if character expressions
start with (text:), end with (:text) or contain a certain text (:text:).
These also get translated.

## See also

Conditions:
[`if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else_if.()`](https://s3rdia.github.io/qol/reference/if_else.md),
[`else.()`](https://s3rdia.github.io/qol/reference/if_else.md)

Filter Data Frame:
[`where.()`](https://s3rdia.github.io/qol/reference/where..md)

Create new Variables:
[`compute.()`](https://s3rdia.github.io/qol/reference/compute..md)

Resolve macro variables:
[`macro()`](https://s3rdia.github.io/qol/reference/macro.md),
[`apply_macro()`](https://s3rdia.github.io/qol/reference/macro.md)

## Examples

``` r
# Example data frame
my_data <- dummy_data(100)

# Simple ifelse statement
my_data[["under18"]]    <- my_data |> ifelse_multi(" age < 18 " = 1,       else. = 0)
my_data[["middle_age"]] <- my_data |> ifelse_multi(" 15 <= age < 65 " = 1, else. = 0)

my_data[["age_gr"]] <- my_data |> ifelse_multi(" age < 18 "       = "under 18",
                                               " 18 <= age < 25 " = "18 to under 25",
                                               " 25 <= age < 50 " = "25 to under 50",
                                               " 50 <= age < 65 " = "50 to under 65",
                                               " 65 <= age      " = "65 and more")

# With overarching do_if condition
my_data[["age_gr_edu"]] <- my_data |> ifelse_multi(do_if = " education in ('middle' 'high') ",
                                                       " age < 18 "       = "under 18",
                                                       " 18 <= age < 25 " = "18 to under 25",
                                                       " 25 <= age < 50 " = "25 to under 50",
                                                       " 50 <= age < 65 " = "50 to under 65",
                                                       " 65 <= age      " = "65 and more")

# And/or translation
my_data[["and"]] <- my_data |> ifelse_multi(" age > 65 and sex = 1 " = 1,
                                            " age > 65 and sex = 2 " = 2,
                                            else. = 0)

my_data[["or"]] <- my_data |> ifelse_multi(" age > 65 or sex = 1 " = 1,
                                           " age > 65 or sex = 2 " = 2,
                                           else. = 0)

# "in" translation
my_data[["in"]] <- my_data |> ifelse_multi(" age in (1 10 25 65 90) " = 1, else. = 0)

# Colon translation: start/ends with and contains
my_data[["start"]]    <- my_data |> ifelse_multi(" education == 'lo:' "  = 1, else. = 0)
my_data[["end"]]      <- my_data |> ifelse_multi(" education == ':le' "  = 1, else. = 0)
my_data[["contains"]] <- my_data |> ifelse_multi(" education == ':ig:' " = 1, else. = 0)

# Macro variables can be integrated in any place
variable     <- "age"
age_to_check <- 18
value_to_set <- "under 18"

my_data[["macro"]] <- my_data |> ifelse_multi(" &variable < &age_to_check " = "&value_to_set",
                                              else. = "other")

# NA translation
my_data[["NA"]]    <- my_data |> ifelse_multi(" age == .       " = 1, else. = 0)
my_data[["NotNA"]] <- my_data |> ifelse_multi(" education != . " = 1, else. = 0)
```
