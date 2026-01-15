# Convert Function Arguments to Character Vector

`args_to_char()`: Converts any argument passed as a single character or
symbol as well as character vectors or vector of symbols back as
character vector.

`dots_to_char()`: When you define a function and want the user to be
able to pass variable names without the need to have them stored in a
vector c() or list() beforehand and without putting the names into
quotation marks, you can convert this variable list passed as ... into a
character vector.

Note: If the user passes a list of characters it is returned as given.

`get_origin_as_char()` is a wrapper that allows to retrieve the original
contents of the provided variable, whether called directly or nested in
multiple function calls, as a character vector.

## Usage

``` r
args_to_char(argument)

dots_to_char(...)

get_origin_as_char(original, substituted)
```

## Arguments

- argument:

  Function argument to convert.

- ...:

  Used for variable names listed in ... without the need to put them in
  c() or list().

- original:

  The data frame which contains the columns to be checked.

- substituted:

  The grouping variables which potentially form unique combinations.

## Value

Returns a character vector.

## Examples

``` r
# Example function with function parameter
print_vnames <- function(parameter){
    var_names <- args_to_char(substitute(parameter))
    print(var_names)
}

print_vnames(age)
print_vnames("age")
print_vnames(c(age, sex, income, weight))
print_vnames(c("age", "sex", "income", "weight"))

# You can also pass in a character vector, if you have stored variable names elsewhere
var_names <- c("age", "sex", "income", "weight")
print_vnames(var_names)

# If you plan to use the function within other functions, better use get_origin_as_char()
print_vnames <- function(parameter){
    var_names <- get_origin_as_char(parameter, substitute(parameter))
    print(var_names)
}

another_function <- function(parameter){
    print_vnames(parameter)
}

another_function("age")
another_function(c("age", "sex", "income", "weight"))

# Example function with ellipsis
print_vnames <- function(...){
    var_names <- dots_to_char(...)
    print(var_names)
}

print_vnames(age)
print_vnames("age")
print_vnames(age, sex, income, weight)
print_vnames("age", "sex", "income", "weight")

# You can also pass in a character vector, if you have stored variable names elsewhere
var_names <- c("age", "sex", "income", "weight")
print_vnames(var_names)
```
