# Converts Numbers into 'Excel' Ranges

Converts a column number into the according letter to form a cell
reference like it is used in 'Excel' (e.g "A1"). Also can compute a
range from cell to cell (e.g. "A1:BY22").

## Usage

``` r
get_excel_range(
  row = NULL,
  column = NULL,
  from_row = NULL,
  from_column = NULL,
  to_row = NULL,
  to_column = NULL
)
```

## Arguments

- row:

  Single row number.

- column:

  Single column number.

- from_row:

  Range start row.

- from_column:

  Range start column.

- to_row:

  Range end row.

- to_column:

  Range end column.

## Value

Returns a character with an 'Excel' range.

## Examples

``` r
single_cell <- get_excel_range(row = 1, column = 6)
range       <- get_excel_range(from_row = 1, from_column = 6,
                                 to_row = 5,   to_column = 35)
```
