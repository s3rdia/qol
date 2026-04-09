# Message Helper Functions

`get_message_stack()`: Retrieves the current message stack as list or
data frame.

`set_no_print()`: FALSE by default. If set to TRUE the messages will be
formatted and returned but not printed to the console. Can e.g. be used
in unit test situations.

`print_stack_as_messages()`: Prints the message stack as actual messages
(only not suppressed messages). Can be used to trigger expect_message,
expect_warning or expect_error in unit tests.

`convert_square_brackets()`: Convert different curly bracket patterns
into ansi formatting or passes the context of vectors into the text.

## Usage

``` r
get_message_stack(as_data_frame = FALSE)

set_no_print(value = FALSE)

print_stack_as_messages(type = NULL)

convert_square_brackets(text, ...)
```

## Arguments

- as_data_frame:

  FALSE by default. If TRUE returns message stack information as data
  frame.

- value:

  Can be TRUE or FALSE.

- type:

  The message type to filter.

- text:

  The text in which to replace the curly brackets.

- ...:

  The actual replacement vectors.

## Value

`get_message_stack()`: Returns a list of messages or a data frame.

`set_no_print()`: Returns the global no_print option.

`print_stack_as_messages()`: Returns NULL.

`convert_square_brackets()`: Returns formatted text.

## Details

The message types in which you can enter custom texts, are capable of
using different styling operators. These are:

- Insert list of elements: \[vector_name\]

- Adding conditional words, if list of elements has more than one
  element: \[?word\]

- Adding conditional singular/plural, depending on list of element
  length: \[?singular/plural\]

- Bold, italic and underline: \[b\]some text\[/b\], \[i\]some
  text\[/i\], \[u\]some text\[/u\]

- Coloring parts of the message: \[#FF00FF some text\]

## See also

Main printing functions:
[`print_message()`](https://s3rdia.github.io/qol/reference/messages.md),
[`print_headline()`](https://s3rdia.github.io/qol/reference/messages.md),
[`print_start_message()`](https://s3rdia.github.io/qol/reference/messages.md),
[`print_closing()`](https://s3rdia.github.io/qol/reference/messages.md),
[`print_step()`](https://s3rdia.github.io/qol/reference/messages.md)
