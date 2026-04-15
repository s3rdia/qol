# Print Styled Messages

Printing styled messages for different occasions. There are
notifications, warnings, errors, function call headlines, progress and
function closing messages. Or just a neutral one.

## Usage

``` r
print_message(
  type,
  text,
  ...,
  always_print = FALSE,
  utf8 = .qol_messages[["format"]][["utf8"]]
)

print_headline(
  text,
  ...,
  line_char = "=",
  max_width = getOption("width"),
  always_print = FALSE
)

print_start_message(
  current_time = Sys.time(),
  caller_color = "#63C2C9",
  always_print = FALSE,
  suppress = FALSE
)

print_closing(
  time_threshold = 2,
  start_time = .qol_messages[["start_time"]],
  caller_color = "#63C2C9",
  always_print = FALSE,
  suppress = FALSE
)

print_step(
  type,
  text,
  ...,
  always_print = FALSE,
  utf8 = .qol_messages[["format"]][["utf8"]]
)

set_up_custom_message(
  ansi_icon = "🦄",
  text_icon = "^",
  indent = 1,
  type = "UNICORN",
  color = "#FF00FF",
  text_bold = FALSE,
  text_italic = FALSE,
  text_underline = FALSE,
  text_color = NULL
)
```

## Arguments

- type:

  If displayed as a normal note, then this is the text displayed in
  front of the message. This also appears as type in the message stack.

- text:

  The message text to display.

- ...:

  Additional information to display like variable names. To use these
  write \[\] in the text.

- always_print:

  FALSE by default. If TRUE, prints headlines even in deeper nested
  situations.

- utf8:

  Whether to display complex characters or just plain text.

- line_char:

  The character that that forms the line.

- max_width:

  The maximum number of characters drawn, which determines the line
  length of the headline.

- current_time:

  The current time to create a time stamp.

- caller_color:

  Hex color code for the displayed caller function.

- suppress:

  FALSE by default. If TRUE triggers all the message procedures to
  create a message stack but doesn't print the message to the console.

- time_threshold:

  The total time spent is displayed in different colors from green over
  yellow to red, depending on the threshold specified (in seconds).

- start_time:

  The time at which the function call started to calculate the time
  difference and output the total time spent.

- ansi_icon:

  The icon used when message is displayed in utf8 mode.

- text_icon:

  The icon used when message is displayed in text only mode.

- indent:

  How many spaces to indent the message.

- color:

  The color of the message type.

- text_bold:

  FALSE by default. If TRUE prints the message text in bold letters.

- text_italic:

  FALSE by default. If TRUE prints the message text in italic letters.

- text_underline:

  FALSE by default. If TRUE prints the message text underlined.

- text_color:

  The color of the actual message text.

## Value

Return text without styling or total running time.

`set_up_custom_message()`: Returns a list.

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

Also have a look at the small helpers:
[`get_message_stack()`](https://s3rdia.github.io/qol/reference/message_helpers.md),
[`set_no_print()`](https://s3rdia.github.io/qol/reference/message_helpers.md),
[`print_stack_as_messages()`](https://s3rdia.github.io/qol/reference/message_helpers.md),
[`convert_square_brackets()`](https://s3rdia.github.io/qol/reference/message_helpers.md)

## Examples

``` r
# Example messages
print_message("NOTE", c("Just a quick note that you can also insert e.g.[? a / ]variable",
                        "name[?s] like this: [listing].",
                        "Depending on the number of variables you can also alter the text."),
             listing = c("age", "state", "NUTS3"))

print_message("WARNING", "Just a quick [#FF00FF colored warning]!")

print_message("ERROR", "Or an error")

print_message("NEUTRAL", c("You can also just output [u]plain text[/u] if you like and use",
                           "[#FFFF00[b]all the different[/b] [i]formatting options.[/i]]"))

# Different headlines
print_headline("This is a headline")

print_headline("[#00FFFF This is a different headline] with some color",
               line_char = "-")

print_headline("[b]This is a very small[/b] and bold headline",
               line_char = ".",
               max_width = 60)

# Messages with time stamps
test_func <- function(){
    print_start_message()
    print_step("GREY", "Probably not so important")
    print_step("MAJOR", "This is a major step...")
    print_step("MINOR", "Sub step1")
    print_step("MINOR", "Sub step2")
    print_step("MINOR", "Sub step3")
    print_step("MAJOR", "[b]Finishing... [/b][#00FFFF with some color again!]")
    print_closing()
}

test_func()

# See what is going on in the message stack
message_stack <- get_message_stack()

# Set up a custom message
hotdog <- set_up_custom_message(ansi_icon = "\U0001f32d",
                                text_icon = "IOI",
                                indent    = 1,
                                type      = "HOTDOG",
                                color     = "#B27A01")

hotdog_print <- function(){
    print_start_message()
    print_message(hotdog, c("This is the first hotdog message! Hurray!",
                            "And it is also multiline in this version."))
    print_step(hotdog, "Or use as single line message with time stamps.")
    print_step(hotdog, "Or use as single line message with time stamps.")
    print_step(hotdog, "Or use as single line message with time stamps.")
    print_closing()
}

hotdog_print()

# See new message in the message stack
hotdog_stack <- get_message_stack()
```
