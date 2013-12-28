# Cheapskate

This is an experimental Markdown processor.  (A cheapskate is
always in search of the best markdown.) It aims to process
Markdown efficiently and in the most forgiving possible way.
It is about five times faster than pandoc and uses a fifth the memory.
It is significantly faster (3:4) and much more accurate
than the markdown package on Hackage.

There is no such thing as an invalid Markdown document. Any
string of characters is valid Markdown.  So the processor should
finish efficiently no matter what input it gets. Garbage in
should not cause an error or exponential slowdowns.  This processor
has been tested on many large inputs consisting of random strings of
characters, with fairly constant performance proportional to the input
size. (Try `make fuzztest`.)

## Usage

As an executable:

    markdown [FILE*]

As a library:

    {-# LANGUAGE OverloadedStrings #-}
    import Cheapskate
    import Text.Blaze.Html

    html :: Html
    html = renderBlocks $ parseMarkdown "Hello *world*"

**Important note on security:** If the markdown input you are converting comes
from an untrusted source (e.g. a web form), you should *always*
sanitize the output using a library like `xss-sanitize` before displaying
it on a web page.  It is *not* sufficient to sanitize only the raw HTML
on the markdown page, as some libraries do, because this does not protect
against possible XSS attacks via link and image attributes.

## Extensions

This processor adds the following Markdown extensions:

### Hyperlinked URLs

All absolute URLs are automatically made into hyperlinks, where
inside `<>` or not.

### Fenced code blocks

Fenced code blocks with attributes are allowed.  These begin with
a line of three or more backticks or tildes, followed by an
optional language name and possibly other metadata.  They end
with a line of backticks or tildes (the same character as started
the code block) of at least the length of the starting line.

### Explicit hard line breaks

A hard line break can be indicated with a backslash before a
newline. The standard method of two spaces before a newline also
works, but this gives a more "visible" alternative.

### Backslash escapes

All ASCII symbols and punctuation marks can be backslash-escaped,
not just those with a use in Markdown.

## Revisions

In departs from the markdown syntax document in the following ways:

### Intraword emphasis

Underscores cannot be used for word-internal emphasis. This
prevents common mistakes with filenames, usernames, and indentifiers.
Asterisks can still be used if word in*ter*nal emphasis is needed.

### Ordered lists

The starting number of an ordered list is now significant.
Other numbers are ignored, so you can still use `1.` for each
list item.

In addition to the `1.` form, you can use `1)` in your ordered lists.
A new list starts if you change the form of the delimiter. So, the
following is two lists:

    1. one
    2. two
    1) one
    2) two

### Bullet lists

A new bullet lists starts if you change the bullet marker.
So, the following is two consecutive bullet lists:

    + one
    + two
    - one
    - two

### List separation

Two blank lines breaks out of a list.  This allows you to
have consecutive lists:

    - one

    - two


    - one (new list)

The blank lines break out of a list no matter how deeply it
is nested:

    - one
      - two
        - three


    - new list

### Indentation of list continuations

Block elements inside list items need not be indented four
spaces.  If they are indented beyond the bullet or numerical
list marker, they will be considered additional blocks inside
the list item.  So, the following is a list item with two paragraphs:

    - one

     two

The amount of indentation required for an indented code block
inside a list item depends on the first line of the list item.
Generally speaking, code must be indented four spaces past the
first non-space character after the list marker.  Thus:

     -   My code

             {code here}

     - My code

           {code here}

When the first line itself contains indented code, then subsequent
indented code blocks should be indented five spaces past the list
marker:

     -     { code }

           { more code }

### Raw HTML blocks

Raw HTML blocks work a bit differently than in Markdown.pl.
A raw HTML block starts with a block-level HTML tag (opening or
closing), or a comment start `<!--` or end `-->`, and goes until
the next blank line.  The whole block is included as raw HTML.
No attempt is made to parse balanced tags.  This means that
in the following, the asterisks are literal asterisks:

    <div>
    *hello*
    </div>

while in the following, the asterisks are interpreted as markdown
emphasis:

    <div>

    *hello*

    </div>

In the first example, we have a single raw HTML block; in the second,
we have two raw HTML blocks with an intervening paragraph.  This system
provides flexibility to authors to use enclose markdown sections
in html block-level tags if they wish, while also allowing them
to include verbatim HTML blocks (taking care that the don't include
any blank lines).

As a consequence of this rule, HTML blocks may not contain blank lines.

## Clarifications

This implementation resolves the following issues left vague in the markdown
syntax document:

### Tight vs. loose lists

A list is considered "tight" if (a) it has only one item or
there is no blank space between any two consecutive items, and
(b) no item has blank lines as its immediate children.
If a list is "tight," then list items consisting of a single
paragraph or a paragraph followed by a sublist will be rendered
without `<p>` tags.

### Sublists

Sublists work like other block elements inside list items;
they  must be indented past the bullet or numerical list marker
(but no more than three spaces past, or they will be interpreted
as indented code).

### ATX headers

ATX headers must have a space after the initial `###`s.

### Separation of block quotes

A blank line will end a blockquote. So, the following is a single
blockquote:

    > hi
    >
    > there

But this is two blockquotes:

    > hi

    > there

Blank lines are not required before horizontal rules, blockquotes,
lists, code blocks, or headers.  They are not required after, either,
though in many cases "laziness" will effectively require a blank
line after.  For example, in

    Hello there.
    > A quote.
    Still a quote.

the "Still a quote." is part of the block quote, because of laziness
(the ability to leave off the > from the beginning of subsequent
lines).  Laziness also affects lists. However, we can have a code
block, ATX header, or horizontal rule between two paragraphs without any
blank lines.

## Installing

To build, get the Haskell Platform, then:

    cabal update && cabal install

## Tests

The `tests` subdirectory contains an extensive suite of tests,
including all of John Gruber's original Markdown tests, plus
many of the tests from Michel Fortin's `mdtest` suite.  Each
test consists in two files with the same basename, a markdown
source and an expected HTML output.

To run the test suite, do

    make test

To run only tests that match a regex pattern, do

    PATT=Orig make test

Setting the environment variable `TIDY=1` will run the expected and
actual output through tidy before comparing them.  You can run this
test suite on another markdown processor by doing

    PROG=myothermarkdown make test

## License

Copyright &copy; 2012--3 John MacFarlane.

The library is released under the BSD license; see LICENSE for terms.

Some of the test cases are borrowed from Michel Fortin's mdtest suite
and John Gruber's original markdown test suite.
