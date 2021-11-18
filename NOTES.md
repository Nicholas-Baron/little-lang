# Style Guide

The key words "must", "must not", "required", "shall", "shall not", "should", "should not",
"recommended",  "may", and "optional" in this document are to be interpreted as described in RFC 2119.

## Naming
Classes should follow the C++ standard library's capitalization rules,
that is all lowercase with underscores separating words (e.g. snakecase).

Certain abbreviations should be used in naming:
 - `stmt` for statement
 - `expr` for expression

## Namespaces
Namespaces should have their own folder and should be named like that folder.
Namespaces may have a forward declaration header,
which should contain the word "forward" in its name.
