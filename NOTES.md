# Style Guide

The key words "must", "must not", "required", "shall", "shall not", "should", "should not",
"recommended", "may", and "optional" in this document are to be interpreted as described in [RFC 2119](https://datatracker.ietf.org/doc/html/rfc2119).

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

## Resources
- [CPP Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)

Note: none of the above mentioned resources are authoritative, but their advice is recommended.

# Design Patterns

## Visitor Pattern
The visitor pattern is used for reading the AST.
It is needed because different visitor produce different results
and need to walk the tree differently.

This entails a few things about the node classes:
1. All nodes must override the `accept` member function. There is a `make_visitable;` macro to help with this.
2. All data in a node must be public. This allows visitors to easily access whatever they may need.
3. All nodes must delete their copy constructors and assignment operators. There is a `non_copyable` macro to help with this.

Note: Number 2 in the previous list may not be a permanent rule.

### Returning Values from Child Nodes
Many visitors need to return values up the tree,
as parent nodes need to know some extra or new information about their children.
In this case, there is a `value_getter` template base class that should be inherited from.

To get a value from the child, call `value_getter::get_value` with the child and a "context".
The context can be yourself (`*this`) or a fresh context.

### Modifying the Visited Nodes
There are cases where the visited nodes (i.e. the AST) need to be modified.
In this case, the basic `value_getter::store_result` is not useful,
as it knows rather little of the node type.

To simultaneously return a value and modify a node,
overload `store_result` on the visitor to take both the result and the node that should be modified.

### Resources
 - [Visitor with Return Values](https://www.codeproject.com/Tips/1018315/Visitor-with-the-Return-Value)
