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
which must contain the word "forward" in its name.

## Errors
Write what is expected first, and then what was found.

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
2. All data in a node should be public. This allows visitors to easily access whatever they may need.
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
In this case, the basic `value_getter::get_value` is not useful,
as it knows rather little of the node type.

To simultaneously return a value and modify a node,
overload `store_result` on the visitor to take both the result and the node that should be modified.
In the new `store_result`, modify the node as necessary and,
at some point, call `value_getter::store_result` to return the value to the parent node.

### Resources
 - [Visitor with Return Values](https://www.codeproject.com/Tips/1018315/Visitor-with-the-Return-Value)

## `std::shared_ptr`

Using `std::shared_ptr` is a possible [anti-pattern](https://ddanilov.me/shared-ptr-is-evil/).
Additionally, reference cycles are a persistent worry surrounding their usage.
Data structures using `std::shared_ptr` thus should ensure that they can form a directed acyclic graph (DAG).

This safety mechanism may not last forever.
As such, single owner systems are preferable.
Previous work within the repo are the `control_flow::graph` and the `ast::type_context`,
the former of which is cyclic due to value and control passing between nodes.

# Compiler Internals

## Control Flow Graph

The control flow graph is used to typecheck programs and is the intermediate stage before LLVM.


Most nodes are possibly values.
However, there is no explicit marking of this fact due to certain nodes not "making sense" in a value position.
The list of such cases:
- `function_start` only makes sense as the callee of a `function_call` node.
NOTE: This may change when function pointers get implemented.
- `function_end` is not a value, as it marks the `return` statement of a function.
- `phi` is only a value iff all of its previous nodes have the same type.
