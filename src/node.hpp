#ifndef _NODE_HPP
#define _NODE_HPP

#include "context_module.hpp"

#include <llvm/IR/Value.h>

class Node{
	public:
		Node() = default;
		Node(const Node &) = delete;
		Node& operator=(const Node &) = delete;
		Node(Node &&) = delete;
		Node& operator=(Node &&) = delete;
		virtual ~Node() = default;

		virtual llvm::Value* codegen(context_module& context) = 0;
};


#endif
