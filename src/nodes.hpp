#ifndef _NODE_HPP
#define _NODE_HPP

#include "context_module.hpp"

#include <llvm/IR/Value.h>

class Node {
   public:
	Node() = default;

	Node(const Node &) = delete;
	Node & operator=(const Node &) = delete;

	Node(Node &&)  = default;
	Node & operator=(Node &&) = default;

	virtual ~Node() = default;

	virtual llvm::Value * codegen(context_module & context) = 0;
};

// Base classes
class Expression : public virtual Node {};
class Statement : public virtual Node {};
class Top_Level : public virtual Node {};

class Top_Level_Seq final : public Node {
   public:
	Top_Level_Seq() = default;

	Top_Level_Seq(const Top_Level_Seq&) = delete;
	Top_Level_Seq& operator=(const Top_Level_Seq &) = delete;

	Top_Level_Seq(Top_Level_Seq&&)  = default;
	Top_Level_Seq& operator=(Top_Level_Seq&&) = default;

	~Top_Level_Seq() override = default;

	template<typename... args_t>
	void append_item(args_t &&... args) {
		top_lvl_seq_.emplace_back(args...);
	}

	Top_Level_Seq & operator+=(Top_Level * item) {
		top_lvl_seq_.emplace_back(item);
		return *this;
	}

	// The return value should not be used
	llvm::Value * codegen(context_module& context) override {
		for(const auto& item : top_lvl_seq_){
			item->codegen(context);
		}

		return nullptr;
	}

   private:
	std::vector<std::unique_ptr<Top_Level>> top_lvl_seq_;
};

#endif
