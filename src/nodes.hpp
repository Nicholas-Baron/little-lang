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

class Typed_Var final {

	std::string type_;
	std::string name_;

   public:
	Typed_Var(std::string && name, std::string && type)
		: type_{type}, name_{name} {}

	const auto & name() const { return name_; }
	const auto & type() const { return type_; }
};

class Statement_Seq final : public Statement {
   public:
	Statement_Seq() = default;

	Statement_Seq(const Statement_Seq &) = delete;
	Statement_Seq & operator=(const Statement_Seq &) = delete;

	Statement_Seq(Statement_Seq &&) = default;
	Statement_Seq & operator=(Statement_Seq &&) = default;

	~Statement_Seq() override = default;

	void append(Statement * stmt) { statements.emplace_back(stmt); }

	// The return value should not be used
	llvm::Value * codegen(context_module & context) override {
		for (const auto & entry : statements) { entry->codegen(context); }

		return nullptr;
	}

   private:
	std::vector<std::unique_ptr<Statement>> statements{};
};

#endif
