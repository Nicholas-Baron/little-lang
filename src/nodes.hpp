#ifndef _NODE_HPP
#define _NODE_HPP

#include "context_module.hpp"

#include <llvm/IR/Value.h>

// Classes that do not need Node
class Typed_Var final {

	std::string type_;
	std::string name_;

   public:
	Typed_Var(std::string && name, std::string && type)
		: type_{type}, name_{name} {}

	const auto & name() const { return name_; }
	const auto & type() const { return type_; }
};

class Func_Header final {
   public:
	Func_Header(std::string && name, std::vector<Typed_Var> && parameters)
		: name_(std::move(name)), params(std::move(parameters)) {}

	void set_ret_type(std::string && type) { ret_type = type; }

	llvm::FunctionType * full_type(context_module & context);

	const Typed_Var &   arg(unsigned index) const { return params.at(index); }
	const std::string & name() const { return name_; }

   private:
	std::vector<llvm::Type *> param_types(context_module & context);

	std::string			   name_;
	std::vector<Typed_Var> params;
	std::string			   ret_type{};
};

// Basic Node
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

// Direct from Node classes
class Top_Level_Seq final : public Node {
   public:
	Top_Level_Seq() = default;

	Top_Level_Seq(const Top_Level_Seq &) = delete;
	Top_Level_Seq & operator=(const Top_Level_Seq &) = delete;

	Top_Level_Seq(Top_Level_Seq &&) = default;
	Top_Level_Seq & operator=(Top_Level_Seq &&) = default;

	~Top_Level_Seq() override = default;

	void append(Top_Level * item) { top_lvl_seq_.emplace_back(item); }

	// The return value should not be used
	llvm::Value * codegen(context_module & context) override {
		for (const auto & item : top_lvl_seq_) {
			assert(item != nullptr);
			item->codegen(context);
		}
		return nullptr;
	}

   private:
	std::vector<std::unique_ptr<Top_Level>> top_lvl_seq_;
};

// Expression classes

class UserValue final : public Expression {
   public:
	UserValue(std::string && value) : val(value) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	std::string val;
};

class UnaryExpression final : public Expression {
   public:
	UnaryExpression(int token, Expression * operand)
		: tok(token), expr(operand) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	int							tok;
	std::unique_ptr<Expression> expr;
};

class BinaryExpression final : public Expression {
   public:
	BinaryExpression(Expression * lhs, int op, Expression * rhs)
		: lhs_(lhs), rhs_(rhs), tok(op) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	std::unique_ptr<Expression> lhs_, rhs_;
	int							tok;
};

// The FunctionCall class needs to be either a Expression or Statement
// because calling a function could be a statement or part of an expression

class FunctionCall final : public Statement, public Expression {
   public:
	FunctionCall(std::string && name, std::vector<Expression *> && args)
		: name_(name) {
		for (auto * arg : args) { args_.emplace_back(arg); }
	}

	llvm::Value * codegen(context_module & context) override;

   private:
	std::string								 name_;
	std::vector<std::unique_ptr<Expression>> args_{};
};

// Statement classes

class If_Statement final : public Statement {
   public:
	If_Statement(Expression * cond, Statement * on_true, Statement * on_false)
		: condition(cond), true_branch(on_true), else_branch(on_false) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	std::unique_ptr<Expression> condition;
	std::unique_ptr<Statement>  true_branch;
	std::unique_ptr<Statement>  else_branch;
};

class Let_Statement final : public Statement {
   public:
	Let_Statement(std::string && name, Expression * value)
		: name_and_type(std::forward<std::string>(name), "auto")
		, value_(value) {}
	Let_Statement(Typed_Var && typed_name, Expression * value)
		: name_and_type(typed_name), value_(value) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	Typed_Var					name_and_type;
	std::unique_ptr<Expression> value_;
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

class Return_Statement final : public Statement {
   public:
	explicit Return_Statement(Expression * val = nullptr) : value(val) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	std::unique_ptr<Expression> value;
};

// Top Level classes
class Function final : public Top_Level {
   public:
	Function(Func_Header && head, Statement * body)
		: head_(std::move(head)), body_(body) {}

	llvm::Value * codegen(context_module & context) override;

   private:
	Func_Header				   head_;
	std::unique_ptr<Statement> body_;
};

#endif
