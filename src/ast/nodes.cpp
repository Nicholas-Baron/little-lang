#include "nodes.hpp"

namespace ast {

    bool binary_expr::is_comparison() const noexcept {
        switch (op) {
        case operand::ge:
        case operand::gt:
        case operand::lt:
        case operand::le:
        case operand::eq:
        case operand::ne:
            return true;
        default:
            return false;
        }
    }

    bool binary_expr::is_shortcircuiting() const noexcept {
        return op == operand::bool_or or op == operand::bool_and;
    }

    [[nodiscard]] bool binary_expr::is_arithmetic() const noexcept {
        switch (op) {
        case operand::add:
        case operand::sub:
        case operand::mult:
        case operand::div:
        case operand::mod:
            return true;
        default:
            return false;
        }
    }

    std::shared_ptr<ast::struct_type> struct_decl::type(const std::string & module_name) const {
        std::vector<ast::struct_type::field_type> fields;
        for (const auto & typed_id : this->fields) {
            fields.emplace_back(typed_id.name(), typed_id.type());
        }
        return ast::struct_type::create(std::string{name}, module_name, std::move(fields));
    }

} // namespace ast
