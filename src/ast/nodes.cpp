#include "nodes.hpp"

#include "operations.hpp"

namespace ast {

    bool binary_expr::is_comparison() const noexcept { return operation::is_comparison(op); }

    bool binary_expr::is_shortcircuiting() const noexcept {
        return operation::is_shortcircuiting(op);
    }

    [[nodiscard]] bool binary_expr::is_arithmetic() const noexcept {
        return operation::is_arithmetic(op);
    }

    std::shared_ptr<ast::struct_type> struct_decl::type(const std::string & module_name) const {
        std::vector<ast::struct_type::field_type> fields;
        for (const auto & typed_id : this->fields) {
            fields.emplace_back(typed_id.name(), typed_id.type());
        }
        return ast::struct_type::create(std::string{name}, module_name, std::move(fields));
    }

} // namespace ast
