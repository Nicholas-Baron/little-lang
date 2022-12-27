#include "nodes.hpp"

#include "operations.hpp"
#include "top_lvl_nodes.hpp"

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

    std::shared_ptr<ast::function_type> func_decl::func_type() const {

        std::vector<ast::type_ptr> param_types;
        for (const auto & param : params) { param_types.push_back(param.type()); }

        return ast::function_type::create(ret_type, std::move(param_types));
    }
} // namespace ast
