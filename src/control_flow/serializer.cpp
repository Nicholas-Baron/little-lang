#include "serializer.hpp"

#include "control_flow/node.hpp"
#include "nlohmann/json.hpp"
#include "token_to_string.hpp"

#include <iostream>

namespace control_flow {

    serializer::serializer() = default;

    serializer::~serializer() noexcept = default;

    void serializer::into_stream(std::ostream & stream, const std::vector<node *> & roots,
                                 bool human_readable) {
        serializer visitor{};
        for (auto * node : roots) {
            node->accept(visitor);
            visitor.drop_result();
        }

        nlohmann::json result;
        for (auto & item : visitor.graph_array) { result.push_back(*item); }
        stream << result.dump(human_readable ? 4 : -1);

#ifdef DEBUG
        stream << "digraph {\n";
        for (auto i = 0UL; i < result.size(); ++i) {
            auto & node = result[i];
            if (node.contains("next")) {
                stream << i << " -> " << node["next"] << ";\n";
            } else if (node.contains("condition")) {
                stream << i << " -> " << node["true case"] << ";\n";
                stream << i << " -> " << node["false case"] << ";\n";
            }
        }
        stream << "}\n";
#endif
    }

    std::pair<nlohmann::json *, size_t> serializer::add_node(node * node) {
        if (auto iter = visited.find(node); iter != visited.end()) {
            return {nullptr, iter->second};
        }

        auto & result = graph_array.emplace_back(std::make_unique<nlohmann::json>());
        visited.emplace(node, &result - graph_array.data());
        return {result.get(), &result - graph_array.data()};
    }

    void serializer::store_result(nlohmann::json * value) {
        auto iter = std::find_if(graph_array.begin(), graph_array.end(),
                                 [&value](auto & item) -> bool { return item.get() == value; });
        return value_getter::store_result(std::distance(graph_array.begin(), iter));
    }

    void serializer::visit(function_start & function_start) {
        auto [result, index] = add_node(&function_start);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {
            {"arg count", function_start.arg_count},
            {"exported", function_start.exported},
            {"next", get_value(*function_start.next, *this)}
        };
        return store_result(result);
    }

    void serializer::visit(function_end & function_end) {
        auto [result, index] = add_node(&function_end);
        if (result == nullptr) { return value_getter::store_result(index); }

        auto value = nlohmann::json{};
        if (function_end.value != nullptr) { value = get_value(*function_end.value, *this); }

        *result = {
            {"value", std::move(value)}
        };
        return store_result(result);
    }

    void serializer::visit(binary_operation & binary_operation) {
        auto [result, index] = add_node(&binary_operation);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {
            {"next", get_value(*binary_operation.next, *this)},
            {"left", get_value(*binary_operation.lhs, *this)},
            {"right", get_value(*binary_operation.rhs, *this)},
            {"op", token_to_string(binary_operation.op)}
        };
        return store_result(result);
    }

    void serializer::visit(branch & branch) {
        auto [result, index] = add_node(&branch);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {
            {"condition",  get_value(*branch.condition_value, *this)},
            {"true case",  get_value(*branch.true_case,       *this)},
            {"false case", get_value(*branch.false_case,      *this)},
        };
        return store_result(result);
    }

    void serializer::visit(constant & constant) {
        auto [result, index] = add_node(&constant);
        if (result == nullptr) { return value_getter::store_result(index); }

        auto value = std::visit(
            [](auto arg) -> nlohmann::json {
                using T = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<T, std::monostate>) {
                    return {};
                } else {
                    return arg;
                }
            },
            constant.value);

        *result = {
            {"value", std::move(value)},
            {"type", token_to_string(constant.val_type)},
            {"next", get_value(*constant.next, *this)}
        };
        return store_result(result);
    }

    void serializer::visit(function_call & function_call) {
        auto [result, index] = add_node(&function_call);
        if (result == nullptr) { return value_getter::store_result(index); }

        nlohmann::json::array_t args;
        for (auto * arg : function_call.arguments) { args.push_back(get_value(*arg, *this)); }

        *result = {
            {"next", get_value(*function_call.next, *this)},
            {"arguments", std::move(args)},
            {"callee", function_call.callee->name}
        };
        return store_result(result);
    }

    void serializer::visit(intrinsic_call & intrinsic_call) {
        auto [result, index] = add_node(&intrinsic_call);
        if (result == nullptr) { return value_getter::store_result(index); }

        nlohmann::json::array_t args;
        for (auto * arg : intrinsic_call.arguments) { args.push_back(get_value(*arg, *this)); }

        *result = {
            {"next", get_value(*intrinsic_call.next, *this)},
            {"arguments", std::move(args)},
            {"callee", intrinsic_call.name}
        };
        return store_result(result);
    }

    void serializer::visit(member_access & member_access) {

        auto [result, index] = add_node(&member_access);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {
            {"next", get_value(*member_access.next, *this)},
            {"member", member_access.member_name},
            {"lhs", get_value(*member_access.lhs, *this)}
        };

        return store_result(result);
    }

    void serializer::visit(phi & phi) {
#ifdef DEBUG
        // DEBUG: Print the visited set
        {
            nlohmann::json::object_t debug;
            for (const auto & [node, id] : visited) {
                // NOLINTNEXTLINE
                auto raw_addr = reinterpret_cast<std::uintptr_t>(node);
                auto hex_addr
                    = (std::stringstream{} << std::setfill('0') << std::showbase << std::hex
                                           << std::setw(sizeof(raw_addr) * 2) << raw_addr)
                          .str();
                debug.emplace(hex_addr, id);
            }

            std::cout << nlohmann::json{debug}.dump(4) << std::endl;
        }
#endif

        auto [result, index] = add_node(&phi);
        if (result == nullptr) { return value_getter::store_result(index); }

        nlohmann::json::array_t args;
        for (auto * arg : phi.previous) { args.push_back(get_value(*arg, *this)); }

        *result = {
            {"next", get_value(*phi.next, *this)},
            {"arguments", std::move(args)},
        };

        return store_result(result);
    }

    void serializer::visit(struct_init & struct_init) {
        auto [result, index] = add_node(&struct_init);
        if (result == nullptr) { return value_getter::store_result(index); }

        nlohmann::json::array_t fields;
        for (auto [name, value] : struct_init.fields) {
            fields.push_back(nlohmann::json::object_t{
                {"value", get_value(*value, *this)},
                {"name", name}
            });
        }

        *result = {
            {"type name",    struct_init.result_type->user_name()},
            {"initializers", std::move(fields)                   }
        };

        return store_result(result);
    }

    void serializer::visit(unary_operation & unary_operation) {

        auto [result, index] = add_node(&unary_operation);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {
            {"next", get_value(*unary_operation.next, *this)},
            {"op", token_to_string(unary_operation.op)},
            {"operand", get_value(*unary_operation.operand, *this)}
        };
        return store_result(result);
    }
} // namespace control_flow
