#include "serializer.hpp"

#include "control_flow/node.hpp"
#include "nlohmann/json.hpp"

namespace control_flow {

    serializer::serializer() = default;

    serializer::~serializer() noexcept = default;

    void serializer::into_stream(std::ostream & stream, const std::vector<node *> & roots,
                                 bool human_readable) {
        serializer visitor{};
        for (auto * node : roots) { node->accept(visitor); }
        stream << nlohmann::json{visitor.graph_array}.dump(human_readable ? 4 : -1);
    }

    void serializer::store_result(nlohmann::json && value, node * source) {
        if (auto iter = visited.find(source); iter != visited.end()) {
            return value_getter::store_result(iter->second);
        }

        auto result = graph_array.size();
        graph_array.push_back(std::move(value));
        return value_getter::store_result(result);
    }

    void serializer::visit(function_start & function_start) {
        return store_result({{"arg count", function_start.arg_count},
                             {"exported", function_start.exported},
                             {"next", get_value(*function_start.next, *this)}},
                            &function_start);
    }

    void serializer::visit(function_end & function_end) {
        return store_result({{"value", (function_end.value != nullptr)
                                           ? nlohmann::json{get_value(*function_end.value, *this)}
                                           : nullptr}},
                            &function_end);
    }

    void serializer::visit(binary_operation & binary_operation) {
        return store_result({{"next", get_value(*binary_operation.next, *this)},
                             {"left", get_value(*binary_operation.lhs, *this)},
                             {"right", get_value(*binary_operation.rhs, *this)},
                             {"op", binary_operation.op}},
                            &binary_operation);
    }

    void serializer::visit(branch & branch) {
        return store_result(
            {
                {"condition", get_value(*branch.condition_value, *this)},
                {"true case", get_value(*branch.true_case, *this)},
                {"false case", get_value(*branch.false_case, *this)},
            },
            &branch);
    }

    void serializer::visit(constant & constant) {
        // TODO: Print the name of the val_type
        return store_result(
            {{"value", std::visit(
                           [](auto arg) -> nlohmann::json {
                               using T = std::decay_t<decltype(arg)>;
                               if constexpr (std::is_same_v<T, std::monostate>) {
                                   return "nullptr";
                               } else if constexpr (std::is_same_v<T, std::string>) {
                                   return arg;
                               } else {
                                   return std::to_string(arg);
                               }
                           },
                           constant.value)},
             {"type", (int)constant.val_type},
             {"next", get_value(*constant.next, *this)}},
            &constant);
    }

    void serializer::visit(function_call & function_call) {
        nlohmann::json::array_t args;
        for (auto * arg : function_call.arguments) { args.push_back(get_value(*arg, *this)); }

        return store_result({{"next", get_value(*function_call.next, *this)},
                             {"arguments", std::move(args)},
                             {"callee", function_call.callee->name}},
                            &function_call);
    }

    void serializer::visit(phi & phi) {
        nlohmann::json::array_t args;
        for (auto * arg : phi.previous) { args.push_back(get_value(*arg, *this)); }

        return store_result(
            {
                {"next", get_value(*phi.next, *this)},
                {"arguments", std::move(args)},
            },
            &phi);
    }

    void serializer::visit(unary_operation & unary_operation) {

        return store_result({{"next", get_value(*unary_operation.next, *this)},
                             {"op", (int)unary_operation.op},
                             {"operand", get_value(*unary_operation.operand, *this)}},
                            &unary_operation);
    }
} // namespace control_flow
