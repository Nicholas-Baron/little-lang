#include "serializer.hpp"

#include "control_flow/node.hpp"

namespace control_flow {

    void serializer::into_stream(std::ostream & stream, const std::vector<node *> & roots,
                                 bool human_readable) {
        serializer visitor{};
        nlohmann::json result;
        for (auto * node : roots) { result.push_back(serializer::get_value(*node, visitor)); }
        stream << result.dump(human_readable ? 4 : -1);
    }

    void serializer::visit(function_start & function_start) {
        return store_result(
            nlohmann::json::object_t{{"arg count", function_start.arg_count},
                                     {"exported", function_start.exported},
                                     {"next", get_value(*function_start.next, *this)}});
    }

    void serializer::visit(function_end & function_end) {
        return store_result(nlohmann::json::object_t{
            {"value",
             (function_end.value != nullptr) ? get_value(*function_end.value, *this) : nullptr}});
    }

    void serializer::visit(binary_operation & binary_operation) {
        return store_result(
            nlohmann::json::object_t{{"next", get_value(*binary_operation.next, *this)},
                                     {"left", get_value(*binary_operation.lhs, *this)},
                                     {"right", get_value(*binary_operation.rhs, *this)},
                                     {"op", binary_operation.op}});
    }

} // namespace control_flow
