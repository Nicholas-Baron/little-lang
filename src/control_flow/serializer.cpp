#include "serializer.hpp"

#include "control_flow/node.hpp"

namespace control_flow {

    void serializer::visit(function_start & function_start) {
        return store_result(nlohmann::json::object_t{{"arg count", function_start.arg_count},
                                                     {"exported", function_start.exported}});
    }

} // namespace control_flow
