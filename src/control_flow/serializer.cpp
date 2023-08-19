#include "serializer.hpp"

#include "control_flow/node.hpp"
#include "token_to_string.hpp"

#include <iostream>

#define NODE_NAME(node) &node, #node

namespace control_flow {

    serializer::serializer() = default;

    serializer::~serializer() noexcept = default;

    std::string serializer::node_data::id() const {
        auto stream = std::stringstream{};
        stream << ptr;
        return stream.str();
    }

    void serializer::into_stream(std::ostream & stream, const std::vector<node *> & roots) {
        serializer visitor{};
        for (auto * node : roots) {
            node->accept(visitor);
            visitor.drop_result();
        }

        stream << "digraph {\n";

        for (auto & node : visitor.node_array) {
            stream << '"' << node->id() << "\" [label=" << node->name << "]\n";
            for (auto & [index, connection_type] : node->connections) {
                stream << '"' << node->id() << "\" -> \"" << visitor.node_array[index]->id()
                       << "\" [label=\"" << connection_type << "\"]\n";
            }
        }

        stream << "}\n";
    }

    std::pair<serializer::node_data *, size_t> serializer::add_node(node * node) {
        if (auto iter = visited.find(node); iter != visited.end()) {
            return {nullptr, iter->second};
        }

        auto & result = node_array.emplace_back(std::make_unique<node_data>());
        visited.emplace(node, &result - node_array.data());
        return {result.get(), &result - node_array.data()};
    }

    void serializer::store_result(node_data * value) {
        auto iter = std::find_if(node_array.begin(), node_array.end(),
                                 [&value](auto & item) -> bool { return item.get() == value; });
        return value_getter::store_result(std::distance(node_array.begin(), iter));
    }

    void serializer::visit(function_start & function_start) {
        auto [result, index] = add_node(&function_start);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {NODE_NAME(function_start), {{get_value(*function_start.next, *this), "next"}}};
        return store_result(result);
    }

    void serializer::visit(function_end & function_end) {
        auto [result, index] = add_node(&function_end);
        if (result == nullptr) { return value_getter::store_result(index); }

        decltype(result->connections) connections;
        if (function_end.value != nullptr) {
            auto value = get_value(*function_end.value, *this);
            connections.emplace(value, "value");
        }

        *result = {NODE_NAME(function_end), connections};
        return store_result(result);
    }

    void serializer::visit(cast & cast) {
        auto [result, index] = add_node(&cast);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*cast.next,  *this), "next"   },
            {get_value(*cast.value, *this), "operand"},
        };

        *result = {NODE_NAME(cast), connections};
        return store_result(result);
    }

    void serializer::visit(binary_operation & binary_operation) {
        auto [result, index] = add_node(&binary_operation);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*binary_operation.next, *this), "next" },
            {get_value(*binary_operation.lhs,  *this), "left" },
            {get_value(*binary_operation.rhs,  *this), "right"},
        };

        *result = {NODE_NAME(binary_operation), connections};
        return store_result(result);
    }

    void serializer::visit(branch & branch) {
        auto [result, index] = add_node(&branch);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*branch.condition_value, *this), "condition" },
            {get_value(*branch.true_case,       *this), "true case" },
            {get_value(*branch.false_case,      *this), "false case"},
        };

        *result = {NODE_NAME(branch), connections};
        return store_result(result);
    }

    void serializer::visit(constant & constant) {
        auto [result, index] = add_node(&constant);
        if (result == nullptr) { return value_getter::store_result(index); }

        auto printable_value = std::visit(
            [](auto & arg) -> std::string {
                using contained_type = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<contained_type, std::string>) {
                    return arg;
                } else if constexpr (std::is_same_v<contained_type, std::monostate>) {
                    return "monostate";
                } else {
                    return (std::stringstream{} << arg).str();
                }
            },
            constant.value);

        std::map<size_t, std::string> connections{
            {get_value(*constant.next, *this), "next"},
        };

        *result = {&constant, "constant " + printable_value, connections};
        return store_result(result);
    }

    void serializer::visit(function_call & function_call) {
        auto [result, index] = add_node(&function_call);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*function_call.next,   *this), "next"  },
            {get_value(*function_call.callee, *this), "callee"}
        };

        auto arg_place = 0U;
        for (auto * arg : function_call.arguments) {
            connections.emplace(get_value(*arg, *this), std::to_string(arg_place));
            ++arg_place;
        }

        *result = {NODE_NAME(function_call), connections};
        return store_result(result);
    }

    void serializer::visit(intrinsic_call & intrinsic_call) {
        auto [result, index] = add_node(&intrinsic_call);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*intrinsic_call.next, *this), "next"}
        };

        auto arg_place = 0U;
        for (auto * arg : intrinsic_call.arguments) {
            connections.emplace(get_value(*arg, *this), std::to_string(arg_place));
            ++arg_place;
        }

        *result = {&intrinsic_call, "intrinsic_call " + intrinsic_call.name, connections};
        return store_result(result);
    }

    void serializer::visit(member_access & member_access) {

        auto [result, index] = add_node(&member_access);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*member_access.next, *this), "next"},
            {get_value(*member_access.lhs,  *this), "lhs" }
        };

        *result = {&member_access, "member_access " + member_access.member_name, connections};

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

        std::map<size_t, std::string> connections{
            {get_value(*phi.next, *this), "next"},
        };

        for (auto * arg : phi.previous) { connections.emplace(get_value(*arg, *this), "arg"); }

        *result = {NODE_NAME(phi), connections};

        return store_result(result);
    }

    void serializer::visit(struct_init & struct_init) {
        auto [result, index] = add_node(&struct_init);
        if (result == nullptr) { return value_getter::store_result(index); }

        std::map<size_t, std::string> connections{
            {get_value(*struct_init.next, *this), "next"},
        };

        for (auto [name, value] : struct_init.fields) {
            connections.emplace(get_value(*value, *this), name);
        }

        *result = {NODE_NAME(struct_init), connections};

        return store_result(result);
    }

    void serializer::visit(unary_operation & unary_operation) {

        auto [result, index] = add_node(&unary_operation);
        if (result == nullptr) { return value_getter::store_result(index); }

        *result = {
            NODE_NAME(unary_operation),
            {{get_value(*unary_operation.next, *this), "next"},
                               {get_value(*unary_operation.operand, *this), "operand"}}
        };
        return store_result(result);
    }
} // namespace control_flow
