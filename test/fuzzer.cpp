
#include "ast/parser.hpp"
#include "ast/type_context.hpp"

#include <cstdint> // size_t

extern "C" int LLVMFuzzerTestOneInput(const uint8_t * data, std::size_t size) {

    static_assert(sizeof(uint8_t) == sizeof(char));
    ast::type_context ty_context;
    auto parser = parser::from_buffer(reinterpret_cast<const char *>(data), size, ty_context);

    assert(parser != nullptr);
    (void)parser->parse();
    return 0;
}
