
#include "ast/parser.hpp"
#include "ast/type_context.hpp"

#include <cstdint> // size_t

extern "C" int LLVMFuzzerTestOneInput(const uint8_t * data, std::size_t size) {

    std::string buffer(reinterpret_cast<const char *>(data), size);

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    assert(parser != nullptr);
    (void)parser->parse();
    return 0;
}
