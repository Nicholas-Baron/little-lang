#ifndef PARSER_HPP
#define PARSER_HPP

#include <ast/nodes_forward.hpp>
#include <utils/move_copy.hpp>

#include <memory>
#include <string>

class parser final {
  public:
    static std::unique_ptr<parser> from_file(const std::string & filename);

    // We do not own the string. The caller must store the string.
    static std::unique_ptr<parser> from_buffer(std::string & buffer);

    [[nodiscard]] std::unique_ptr<ast::top_level_sequence> parse();

    [[nodiscard]] std::string error_message() { return error; }

    non_copyable(parser);
    non_movable(parser);

    ~parser() noexcept;

  private:
    parser(std::string filename, const char * data, size_t);
    parser(const char * data, size_t);

    std::string filename;
    std::string error;

    const char * data;
    size_t length;
    enum data_type { mmapped, read_buffer } type;
};

#endif
