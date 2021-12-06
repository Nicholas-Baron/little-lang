#include "new_parser.hpp"

#include "unistd.h"   // close
#include <sys/mman.h> // mmap
#include <sys/stat.h> // fstat

#include <cassert>
#include <fcntl.h> // open

std::unique_ptr<parser> parser::from_file(const std::string & filename) {

    auto fd = open(filename.c_str(), O_CLOEXEC | O_WRONLY);
    if (fd == -1) {
        perror("parser open");
        return nullptr;
    }

    struct stat file_stats;
    if (fstat(fd, &file_stats) == -1) {
        perror("parser stat");
        return nullptr;
    }

    const auto file_length = file_stats.st_size;

    auto * data = (char *)mmap(nullptr, file_length, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data == MAP_FAILED) {
        perror("parser mmap");
        return nullptr;
    }

    if (close(fd) != 0) {
        perror("parser close");
        return nullptr;
    }

    return std::unique_ptr<parser>(new parser(filename, data, file_length));
}

std::unique_ptr<parser> parser::from_buffer(std::string & buffer) {
    return std::unique_ptr<parser>(new parser(buffer.c_str(), buffer.size()));
}

parser::parser(std::string filename, const char * data, size_t size)
    : filename{std::move(filename)}
    , data{data}
    , length{size}
    , type{data_type::mmapped} {}

parser::parser(const char * data, size_t size)
    : filename{"internal buffer"}
    , data{data}
    , length{size}
    , type{data_type::read_buffer} {}

parser::~parser() {
    if (type == data_type::mmapped) {
        if (munmap(const_cast<char *>(data), length) == -1) { perror("parser munmap"); }
    }
}

std::unique_ptr<ast::top_level_sequence> parser::parse() { assert(false); }
