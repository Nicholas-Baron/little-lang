#include "program.hpp"
#include "settings.hpp"
#include "utils/execute.hpp" // exec_command

#include <iostream> // cout

int main(const int arg_count, const char * const * const args) {

    const auto command_line = read_settings(arg_count, args);

    const auto & filename = command_line->file_to_read;

    auto program = program::from_root_file(filename, command_line);

    if (program == nullptr) { return 1; }

    program->lower_to_control_flow_graph();
    if (not program->type_check()) { return 2; }
    program->generate_ir();

    if (command_line->flag_is_set(cmd_flag::no_output)) { return 0; }

    if (command_line->flag_is_set(cmd_flag::simulate)) {
        std::cout << "Return value: " << program->jit() << std::endl;
        return 0;
    }

    auto program_name = program->emit_and_link();
    if (command_line->flag_is_set(cmd_flag::run_result)) {
        auto args = std::vector{program_name};
        std::copy(command_line->extra_args.begin(), command_line->extra_args.end(),
                  std::back_inserter(args));
        if (not exec_command(std::move(args),
                             command_line->flag_is_set(cmd_flag::debug_show_execs))) {
            std::cerr << "Could not run " << program_name << std::endl;
        }
    }
}
