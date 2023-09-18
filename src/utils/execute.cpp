#include "execute.hpp"

#include <cstring>  // strncpy
#include <iostream> // cout

#include <sys/wait.h> // waitpid
#include <unistd.h>   // execve

bool exec_command(std::vector<std::string> && cmd, bool debug) {

    if (debug) {
        std::cout << "[CMD] ";
        for (const auto & arg : cmd) { std::cout << arg << ' '; }
        std::cout << std::endl;
    }

    if (auto pid = fork(); pid == 0) {
        // in child

        std::vector<char *> args;
        args.reserve(cmd.size() + 1);
        for (auto & arg : cmd) { args.emplace_back(arg.data()); }

        args.push_back(nullptr);

        if (execvp(args[0], args.data()) == -1) {
            perror("execvp");
            exit(-1);
        } else {
            exit(0);
        }
    } else if (pid == -1) {
        perror("fork");
        return false;
    } else {

        int wait_status = 0;
        if (waitpid(pid, &wait_status, 0) != pid) {
            perror("waitpid");
            return false;
        }

        if (not WIFEXITED(wait_status)) {
            std::cerr << "Command " << cmd[0] << " exited abnormally" << std::endl;
            return false;
        }

        if (auto exit_status = WEXITSTATUS(wait_status); exit_status != 0) {
            std::cerr << "Command " << cmd[0] << " exited with status " << exit_status << std::endl;
        }

        return true;
    }
}
