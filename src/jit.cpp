#include "jit.hpp"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>

using namespace llvm;

int run_module(std::vector<std::unique_ptr<Module>> ir_modules) {

    // pass some module to get the jit setup correctly
    auto * executionEngine
        = EngineBuilder{std::move(ir_modules.back())}.setEngineKind(EngineKind::JIT).create();
    ir_modules.pop_back();

    for (auto && ir_module : ir_modules) { executionEngine->addModule(std::move(ir_module)); }
    auto * main = executionEngine->FindFunctionNamed("main");
    // TODO: pass args into the jit
    auto result = executionEngine->runFunction(main, {});

    // return the result
    return result.IntVal.getLimitedValue();
}
