#include "jit.hpp"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>

int run_module(std::unique_ptr<llvm::Module> ir_module) {

    // execute it!
    auto * executionEngine
        = llvm::EngineBuilder{std::move(ir_module)}.setEngineKind(llvm::EngineKind::JIT).create();
    auto * main = executionEngine->FindFunctionNamed("main");
    auto result = executionEngine->runFunction(main, {});

    // return the result
    return result.IntVal.getLimitedValue();
}
