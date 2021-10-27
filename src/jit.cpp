#include "jit.hpp"

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>

int run_module(context_module && context) {

    // execute it!
    auto * executionEngine = llvm::EngineBuilder{std::move(context).take_module()}
                                 .setEngineKind(llvm::EngineKind::JIT)
                                 .create();
    auto * main = executionEngine->FindFunctionNamed("main");
    auto result = executionEngine->runFunction(main, {});

    // return the result
    return result.IntVal.getLimitedValue();
}
