#include "emit_asm.hpp"

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace llvm;

std::string init_llvm_targets() {
    // TODO: Support all targets
    // InitializeAllTargetInfos();
    // InitializeAllTargets();
    // InitializeAllTargetMCs();
    // InitializeAllAsmParsers();
    // InitializeAllAsmPrinters();

    InitializeNativeTarget();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetAsmPrinter();

    return sys::getDefaultTargetTriple();
}

bool optimize_module(llvm::Module & ir_module, bool debug_optimized_ir) {

    std::string error;
    const auto * target = llvm::TargetRegistry::lookupTarget(ir_module.getTargetTriple(), error);
    if (target == nullptr) {
        errs() << error << '\n';
        errs().flush();
        return false;
    }

    llvm::TargetOptions opt;
    opt.FunctionSections = true;
    opt.DataSections = true;
    const auto * cpu = "generic";

    auto * target_machine
        = target->createTargetMachine(ir_module.getTargetTriple(), cpu, "", opt, llvm::Reloc::PIC_);
    assert(target->hasTargetMachine());

    ir_module.setDataLayout(target_machine->createDataLayout());

    // new pass manager
    PassBuilder pass_builder{target_machine};

    LoopAnalysisManager lam;
    FunctionAnalysisManager fam;
    CGSCCAnalysisManager cgam;
    ModuleAnalysisManager mam;

    fam.registerPass([&] { return pass_builder.buildDefaultAAPipeline(); });

    pass_builder.registerModuleAnalyses(mam);
    pass_builder.registerCGSCCAnalyses(cgam);
    pass_builder.registerFunctionAnalyses(fam);
    pass_builder.registerLoopAnalyses(lam);
    pass_builder.crossRegisterProxies(lam, fam, cgam, mam);

    auto mpm = pass_builder.buildPerModuleDefaultPipeline(OptimizationLevel::O2);

    mpm.run(ir_module, mam);

    if (debug_optimized_ir) { llvm::outs() << ir_module << '\n'; }
    return true;
}

void emit_asm(std::unique_ptr<llvm::Module> ir_module, std::string && output_filename) {

    std::string error;
    const auto * target = llvm::TargetRegistry::lookupTarget(ir_module->getTargetTriple(), error);
    if (target == nullptr) {
        errs() << error << '\n';
        errs().flush();
        return;
    }

    llvm::TargetOptions opt;
    opt.FunctionSections = true;
    opt.DataSections = true;
    const auto * cpu = "generic";

    std::error_code error_code;

    if (auto slash_pos = output_filename.find_last_of('/'); slash_pos != std::string::npos) {
        if (error_code = llvm::sys::fs::create_directories(output_filename.substr(0, slash_pos));
            error_code) {
            errs() << "Could not create directories for " << output_filename << " : "
                   << error_code.message() << '\n';
            errs().flush();
            return;
        }
    }

    llvm::raw_fd_ostream dest{output_filename, error_code, llvm::sys::fs::OF_None};

    if (error_code) {
        errs() << "Could not open file " << output_filename << " : " << error_code.message()
               << '\n';
        errs().flush();
        return;
    }

    auto * target_machine = target->createTargetMachine(ir_module->getTargetTriple(), cpu, "", opt,
                                                        llvm::Reloc::PIC_);
    assert(target->hasTargetMachine());

    {
        legacy::PassManager pass_manager;
        auto filetype = CGFT_ObjectFile;
        if (target_machine->addPassesToEmitFile(pass_manager, dest, nullptr, filetype)) {
            errs() << "target machine does not emit a file of this type.\n";
            errs().flush();
            return;
        }

        pass_manager.run(*ir_module);
    }

    dest.flush();
}
