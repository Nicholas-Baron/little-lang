#include "emit_asm.hpp"

#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Host.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace llvm;

std::string init_llvm_targets() {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    return sys::getDefaultTargetTriple();
}

void emit_asm(std::unique_ptr<llvm::Module> ir_module, std::string && output_filename,
              bool debug_optimized_ir) {

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

    auto * target_machine = target->createTargetMachine(ir_module->getTargetTriple(), cpu, "", opt,
                                                        llvm::Reloc::PIC_);
    assert(target->hasTargetMachine());

    ir_module->setDataLayout(target_machine->createDataLayout());

    std::error_code ec;

    if (auto slash_pos = output_filename.find_last_of('/'); slash_pos != std::string::npos) {
        if (ec = llvm::sys::fs::create_directories(output_filename.substr(0, slash_pos)); ec) {
            errs() << "Could not create directories for " << output_filename << " : "
                   << ec.message() << '\n';
            errs().flush();
            return;
        }
    }

    llvm::raw_fd_ostream dest{output_filename, ec, llvm::sys::fs::OF_None};

    if (ec) {
        errs() << "Could not open file " << output_filename << " : " << ec.message() << '\n';
        errs().flush();
        return;
    }

    {
        // new pass manager
        PassBuilder pb{target_machine};

        LoopAnalysisManager lam;
        FunctionAnalysisManager fam;
        CGSCCAnalysisManager cgam;
        ModuleAnalysisManager mam;

        fam.registerPass([&] { return pb.buildDefaultAAPipeline(); });

        pb.registerModuleAnalyses(mam);
        pb.registerCGSCCAnalyses(cgam);
        pb.registerFunctionAnalyses(fam);
        pb.registerLoopAnalyses(lam);
        pb.crossRegisterProxies(lam, fam, cgam, mam);

        auto mpm = pb.buildPerModuleDefaultPipeline(PassBuilder::OptimizationLevel::O2);

        mpm.run(*ir_module, mam);
    }

    if (debug_optimized_ir) { llvm::outs() << *ir_module << '\n'; }

    {
        legacy::PassManager pm;
        auto filetype = CGFT_ObjectFile;
        if (target_machine->addPassesToEmitFile(pm, dest, nullptr, filetype)) {
            errs() << "target machine does not emit a file of this type.\n";
            errs().flush();
            return;
        }

        pm.run(*ir_module);
    }

    dest.flush();
}
