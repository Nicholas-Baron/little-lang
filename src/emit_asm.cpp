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

void emit_asm(context_module && context, std::string && target_triple) {

    std::string error;
    const auto * target = llvm::TargetRegistry::lookupTarget(target_triple, error);
    if (target == nullptr) {
        errs() << error << '\n';
        errs().flush();
        return;
    }

    llvm::TargetOptions opt;
    const auto * cpu = "generic";

    auto * target_machine
        = target->createTargetMachine(target_triple, cpu, "", opt, llvm::Reloc::PIC_);
    assert(target->hasTargetMachine());

    context.module().setDataLayout(target_machine->createDataLayout());

    const auto * output_filename = "main.o";
    std::error_code ec;

    llvm::raw_fd_ostream dest{output_filename, ec, llvm::sys::fs::OF_None};

    if (ec) {
        errs() << "Could not open file " << output_filename << " : " << ec.message() << '\n';
        errs().flush();
        return;
    }

    {
        // new pass manager
        PassBuilder pb{false, target_machine};

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

        mpm.run(context.module(), mam);
    }

    {
        legacy::PassManager pm;
        auto filetype = CGFT_ObjectFile;
        if (target_machine->addPassesToEmitFile(pm, dest, nullptr, filetype)) {
            errs() << "target machine does not emit a file of this type.\n";
            errs().flush();
            return;
        }

        pm.run(context.module());
    }

    dest.flush();
}
