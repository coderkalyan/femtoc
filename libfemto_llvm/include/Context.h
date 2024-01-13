#pragma once

#include <memory>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

namespace femto {

class Context {
public:
    Context();
    ~Context();

    Context (const Context&) = delete;
    Context& operator= (const Context&) = delete;

private:
    // std::unique_ptr<llvm::LLVMContext> _context;
    llvm::LLVMContext *_context;
    std::unique_ptr<llvm::IRBuilder<>> _builder;
    // std::unique_ptr<llvm::Module> _module;
    llvm::Module *_module;
};

};
