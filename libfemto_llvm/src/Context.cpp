// #include <iostream>
#include "Context.h"

namespace femto {

Context::Context() {
    // _context = std::make_unique<llvm::LLVMContext>();
    _context = new llvm::LLVMContext();
    _builder = std::unique_ptr<llvm::IRBuilder<>>(new llvm::IRBuilder<>(*_context));
    // _module = std::make_unique<llvm::Module>("femtoc_main", *_context);
    _module = new llvm::Module("femtoc_main", *_context);
    // _module = std::unique_ptr<llvm::Module>(new llvm::Module("Module", *_context));
}

Context::~Context() {
    // delete _module;
    delete _context;
}

};
