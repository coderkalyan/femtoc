#include <memory>
#include <variant>
#include <type_traits>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

namespace femto {

class Context {
public:
    Context() {
        _context = std::make_unique<llvm::LLVMContext>();
        _module = std::make_unique<llvm::Module>("femto_main", *_context);
    }

private:
    std::unique_ptr<llvm::LLVMContext> _context;
    std::unique_ptr<llvm::Module> _module;
};

};

struct Context;

Context *context_create(void) {
    femto::Context *context = new femto::Context();
    Context *handle = reinterpret_cast<struct Context *>(context);
    return handle;
}

void context_destroy(Context *handle) {
    femto::Context *context = reinterpret_cast<femto::Context *>(handle);
    delete context;
}

int main(void) {
    Context *context = context_create();
    context_destroy(context);

    return 0;
}
